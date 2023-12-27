use anyhow::Result;
use helix_view::{graphics::Rect, input::Event, theme::Modifier};
use tui::buffer::Buffer as Surface;

use crate::{
    compositor::{Context, EventResult},
    key,
};

pub trait TreeViewItem: Sized + Ord {
    type Params: Default;

    fn name(&self) -> String;
    fn is_parent(&self) -> bool;

    fn filter(&self, s: &str) -> bool {
        self.name().to_lowercase().contains(&s.to_lowercase())
    }

    fn get_children(&self) -> Result<Vec<Self>>;
}

fn vec_to_tree<T: TreeViewItem>(mut items: Vec<T>) -> Vec<Tree<T>> {
    items.sort();
    index_elems(
        0,
        items
            .into_iter()
            .map(|item| Tree::new(item, vec![]))
            .collect(),
    )
}

pub enum TreeOp {
    Noop,
    GetChildsAndInsert,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Tree<T> {
    item: T,
    parent_index: Option<usize>,
    index: usize,
    children: Vec<Self>,
    is_opened: bool,
}

impl<T> Tree<T> {
    pub fn new(item: T, children: Vec<Tree<T>>) -> Self {
        let is_opened = !children.is_empty();
        Self {
            item,
            index: 0,
            parent_index: None,
            children: index_elems(0, children),
            is_opened,
        }
    }

    fn regenerate_index(&mut self) {
        let items = std::mem::take(&mut self.children);
        self.children = index_elems(0, items);
    }

    fn len(&self) -> usize {
        1_usize.saturating_add(self.children.iter().map(|elem| elem.len()).sum())
    }

    pub fn item(&self) -> &T {
        &self.item
    }

    fn get(&self, index: usize) -> Option<&Tree<T>> {
        if self.index == index {
            Some(self)
        } else {
            self.children.iter().find_map(|elem| elem.get(index))
        }
    }

    #[allow(dead_code)]
    fn get_mut(&mut self, index: usize) -> Option<&mut Tree<T>> {
        if self.index == index {
            Some(self)
        } else {
            self.children
                .iter_mut()
                .find_map(|elem| elem.get_mut(index))
        }
    }
}

impl<T: TreeViewItem> Tree<T> {
    fn open(&mut self) -> Result<()> {
        if self.item.is_parent() {
            self.children = self.get_children()?;
            self.is_opened = true;
        }
        Ok(())
    }

    fn close(&mut self) {
        self.is_opened = false;
        self.children = vec![];
    }

    fn refresh(&mut self) -> Result<()> {
        if !self.is_opened {
            return Ok(());
        }
        let latest_children = self.get_children()?;
        let filtered = std::mem::take(&mut self.children)
            .into_iter()
            .filter(|tree| {
                latest_children
                    .iter()
                    .any(|child| tree.item.name().eq(&child.item.name()))
            })
            .map(|mut tree| {
                tree.refresh()?;
                Ok(tree)
            })
            .collect::<Result<Vec<_>>>()?;

        let new_nodes = latest_children
            .into_iter()
            .filter(|child| {
                !filtered
                    .iter()
                    .any(|child_| child.item.name().eq(&child_.item.name()))
            })
            .collect::<Vec<_>>();

        self.children = filtered.into_iter().chain(new_nodes).collect();

        self.sort();

        self.regenerate_index();

        Ok(())
    }

    fn sort(&mut self) {
        self.children.sort_by(|a, b| a.item.cmp(&b.item))
    }

    fn get_children(&self) -> Result<Vec<Tree<T>>> {
        Ok(vec_to_tree(self.item.get_children()?))
    }
}

pub struct TreeView<T: TreeViewItem> {
    tree: Tree<T>,

    /// Selected item index
    selected: usize,
    /// For implementing vertical scroll
    winline: usize,

    /// For implementing horizontal scroll
    column: usize,
    /// For implementing horizontal scroll
    max_len: usize,

    #[allow(clippy::type_complexity)]
    on_opened_fn: Option<Box<dyn FnMut(&mut T, &mut Context, &mut T::Params) -> TreeOp + 'static>>,
}

impl<T: TreeViewItem> TreeView<T> {
    pub fn build_tree(root: T) -> Result<Self> {
        let children = root.get_children()?;
        let items = vec_to_tree(children);
        Ok(Self {
            tree: Tree::new(root, items),
            selected: 0,
            winline: 0,
            column: 0,
            max_len: 0,
            on_opened_fn: None,
        })
    }

    pub fn with_enter_fn<F>(mut self, f: F) -> Self
    where
        F: FnMut(&mut T, &mut Context, &mut T::Params) -> TreeOp + 'static,
    {
        self.on_opened_fn = Some(Box::new(f));
        self
    }

    pub fn reveal_item(&mut self, segments: Vec<String>) -> Result<()> {
        let root = self.tree.item.name();
        segments.iter().fold(
            Ok(&mut self.tree),
            |current_tree, segment| match current_tree {
                Err(err) => Err(err),
                Ok(current_tree) => {
                    match current_tree.children.iter_mut().find(|tree| tree.item.name().eq(segment)) {
                        Some(tree) => {
                            if !tree.is_opened {
                                tree.open()?;
                            }
                            Ok(tree)
                        }
                        None => Err(anyhow::anyhow!(format!("Unable to fild path: '{}'. current_segment = '{segment}'. current_root = '{root}'",segments.join("/"),
                        ))),
                    }
                }
            }
        )?;

        self.regenerate_index();
        self.set_selected(
            segments
                .iter()
                .fold(&self.tree, |tree, segment| {
                    tree.children
                        .iter()
                        .find(|tree| tree.item.name().eq(segment))
                        .expect("Should be unreachable")
                })
                .index,
        );

        // TODO:
        // self.align_view_center();
        Ok(())
    }

    fn set_selected(&mut self, selected: usize) {
        let previous_selected = self.selected;
        self.set_selected_without_history(selected);
        if previous_selected.abs_diff(selected) > 1 {
            // self.backward_jump_push(previous_selected)
        }
    }

    fn set_selected_without_history(&mut self, selected: usize) {
        let selected = selected.clamp(0, self.tree.len().saturating_sub(1));
        if selected > self.selected {
            // Move down
            self.winline = selected.min(
                self.winline
                    .saturating_add(selected.saturating_sub(self.selected)),
            );
        } else {
            // Move up
            self.winline = selected.min(
                self.winline
                    .saturating_sub(selected.saturating_sub(selected)),
            );
        }
        self.selected = selected;
    }

    fn regenerate_index(&mut self) {
        self.tree.regenerate_index();
    }

    fn get(&self, index: usize) -> Result<&Tree<T>> {
        self.tree.get(index).ok_or_else(|| {
            anyhow::anyhow!("TreeView.get: index {index} is out of bound(This is bug)")
        })
    }

    fn get_mut(&mut self, index: usize) -> Result<&mut Tree<T>> {
        self.tree.get_mut(index).ok_or_else(|| {
            anyhow::anyhow!("TreeView.get_mut: index {index} is out of bound(This is bug)")
        })
    }

    pub fn current(&self) -> Result<&Tree<T>> {
        self.get(self.selected)
    }

    fn current_mut(&mut self) -> Result<&mut Tree<T>> {
        self.get_mut(self.selected)
    }

    pub fn on_enter(
        &mut self,
        cx: &mut Context,
        params: &mut T::Params,
        selected_index: usize,
    ) -> Result<()> {
        let selected_item = self.get_mut(selected_index)?;
        if selected_item.is_opened {
            selected_item.close();
            self.regenerate_index();
            return Ok(());
        }

        if let Some(mut on_open_fn) = self.on_opened_fn.take() {
            let mut f = || -> Result<()> {
                let current = self.current_mut()?;
                match on_open_fn(&mut current.item, cx, params) {
                    TreeOp::GetChildsAndInsert => {
                        if let Err(err) = current.open() {
                            cx.editor.set_error(format!("{err}"))
                        }
                    }
                    TreeOp::Noop => {}
                };
                Ok(())
            };
            f()?;
            self.regenerate_index();
            self.on_opened_fn = Some(on_open_fn);
        };
        Ok(())
    }

    pub fn refresh(&mut self) -> Result<()> {
        self.tree.refresh()?;
        self.set_selected(self.selected);
        Ok(())
    }
}

impl<T: TreeViewItem + Clone> TreeView<T> {
    pub fn render(
        &mut self,
        area: Rect, // list_area
        _prompt_area: Rect,
        surface: &mut Surface,
        cx: &mut Context,
    ) {
        let style = cx.editor.theme.get("ui.text");
        // TODO: handle search prompt

        let ancestor_style = {
            let style = cx.editor.theme.get("ui.selection");
            let fg = cx.editor.theme.get("ui.text").fg;
            match (style.fg, fg) {
                (None, Some(fg)) => style.fg(fg),
                _ => style,
            }
        };

        let iter = self.render_lines(area).into_iter().enumerate();

        for (index, line) in iter {
            let area = Rect::new(area.x, area.y.saturating_add(index as u16), area.width, 1);
            let indent_len = line.indent.chars().count() as u16;
            surface.set_stringn(
                area.x,
                area.y,
                line.indent.as_str(),
                indent_len as usize,
                style,
            );

            let style = if line.selected {
                style.add_modifier(Modifier::REVERSED)
            } else {
                style
            };
            let x = area.x.saturating_add(indent_len);

            surface.set_stringn(
                x,
                area.y,
                line.content.as_str(),
                area.width
                    .saturating_sub(indent_len)
                    .saturating_sub(1)
                    .into(),
                if line.is_ancestor_of_current_item {
                    ancestor_style
                } else {
                    style
                },
            );
        }
    }

    fn render_lines(&mut self, area: Rect) -> Vec<RenderedLine> {
        // TODO handle pre_render

        self.winline = self.winline.min(area.height.saturating_sub(1) as usize);
        let skip = self.selected.saturating_sub(self.winline);
        let params = RenderTreeParams {
            tree: &self.tree,
            prefix: &"".to_string(),
            level: 0,
            selected: self.selected,
        };

        let lines = render_tree(params);

        self.max_len = lines
            .iter()
            .map(|line| {
                line.indent
                    .chars()
                    .count()
                    .saturating_add(line.content.chars().count())
            })
            .max()
            .unwrap_or(0);

        let max_width = area.width as usize;

        let take = area.height as usize;

        struct RetainAncestorResult {
            skipped_ancestors: Vec<RenderedLine>,
            remaining_lines: Vec<RenderedLine>,
        }

        fn retain_ancestors(lines: Vec<RenderedLine>, skip: usize) -> RetainAncestorResult {
            if skip == 0 {
                return RetainAncestorResult {
                    skipped_ancestors: vec![],
                    remaining_lines: lines,
                };
            }
            if let Some(line) = lines.get(0) {
                if line.selected {
                    return RetainAncestorResult {
                        skipped_ancestors: vec![],
                        remaining_lines: lines,
                    };
                }
            }

            let selected_index = lines.iter().position(|line| line.selected);
            let skip = match selected_index {
                None => skip,
                Some(selected_index) => skip.min(selected_index),
            };
            let (skipped, remaining) = lines.split_at(skip.min(lines.len().saturating_sub(1)));

            let skipped_ancestors = skipped
                .iter()
                .cloned()
                .filter(|line| line.is_ancestor_of_current_item)
                .collect::<Vec<_>>();

            let result = retain_ancestors(remaining.to_vec(), skipped_ancestors.len());
            RetainAncestorResult {
                skipped_ancestors: skipped_ancestors
                    .into_iter()
                    .chain(result.skipped_ancestors.into_iter())
                    .collect(),
                remaining_lines: result.remaining_lines,
            }
        }

        let RetainAncestorResult {
            skipped_ancestors,
            remaining_lines,
        } = retain_ancestors(lines, skip);

        let max_ancestors_len = take.saturating_sub(1);

        // Skip furthest ancestors
        let skipped_ancestors = skipped_ancestors
            .into_iter()
            .rev()
            .take(max_ancestors_len)
            .rev()
            .collect::<Vec<_>>();

        let skipped_ancestors_len = skipped_ancestors.len();

        skipped_ancestors
            .into_iter()
            .chain(
                remaining_lines
                    .into_iter()
                    .take(take.saturating_sub(skipped_ancestors_len)),
            )
            // Horizontal scroll
            .map(|line| {
                let skip = self.column;
                let indent_len = line.indent.chars().count();
                RenderedLine {
                    indent: if line.indent.is_empty() {
                        "".to_string()
                    } else {
                        line.indent
                            .chars()
                            .skip(skip)
                            .take(max_width)
                            .collect::<String>()
                    },
                    content: line
                        .content
                        .chars()
                        .skip(skip.saturating_sub(indent_len))
                        .take((max_width.saturating_sub(indent_len)).clamp(0, line.content.len()))
                        .collect::<String>(),
                    ..line
                }
            })
            .collect()
    }

    #[cfg(test)]
    pub fn render_to_string(&mut self, area: Rect) -> String {
        let lines = self.render_lines(area);
        lines
            .into_iter()
            .map(|line| {
                let name = if line.selected {
                    format!("({})", line.content)
                } else if line.is_ancestor_of_current_item {
                    format!("[{}]", line.content)
                } else {
                    line.content
                };
                format!("{}{}", line.indent, name)
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn move_up(&mut self, rows: usize) {
        self.set_selected(self.selected.saturating_sub(rows))
    }

    fn move_down(&mut self, rows: usize) {
        self.set_selected(self.selected.saturating_add(rows))
    }

    pub fn handle_event(
        &mut self,
        event: &Event,
        cx: &mut Context,
        params: &mut T::Params,
    ) -> EventResult {
        let key_event = match event {
            Event::Key(event) => event,
            // should handle resize ?
            _ => return EventResult::Ignored(None),
        };
        (|| -> Result<EventResult> {
            // TODO on_next_key, search, count

            match key_event {
                key!('j') => self.move_down(1),
                key!('k') => self.move_up(1),
                key!(Enter) => self.on_enter(cx, params, self.selected)?,

                _ => return Ok(EventResult::Ignored(None)),
            };
            Ok(EventResult::Consumed(None))
        })()
        .unwrap_or_else(|err| {
            cx.editor.set_error(format!("{err}"));
            EventResult::Consumed(None)
        })
    }
}

#[derive(Clone, Debug)]
struct RenderedLine {
    indent: String,
    content: String,
    selected: bool,
    is_ancestor_of_current_item: bool,
}

struct RenderTreeParams<'a, T> {
    tree: &'a Tree<T>,
    prefix: &'a String,
    level: usize,
    selected: usize,
}

fn render_tree<T: TreeViewItem>(
    RenderTreeParams {
        tree,
        prefix,
        level,
        selected,
    }: RenderTreeParams<T>,
) -> Vec<RenderedLine> {
    let indent = if level > 0 {
        let indicator = if tree.item().is_parent() {
            if tree.is_opened {
                "⏷"
            } else {
                "⏵"
            }
        } else {
            " "
        };
        format!("{}{} ", prefix, indicator)
    } else {
        "".to_string()
    };
    let name = tree.item.name();
    let head = RenderedLine {
        indent,
        selected: selected == tree.index,
        is_ancestor_of_current_item: selected != tree.index && tree.get(selected).is_some(),
        content: name,
    };
    let prefix = format!("{}{}", prefix, if level == 0 { "" } else { "  " });
    vec![head]
        .into_iter()
        .chain(tree.children.iter().flat_map(|elem| {
            render_tree(RenderTreeParams {
                tree: elem,
                prefix: &prefix,
                level: level + 1,
                selected,
            })
        }))
        .collect()
}

fn index_elems<T>(parent_index: usize, elems: Vec<Tree<T>>) -> Vec<Tree<T>> {
    fn index_elems<T>(
        current_index: usize,
        elems: Vec<Tree<T>>,
        parent_index: usize,
    ) -> (usize, Vec<Tree<T>>) {
        elems
            .into_iter()
            .fold((current_index, vec![]), |(current_index, trees), elem| {
                let index = current_index;
                let item = elem.item;
                let (current_index, folded) = index_elems(current_index + 1, elem.children, index);
                let tree = Tree {
                    item,
                    children: folded,
                    index,
                    is_opened: elem.is_opened,
                    parent_index: Some(parent_index),
                };
                (
                    current_index,
                    trees.into_iter().chain(vec![tree].into_iter()).collect(),
                )
            })
    }
    index_elems(parent_index + 1, elems, parent_index).1
}

#[cfg(test)]
mod test_tree {
    use super::Tree;

    #[test]
    fn test_get() {
        let result = Tree::new(
            "root",
            vec![
                Tree::new("foo", vec![Tree::new("bar", vec![])]),
                Tree::new(
                    "spam",
                    vec![Tree::new("jar", vec![Tree::new("yo", vec![])])],
                ),
            ],
        );
        assert_eq!(result.get(0).unwrap().item, "root");
        assert_eq!(result.get(1).unwrap().item, "foo");
        assert_eq!(result.get(2).unwrap().item, "bar");
        assert_eq!(result.get(3).unwrap().item, "spam");
        assert_eq!(result.get(4).unwrap().item, "jar");
        assert_eq!(result.get(5).unwrap().item, "yo");
    }
}
