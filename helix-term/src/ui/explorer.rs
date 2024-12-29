use std::{
    borrow::Cow,
    cmp::Ordering,
    fs::DirEntry,
    path::{Path, PathBuf},
};

use crate::{
    compositor::{Component, Context, EventResult},
    key, ui,
};
use anyhow::{bail, ensure, Result};
use helix_core::Position;
use helix_view::{
    editor::{Action, CloseError},
    graphics::{CursorKind, Rect},
    input::{Event, KeyEvent},
    Editor,
};
use tui::{
    buffer::Buffer as Surface,
    widgets::{Block, Borders, Widget},
};

use super::{
    tree::{TreeOp, TreeViewItem},
    Prompt, TreeView,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
enum FileType {
    File,
    Directory,
    Root,
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct FileInfo {
    file_type: FileType,
    path: PathBuf,
}

impl FileInfo {
    fn root(path: PathBuf) -> Self {
        Self {
            file_type: FileType::Root,
            path,
        }
    }

    fn get_text(&self) -> Cow<'static, str> {
        let text = match self.file_type {
            FileType::Root => self.path.display().to_string(),
            FileType::File | FileType::Directory => self
                .path
                .file_name()
                .map_or("???".into(), |p| p.to_string_lossy().into_owned()),
        };
        #[cfg(test)]
        let text = text.replace(std::path::MAIN_SEPARATOR, "/");

        text.into()
    }
}

impl PartialOrd for FileInfo {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FileInfo {
    fn cmp(&self, other: &Self) -> Ordering {
        use FileType::*;
        match (self.file_type, other.file_type) {
            (Root, _) => return Ordering::Less,
            (_, Root) => return Ordering::Greater,
            _ => {}
        };

        if let (Some(p1), Some(p2)) = (self.path.parent(), other.path.parent()) {
            if p1 == p2 {
                match (self.file_type, other.file_type) {
                    (Directory, File) => return Ordering::Less,
                    (File, Directory) => return Ordering::Greater,
                    _ => {}
                };
            }
        }
        self.path.cmp(&other.path)
    }
}

impl TreeViewItem for FileInfo {
    // is this nesesarry ?
    type Params = State;

    fn get_children(&self) -> Result<Vec<Self>> {
        match self.file_type {
            FileType::Root | FileType::Directory => {}
            _ => return Ok(vec![]),
        };
        let ret: Vec<_> = std::fs::read_dir(&self.path)?
            .filter_map(|entry| entry.ok())
            .filter_map(|entry| dir_entry_to_file_info(entry, &self.path))
            .collect();
        Ok(ret)
    }

    fn name(&self) -> String {
        self.get_text().to_string()
    }

    fn is_parent(&self) -> bool {
        matches!(self.file_type, FileType::Directory | FileType::Root)
    }
}

fn dir_entry_to_file_info(entry: DirEntry, path: &Path) -> Option<FileInfo> {
    entry.metadata().ok().map(|meta| {
        let file_type = if meta.is_dir() {
            FileType::Directory
        } else {
            FileType::File
        };
        FileInfo {
            file_type,
            path: path.join(entry.file_name()),
        }
    })
}

#[derive(Clone, Debug)]
enum PromptAction {
    CreateFileOrDirectory,
    RemoveDirectory,
    RemoveFile,
    RenameFile,
}

#[derive(Clone, Debug, Default)]
struct State {
    focus: bool,
    /// When open is true, EditorView render Explorer
    open: bool,
    current_root: PathBuf,
    area_width: u16,
}

impl State {
    fn new(focus: bool, current_root: PathBuf) -> Self {
        Self {
            focus,
            current_root,
            open: true,
            area_width: 0,
        }
    }
}

pub struct Explorer {
    tree: TreeView<FileInfo>,
    state: State,
    column_width: u16,
    prompt: Option<(PromptAction, Prompt)>,
}

impl Explorer {
    // ExplorerConfig default value
    const COLUMN_WIDTH: u16 = 36;

    pub fn new(_cx: &mut Context) -> Result<Self> {
        let current_root = std::env::current_dir()
            .unwrap_or_else(|_| "./".into())
            .canonicalize()?;
        Ok(Self {
            tree: Self::new_tree_view(current_root.clone())?,
            state: State::new(true, current_root),
            column_width: Self::COLUMN_WIDTH,
            prompt: None,
        })
    }

    #[cfg(test)]
    fn from_path(root: PathBuf, column_width: u16) -> Result<Self> {
        Ok(Self {
            tree: Self::new_tree_view(root.clone())?,
            state: State::new(true, root),
            column_width,
            prompt: None,
        })
    }

    pub fn reveal_file(&mut self, path: PathBuf) -> Result<()> {
        let current_root = &self.state.current_root.canonicalize()?;
        let current_path = &path.canonicalize()?;

        let segments = {
            let stripped = match current_path.strip_prefix(current_root) {
                Ok(stripped) => Ok::<_, anyhow::Error>(stripped),
                Err(_) => {
                    // TODO: impl change_root to parent case
                    // return Err(anyhow::anyhow!(
                    //     "Failed to strip_prefix {current_root:?} {current_path:?}"
                    // ));
                    Ok::<_, anyhow::Error>(current_path.as_path())
                }
            }?;
            stripped
                .components()
                .map(|c| c.as_os_str().to_string_lossy().to_string())
                .collect::<Vec<_>>()
        };
        self.tree.reveal_item(segments)?;

        Ok(())
    }

    pub fn reveal_current_file(&mut self, cx: &mut Context) -> Result<()> {
        self.focus();
        let current_document_path = doc!(cx.editor).path().cloned();
        match current_document_path {
            None => Ok(()),
            Some(current_path) => self.reveal_file(current_path),
        }
    }

    fn focus(&mut self) {
        self.state.focus = true;
        self.state.open = true;
    }

    fn unfocus(&mut self) {
        self.state.focus = false;
    }

    fn close(&mut self) {
        self.state.focus = false;
        self.state.open = false;
    }

    pub fn is_focus(&self) -> bool {
        self.state.focus
    }

    pub fn is_opened(&self) -> bool {
        self.state.open
    }

    pub fn column_width(&self) -> u16 {
        self.column_width
    }

    fn new_tree_view(root: PathBuf) -> Result<TreeView<FileInfo>> {
        let root = FileInfo::root(root);
        Ok(TreeView::build_tree(root)?.with_enter_fn(Self::toggle_current))
    }

    fn toggle_current(item: &mut FileInfo, cx: &mut Context, state: &mut State) -> TreeOp {
        (|| -> Result<TreeOp> {
            if item.path == Path::new("") {
                return Ok(TreeOp::Noop);
            }
            let meta = std::fs::metadata(&item.path)?;
            if meta.is_file() {
                cx.editor.open(&item.path, Action::Replace)?;
                state.focus = false;
                return Ok(TreeOp::Noop);
            }

            if item.path.is_dir() {
                return Ok(TreeOp::GetChildsAndInsert);
            }

            Err(anyhow::anyhow!("Unknown file type: {:?}", meta.file_type()))
        })()
        .unwrap_or_else(|err| {
            cx.editor.set_error(format!("{err}"));
            TreeOp::Noop
        })
    }

    fn render_tree(
        &mut self,
        area: Rect,
        prompt_area: Rect,
        surface: &mut Surface,
        cx: &mut Context,
    ) {
        self.tree.render(area, prompt_area, surface, cx);
    }

    fn render_embed(&mut self, area: Rect, surface: &mut Surface, cx: &mut Context) {
        if !self.state.open {
            return;
        }
        let width = area.width.min(self.column_width + 2);

        self.state.area_width = area.width;

        // for command prompt
        let side_area = Rect { width, ..area }.clip_bottom(1);
        let background = cx.editor.theme.get("ui.background");
        surface.clear_with(side_area, background);

        let prompt_area = area.clip_top(side_area.height);

        let list_area =
            render_block(side_area.clip_left(0), surface, Borders::RIGHT).clip_bottom(1);

        self.render_tree(list_area, prompt_area, surface, cx);

        // TODO: handle status line
        if let Some((_, prompt)) = self.prompt.as_mut() {
            prompt.render_prompt(prompt_area, surface, cx)
        }
    }

    fn new_remove_directory_prompt(&mut self) -> Result<()> {
        let item = self.tree.current()?.item();
        ensure!(
            item.path.is_dir(),
            "The path '{}' is not a directory",
            item.path.display(),
        );
        self.prompt = Some((
            PromptAction::RemoveDirectory,
            Prompt::new(
                format!(" 🗑️  Delete directory: '{}'? y/N: ", item.path.display()).into(),
                None,
                ui::completers::none,
                |_, _, _| {},
            ),
        ));
        Ok(())
    }

    fn new_remove_file_prompt(&mut self) -> Result<()> {
        let item = self.tree.current()?.item();
        ensure!(
            item.path.is_file(),
            "The path '{}' is not a file",
            item.path.to_string_lossy(),
        );
        self.prompt = Some((
            PromptAction::RemoveFile,
            Prompt::new(
                format!(" 🗑️  Delete file: '{}'? y/N: ", item.path.display()).into(),
                None,
                ui::completers::none,
                |_, _, _| {},
            ),
        ));
        Ok(())
    }

    fn new_create_file_or_directory_prompt(&mut self, cx: &mut Context) -> Result<()> {
        let dir_path = self.nearest_directory()?;
        self.prompt = Some((
            PromptAction::CreateFileOrDirectory,
            Prompt::new(
                format!(
                    "📝 New file or directory (ends with '{}') ",
                    std::path::MAIN_SEPARATOR
                )
                .into(),
                None,
                ui::completers::none,
                |_, _, _| {},
            )
            .with_line(format!("{}/", dir_path.display()), cx.editor),
        ));
        Ok(())
    }

    fn nearest_directory(&self) -> Result<PathBuf> {
        let current = self.tree.current()?.item();
        if current.is_parent() {
            Ok(current.path.to_path_buf())
        } else {
            let parent_path = current.path.parent().ok_or_else(|| {
                anyhow::anyhow!(
                    "Unable to get parent path of '{}'",
                    current.path.to_string_lossy()
                )
            })?;
            Ok(parent_path.to_path_buf())
        }
    }

    fn new_remove_prompt(&mut self) -> Result<()> {
        let item = self.tree.current()?.item();
        match item.file_type {
            FileType::Directory => self.new_remove_directory_prompt(),
            FileType::File => self.new_remove_file_prompt(),
            FileType::Root => bail!("Root is not removable"),
        }
    }

    fn new_rename_prompt(&mut self, cx: &mut Context) -> Result<()> {
        let path = self.tree.current()?.item().path.clone();
        self.prompt = Some((
            PromptAction::RenameFile,
            Prompt::new(
                " Rename to ".into(),
                None,
                ui::completers::none,
                |_, _, _| {},
            )
            .with_line(path.display().to_string(), cx.editor),
        ));
        Ok(())
    }

    fn handle_prompt_event(&mut self, event: &KeyEvent, cx: &mut Context) -> EventResult {
        let result = (|| -> Result<EventResult> {
            let (action, mut prompt) = match self.prompt.take() {
                Some((action, p)) => (action, p),
                _ => return Ok(EventResult::Ignored(None)),
            };
            let line = prompt.line();

            let current_item_path = self.tree.current()?.item().path.clone();
            match (&action, event) {
                (PromptAction::CreateFileOrDirectory, key!(Enter)) => {
                    if line.ends_with(std::path::MAIN_SEPARATOR) {
                        self.new_directory(line)?
                    } else {
                        self.new_file(line)?
                    }
                }
                (PromptAction::RemoveDirectory, key) => {
                    if let key!('y') = key {
                        close_documents(current_item_path, cx)?;
                        self.remove_directory()?;
                    }
                }
                (PromptAction::RemoveFile, key) => {
                    if let key!('y') = key {
                        close_documents(current_item_path, cx)?;
                        self.remove_file()?;
                    }
                }
                (PromptAction::RenameFile, key!(Enter)) => {
                    close_documents(current_item_path, cx)?;
                    self.rename_current(line)?;
                }
                // cancel
                (_, key!(Esc)) => {}
                // continue input
                _ => {
                    prompt.handle_event(&Event::Key(*event), cx);
                    self.prompt = Some((action, prompt));
                }
            }
            Ok(EventResult::Consumed(None))
        })();
        match result {
            Ok(event_result) => event_result,
            Err(err) => {
                cx.editor.set_error(err.to_string());
                EventResult::Consumed(None)
            }
        }
    }

    fn new_directory(&mut self, path: &str) -> Result<()> {
        let path = helix_stdx::path::normalize(&PathBuf::from(path));
        std::fs::create_dir_all(&path)?;
        self.tree.refresh()?;
        self.reveal_file(path)
    }

    fn new_file(&mut self, path: &str) -> Result<()> {
        let path = helix_stdx::path::normalize(&PathBuf::from(path));
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::OpenOptions::new()
            .create_new(true)
            .write(true)
            .open(&path)?;
        self.tree.refresh()?;
        self.reveal_file(path)
    }

    fn rename_current(&mut self, line: &str) -> Result<()> {
        let item = self.tree.current()?.item();
        let path = PathBuf::from(line);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::rename(&item.path, &path)?;
        self.tree.refresh()?;
        self.reveal_file(path)
    }

    fn remove_directory(&mut self) -> Result<()> {
        let item = self.tree.current()?.item();
        std::fs::remove_dir_all(&item.path)?;
        self.tree.refresh()
    }

    fn remove_file(&mut self) -> Result<()> {
        let item = self.tree.current()?.item();
        std::fs::remove_file(&item.path)?;
        self.tree.refresh()
    }
}

fn close_documents(current_item_path: PathBuf, cx: &mut Context) -> Result<()> {
    let ids = cx
        .editor
        .documents
        .iter()
        .filter_map(|(id, doc)| {
            if doc.path()?.starts_with(&current_item_path) {
                Some(*id)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    for id in ids {
        cx.editor.close_document(id, true).map_err(|err| {
            let msg = match err {
                CloseError::DoesNotExist => "does not exits",
                CloseError::BufferModified(_) => "buffer modified",
                CloseError::SaveError(_) => "save error",
            };
            anyhow::anyhow!("Failed to close: {msg}")
        })?;
    }
    Ok(())
}

impl Component for Explorer {
    fn handle_event(&mut self, event: &Event, cx: &mut Context) -> EventResult {
        // TODO: handle tree prompting
        let key_event = match event {
            Event::Key(event) => event,
            // should handle resize to consumed(none) ?
            _ => return EventResult::Ignored(None),
        };
        if !self.is_focus() {
            return EventResult::Ignored(None);
        }
        // TODO: handle on_next_key

        // handle delete | rename prompt
        if let EventResult::Consumed(c) = self.handle_prompt_event(key_event, cx) {
            return EventResult::Consumed(c);
        }

        (|| -> Result<()> {
            match key_event {
                key!(Esc) => self.unfocus(),
                key!('q') => self.close(),
                key!('a') => self.new_create_file_or_directory_prompt(cx)?,
                key!('d') => self.new_remove_prompt()?,
                key!('r') => self.new_rename_prompt(cx)?,
                _ => {
                    self.tree
                        .handle_event(&Event::Key(*key_event), cx, &mut self.state);
                }
            };
            Ok(())
        })()
        .unwrap_or_else(|err| cx.editor.set_error(format!("{err}")));

        EventResult::Consumed(None)
    }

    fn render(&mut self, area: Rect, surface: &mut Surface, cx: &mut Context) {
        if area.width < 10 || area.height < 5 {
            cx.editor.set_error("explorer render area is too small");
            return;
        }
        self.render_embed(area, surface, cx);
    }

    fn cursor(&self, _area: Rect, _editor: &Editor) -> (Option<Position>, CursorKind) {
        // TODO: handle prompt
        (None, CursorKind::Hidden)
    }
}

fn render_block(area: Rect, surface: &mut Surface, borders: Borders) -> Rect {
    let block = Block::default().borders(borders);
    let inner = block.inner(area);
    block.render(area, surface);
    inner
}

#[cfg(test)]
mod test {
    use std::{fs, path::PathBuf};

    use helix_view::graphics::Rect;

    use super::Explorer;

    /// This code should create the following file tree:
    ///
    ///   <temp_path>
    ///   ├── index.html
    ///   ├── .gitignore
    ///   ├── scripts
    ///   │   └── main.js
    ///   └── styles
    ///      ├── style.css
    ///      └── public
    ///          └── file
    ///
    fn dummy_file_tree() -> PathBuf {
        let path = tempfile::tempdir().unwrap().path().to_path_buf();
        if path.exists() {
            fs::remove_dir_all(path.clone()).unwrap();
        }
        fs::create_dir_all(path.clone()).unwrap();
        fs::write(path.join("index.html"), "").unwrap();
        fs::write(path.join(".gitignore"), "").unwrap();

        fs::create_dir_all(path.join("scripts")).unwrap();
        fs::write(path.join("scripts").join("main.js"), "").unwrap();

        fs::create_dir_all(path.join("styles")).unwrap();
        fs::write(path.join("styles").join("style.css"), "").unwrap();

        fs::create_dir_all(path.join("styles").join("public")).unwrap();
        fs::write(path.join("styles").join("public").join("file"), "").unwrap();

        path
    }

    fn render(explorer: &mut Explorer) -> String {
        explorer.tree.render_to_string(Rect::new(0, 0, 100, 10))
    }

    fn new_explorer() -> (PathBuf, Explorer) {
        let path = dummy_file_tree();
        (path.clone(), Explorer::from_path(path, 100).unwrap())
    }

    #[test]
    fn test_reveal_file() {
        let (path, mut explorer) = new_explorer();

        let path_str = path.display().to_string();

        // 0a. Expect the "scripts" folder is not opened
        assert_eq!(
            render(&mut explorer),
            format!(
                "
({path_str})
⏵ scripts
⏵ styles
  .gitignore
  index.html
"
            )
            .trim()
        );
    }
}
