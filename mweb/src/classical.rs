use crate::utils::IteratorAdvanceIf;
use thiserror::Error;

pub struct WEB<'x> {
    text_in_limbo: LimboText<'x>,
    text_in_modules: Vec<ModuleText<'x>>,
}

impl<'x> WEB<'x> {
    pub fn parse(data: &'x [u8]) -> Result<Self, WEBParseError> {
        let mut lines = data
            .split(|&c| c == b'\n')
            .map(|line| {
                if line.last() == Some(&b'\r') {
                    &line[..line.len() - 1]
                } else {
                    line
                }
            })
            .peekable();

        let mut limbo_text = LimboText::new();
        while let Some(limbo_line) = lines.advance_if(|line| !is_module_first_line(line)) {
            limbo_text.append(limbo_line);
        }

        let mut modules = Vec::new();
        while let Some(module_first_line) = lines.advance_if(|line| is_module_first_line(line)) {
            let (module_type, first_line_remaining) = parse_module_header(module_first_line);
            let mut module = ModuleText::new(module_type);
            let mut module_lines = first_line_remaining
                .into_iter()
                .chain(lines.advance_if_iter(|line| !is_module_first_line(line)))
                .peekable();

            for tex_line in module_lines.advance_if_iter(|line| !is_module_definition_line(line)) {
                module.add_text_line(tex_line);
            }

            for definition_line in
                module_lines.advance_if_iter(|line| is_module_definition_line(line))
            {
                module.add_definition_line(definition_line);
            }

            module.add_program_lines(module_lines);

            modules.push(module);
        }

        Ok(WEB {
            text_in_limbo: limbo_text,
            text_in_modules: modules,
        })
    }
}

fn is_module_first_line(l: &[u8]) -> bool {
    l.starts_with(b"@ ") || l.starts_with(b"@*")
}

fn parse_module_header(l: &[u8]) -> (ModuleType<'_>, Option<&[u8]>) {
    if l.starts_with(b"@ ") {
        (ModuleType::Normal, Some(l))
    } else if l.starts_with(b"@*") {
        assert!(l.ends_with(b"."));
        let group_title = &l[(b"@*".len())..(l.len() - b".".len())];
        (ModuleType::Starred { group_title }, None)
    } else {
        unreachable!()
    }
}

fn is_module_definition_line(l: &[u8]) -> bool {
    l.starts_with(b"@d")
}

#[derive(Error, Debug)]
pub enum WEBParseError {
    #[error("not yet implemented")]
    NotYetImplemented,
}

#[derive(Default)]
pub struct LimboText<'x> {
    lines: Vec<&'x [u8]>,
}

impl<'x> LimboText<'x> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn append(&mut self, new_line: &'x [u8]) {
        self.lines.push(new_line)
    }
}

pub enum ModuleType<'x> {
    Normal,
    Starred { group_title: &'x [u8] },
}

pub struct ModuleText<'x> {
    module_type: ModuleType<'x>,
    text_in_tex: Vec<&'x [u8]>,
    macro_in_definitions: Vec<&'x [u8]>,
    code_in_pascal: Vec<&'x [u8]>,
}

impl<'x> ModuleText<'x> {
    pub fn new(module_type: ModuleType<'x>) -> Self {
        ModuleText {
            module_type,
            text_in_tex: vec![],
            macro_in_definitions: vec![],
            code_in_pascal: vec![],
        }
    }

    pub fn add_text_line(&mut self, line: &'x [u8]) {
        self.text_in_tex.push(line);
    }

    pub fn add_definition_line(&mut self, line: &'x [u8]) {
        self.macro_in_definitions.push(line);
    }

    pub fn add_program_lines(&mut self, lines: impl Iterator<Item = &'x [u8]>) {
        self.code_in_pascal.extend(lines);
    }
}
