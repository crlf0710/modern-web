use crate::utils::IteratorAdvanceIf;
use thiserror::Error;

use self::lexer::control_code::{ControlCode, ControlCodeKind};
use self::lexer::token::{lex_token, BoxedTokenList, Token, TokenList};
use self::lexer::{LexControlFlow, LexMode};
use std::convert::{TryFrom, TryInto};

#[derive(Debug)]
pub struct WEB<'x> {
    pub(crate) text_in_limbo: LimboText<'x>,
    pub(crate) text_in_modules: Vec<ModuleText<'x>>,
}

impl<'x> WEB<'x> {
    pub fn lex(data: &'x [u8]) -> Result<Self, WEBParseError> {
        let mut text_in_limbo = LimboText::new();
        let mut text_in_modules = vec![];
        let mut mode = LexMode::Limbo;
        let mut data = data;
        let mut pos = 0;
        loop {
            let mut tokens: &mut TokenList = text_in_modules
                .last_mut()
                .map(|module: &mut ModuleText| match mode {
                    LexMode::TeXText => &mut module.text_in_tex,
                    LexMode::DefinitionText => &mut module.macro_in_definitions,
                    LexMode::PascalText => &mut module.code_in_pascal,
                    _ => unreachable!(),
                })
                .unwrap_or_else(|| &mut text_in_limbo.tokens);
            let (token, control_flow) = lex_token(data, mode, pos)?;
            match control_flow {
                LexControlFlow::Continue(rest_data, new_pos) => {
                    println!(" {mode:?}: {token:?}", mode = mode, token = token);
                    tokens.push(token);
                    pos = new_pos;
                    data = rest_data;
                }
                LexControlFlow::Finish => {
                    println!(" {mode:?}: {token:?};", mode = mode, token = token);
                    tokens.push(token);
                    pos += data.len();
                    data = &data[data.len()..];
                    break;
                }
                LexControlFlow::StartNewModule(new_mode, rest_data, new_pos) => {
                    println!(">{mode:?}: {token:?}", mode = new_mode, token = token);
                    mode = new_mode;
                    pos = new_pos;
                    data = rest_data;
                    text_in_modules.push(ModuleText::new(token.try_into().unwrap()));
                }
                LexControlFlow::StartNewDefinition(new_mode, rest_data, new_pos) => {
                    println!(">{mode:?}: {token:?}", mode = new_mode, token = token);
                    text_in_modules
                        .last_mut()
                        .unwrap()
                        .macro_in_definitions
                        .push(token);
                    mode = new_mode;
                    pos = new_pos;
                    data = rest_data;
                }
                LexControlFlow::StartNewProgramText(new_mode, rest_data, new_pos) => {
                    println!(">{mode:?}: {token:?}", mode = new_mode, token = token);
                    text_in_modules
                        .last_mut()
                        .unwrap()
                        .code_in_pascal
                        .push(token);
                    mode = new_mode;
                    pos = new_pos;
                    data = rest_data;
                }
            }
        }

        Ok(WEB {
            text_in_limbo,
            text_in_modules,
        })
    }
    /*
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
    */
}

fn is_module_first_line(l: &[u8]) -> bool {
    l.starts_with(b"@ ") || l.starts_with(b"@*")
}
/*
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
*/
fn is_module_definition_line(l: &[u8]) -> bool {
    l.starts_with(b"@d")
}

#[derive(Error, Debug)]
pub enum WEBParseError {
    #[error("lex error")]
    LexError(#[from] self::lexer::LexError),
    #[error("internal error")]
    InternalError,
    #[error("not yet implemented")]
    NotYetImplemented,
}

#[derive(Default, Debug)]
pub struct LimboText<'x> {
    pub(crate) tokens: TokenList<'x>,
}

impl<'x> LimboText<'x> {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug)]
pub enum ModuleType<'x> {
    Normal,
    Starred { group_title: BoxedTokenList<'x> },
}

impl<'x> TryFrom<Token<'x>> for ModuleType<'x> {
    type Error = WEBParseError;
    fn try_from(v: Token<'x>) -> Result<ModuleType<'x>, WEBParseError> {
        match v {
            Token::CtrlCode(ControlCode { kind, param }) => match kind {
                ControlCodeKind::DefineModule => Ok(ModuleType::Normal),
                ControlCodeKind::DefineStarredModule => Ok(ModuleType::Starred {
                    group_title: param.unwrap_or_default(),
                }),
                _ => Err(WEBParseError::InternalError),
            },
            _ => Err(WEBParseError::InternalError),
        }
    }
}

#[derive(Debug)]
pub struct ModuleText<'x> {
    module_type: ModuleType<'x>,
    text_in_tex: TokenList<'x>,
    macro_in_definitions: TokenList<'x>,
    code_in_pascal: TokenList<'x>,
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
    /*
    pub fn add_text_line(&mut self, line: &'x [u8]) {
        self.text_in_tex.push(line);
    }

    pub fn add_definition_line(&mut self, line: &'x [u8]) {
        self.macro_in_definitions.push(line);
    }

    pub fn add_program_lines(&mut self, lines: impl Iterator<Item = &'x [u8]>) {
        self.code_in_pascal.extend(lines);
    }
    */
}

mod lexer;
mod tangle;
mod weave;
