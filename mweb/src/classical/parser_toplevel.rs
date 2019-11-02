use thiserror::Error;

use super::lexer::control_code::{ControlCode, ControlCodeKind};
use super::lexer::token::{BoxedTokenList, Token, TokenList};
use super::lexer::{LexError, WEBLexer};
use std::convert::{TryFrom, TryInto};

#[derive(Debug)]
pub struct WEB<'x> {
    pub(crate) text_in_limbo: LimboText<'x>,
    pub(crate) text_in_modules: Vec<ModuleText<'x>>,
}

#[derive(Default, Debug)]
pub struct LimboText<'x> {
    pub(crate) tokens: TokenList<'x>,
}

use super::parser_codemodule::{parse_codemodules, CodeModuleList, ParseCodeModuleError};
use super::parser_definition::{parse_definition, DefinitionList, ParseDefinitionError};

#[derive(Debug)]
pub struct ModuleText<'x> {
    pub(crate) module_type: ModuleType<'x>,
    pub(crate) text_in_tex: TokenList<'x>,
    pub(crate) definitions: DefinitionList<'x>,
    pub(crate) code_in_pascal: CodeModuleList<'x>,
}

#[derive(Debug)]
pub enum ModuleType<'x> {
    Normal,
    Starred { group_title: BoxedTokenList<'x> },
}

#[derive(Error, Debug)]
pub enum WEBParseError {
    #[error("lex error: {0}")]
    LexError(#[from] LexError),
    #[error("parse definition error: {0}")]
    ParseDefinitionError(#[from] ParseDefinitionError),
    #[error("parse code module error: {0}")]
    ParseCodeModuleError(#[from] ParseCodeModuleError),
    #[error("internal error")]
    InternalError,
    #[error("not yet implemented")]
    NotYetImplemented,
}

impl<'x> WEB<'x> {
    pub fn parse(data: &'x [u8]) -> Result<Self, WEBParseError> {
        let mut lexer = WEBLexer::new(data);

        let mut text_in_limbo = LimboText::new();
        if let Some(limbo_data) = lexer.lex_limbo()? {
            text_in_limbo.tokens = limbo_data.limbo_tokens;
        }

        let mut text_in_modules = vec![];
        while let Some(module_data) = lexer.lex_module()? {
            let mut module_text = ModuleText::new(module_data.module_type.try_into()?);
            //println!("{:?}", module_text.module_type);
            module_text.text_in_tex = module_data.text_in_tex;
            let mut definitions = vec![];
            for definition in module_data.definitions {
                if let Some(definition) = parse_definition(definition)? {
                    definitions.push(definition);
                }
            }
            module_text.definitions = definitions;
            //println!("{:?}", module_data.code_in_pascal);
            module_text.code_in_pascal = parse_codemodules(module_data.code_in_pascal)?;
            text_in_modules.push(module_text);
        }
        println!(
            "Parse complete, limbo and {} module(s).",
            text_in_modules.len()
        );
        Ok(WEB {
            text_in_limbo,
            text_in_modules,
        })
    }
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

impl<'x> LimboText<'x> {
    pub fn new() -> Self {
        Default::default()
    }
}

impl<'x> ModuleText<'x> {
    pub fn new(module_type: ModuleType<'x>) -> Self {
        ModuleText {
            module_type,
            text_in_tex: vec![],
            definitions: vec![],
            code_in_pascal: vec![],
        }
    }
}
