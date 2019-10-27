use self::lexer::token::{BoxedTokenList, TokenList};

#[derive(Debug)]
pub struct WEB<'x> {
    pub(crate) text_in_limbo: LimboText<'x>,
    pub(crate) text_in_modules: Vec<ModuleText<'x>>,
}

#[derive(Default, Debug)]
pub struct LimboText<'x> {
    pub(crate) tokens: TokenList<'x>,
}

#[derive(Debug)]
pub enum ModuleType<'x> {
    Normal,
    Starred { group_title: BoxedTokenList<'x> },
}

#[derive(Debug)]
pub struct ModuleText<'x> {
    module_type: ModuleType<'x>,
    text_in_tex: TokenList<'x>,
    macro_in_definitions: TokenList<'x>,
    code_in_pascal: TokenList<'x>,
}

mod lexer;
mod parser_toplevel;
mod tangle;
mod weave;
