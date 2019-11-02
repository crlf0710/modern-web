use super::lexer::control_code::{ControlCode, ControlCodeKind};
use super::lexer::token::{Token, TokenList};
use std::cell::RefCell;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug)]
pub enum CodeModule<'x> {
    Anonymous(TokenList<'x>),
    Named(CodeModuleName<'x>, TokenList<'x>),
}

#[derive(Debug)]
pub struct CodeModuleName<'x> {
    name: Rc<TokenList<'x>>,
    is_shorthand: bool,
    resolved_fullname: RefCell<Option<Rc<TokenList<'x>>>>,
}

pub type CodeModuleList<'x> = Vec<CodeModule<'x>>;

#[derive(Error, Debug)]
pub enum ParseCodeModuleError {
    #[error("unexpected token")]
    UnexpectedToken,
    #[error("any parse module error")]
    AnyParseCodeModuleError,
    #[error("Not ascii string")]
    NotAsciiString(#[from] super::lexer::ascii_str::NotAsciiStrError),
}

fn is_anonymous_code_module_ctrl_code(ctrl_code: &ControlCode) -> bool {
    ctrl_code.kind == ControlCodeKind::DefineProgram
}

fn maybe_named_code_module_ctrl_code(ctrl_code: &ControlCode) -> bool {
    ctrl_code.kind == ControlCodeKind::ModuleName
}

fn read_named_code_module_name<'x>(
    v: &mut impl Iterator<Item = Token<'x>>,
) -> Result<CodeModuleName<'x>, ParseCodeModuleError> {
    use super::lexer::ascii_str;
    use super::lexer::operator::Operator;
    use super::lexer::punctuation::Punctuation;
    let first_token = v.next().ok_or(ParseCodeModuleError::UnexpectedToken)?;
    let second_token = v.next().ok_or(ParseCodeModuleError::UnexpectedToken)?;
    let mut module_name_tokens = match (first_token, second_token) {
        (
            Token::CtrlCode(ControlCode {
                kind: ControlCodeKind::ModuleName,
                param: module_name_tokens,
            }),
            Token::Punct(Punctuation::Op(Operator::Equal)),
        ) => module_name_tokens.map(|x| *x).unwrap_or_default(),
        _ => return Err(ParseCodeModuleError::UnexpectedToken),
    };
    let mut is_shorthand = false;

    match module_name_tokens.pop() {
        Some(Token::TextFragment(text_str)) => {
            let (head, tail) = text_str.try_split_ending_substr(ascii_str::from_bytes(b"...")?);
            module_name_tokens.push(Token::TextFragment(head));
            if let Some(_) = tail {
                is_shorthand = true;
            }
        }
        Some(v) => {
            module_name_tokens.push(v);
        }
        None => {}
    }
    let module_name = CodeModuleName {
        name: Rc::new(module_name_tokens),
        is_shorthand,
        resolved_fullname: RefCell::new(None),
    };
    Ok(module_name)
}

fn read_code_module_tokens<'x>(
    token_stream: &mut itertools::MultiPeek<impl Iterator<Item = Token<'x>>>,
) -> Vec<Token<'x>> {
    use super::lexer::operator::Operator;
    use super::lexer::punctuation::Punctuation;
    let mut tokens = Vec::new();
    'code_module_loop: loop {
        token_stream.reset_peek();
        match token_stream.peek() {
            Some(Token::CtrlCode(ctrl_code)) => {
                if is_anonymous_code_module_ctrl_code(ctrl_code) {
                    break 'code_module_loop;
                } else if maybe_named_code_module_ctrl_code(ctrl_code) {
                    if token_stream.peek() == Some(&Token::Punct(Punctuation::Op(Operator::Equal)))
                    {
                        break 'code_module_loop;
                    }
                }
            }
            Some(_) => {},
            None => {
                break 'code_module_loop;
            }
        }
        tokens.push(token_stream.next().unwrap());
    }
    tokens
}

pub fn parse_codemodules<'x>(
    tokens: TokenList<'x>,
) -> Result<CodeModuleList<'x>, ParseCodeModuleError> {
    use itertools::multipeek;
    let mut code_module_list = vec![];
    //println!("{:?}", tokens);
    let mut token_stream = multipeek(tokens);
    'code_module_loop: loop {
        token_stream.reset_peek();
        match token_stream.peek() {
            Some(Token::CtrlCode(ctrl_code)) => {
                let module_name;
                if is_anonymous_code_module_ctrl_code(ctrl_code) {
                    let _ = token_stream.next();
                    module_name = None;
                } else if maybe_named_code_module_ctrl_code(ctrl_code) {
                    module_name = Some(read_named_code_module_name(&mut token_stream)?);
                } else {
                    //unimplemented!("unexpected token: {:?}", ctrl_code);
                    return Err(ParseCodeModuleError::UnexpectedToken);
                }
                let tokens = read_code_module_tokens(&mut token_stream);
                let code_module = match module_name {
                    None => CodeModule::Anonymous(tokens),
                    Some(name) => CodeModule::Named(name, tokens),
                };
                code_module_list.push(code_module);
            }
            Some(Token::WS) => {
                let _ = token_stream.next();
                continue 'code_module_loop;
            }
            Some(_) => {
                unreachable!();
            }
            None => {
                break 'code_module_loop;
            }
        }
    }
    Ok(code_module_list)
}
