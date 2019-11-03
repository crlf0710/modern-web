use super::lexer::ascii_str::AsciiStr;
use super::lexer::token::{BoxedTokenList, Token};
use thiserror::Error;

#[derive(Debug)]
pub enum MacroDefinition<'x> {
    Numeric {
        name: &'x AsciiStr,
        content_and_comments: BoxedTokenList<'x>,
    },
    Tokens {
        name: &'x AsciiStr,
        has_arg: bool,
        content: BoxedTokenList<'x>,
    },
}

#[derive(Debug)]
pub enum FormatDefinition<'x> {
    Format {
        name: &'x AsciiStr,
        target_ident: &'x AsciiStr,
        comments: BoxedTokenList<'x>,
    },
}

#[derive(Debug)]
pub enum Definition<'x> {
    Macro(MacroDefinition<'x>),
    Format(FormatDefinition<'x>),
}

pub type DefinitionList<'x> = Vec<Definition<'x>>;

#[derive(Error, Debug)]
pub enum ParseDefinitionError {
    #[error("invalid macro")]
    InvalidMacro,
    #[error("invalid numeric macro")]
    InvalidNumericMacro,
    #[error("invalid token macro")]
    InvalidTokenMacro,
    #[error("invalid format")]
    InvalidFormat,
    #[error("unknown definition")]
    UnknownDefinition,
}

pub(crate) fn parse_definition<'x>(
    token: Token<'x>,
) -> Result<Option<Definition<'x>>, ParseDefinitionError> {
    use super::super::utils::PeekingNextIfEq;
    use super::lexer::control_code::{ControlCode, ControlCodeKind};
    use super::lexer::operator::Operator;
    use super::lexer::punctuation::Punctuation;
    use itertools::Itertools;

    match token {
        Token::CtrlCode(ControlCode {
            kind: ControlCodeKind::DefineMacro,
            param,
        }) => {
            let tokens = *param.ok_or(ParseDefinitionError::InvalidMacro)?;
            let mut token_iter = tokens.into_iter().peekable();
            let _ = token_iter.peeking_next_if_eq(Token::WS);
            let name = match token_iter.next() {
                Some(Token::IdentOrKw(name)) => name,
                _ => {
                    return Err(ParseDefinitionError::InvalidMacro);
                }
            };
            let _ = token_iter.peeking_next_if_eq(Token::WS);
            let has_arg;
            if let Some(_) = token_iter.peeking_next_if_eq(Token::Punct(Punctuation::LParen)) {
                let _ = token_iter.peeking_next_if_eq(Token::WS);
                let _ = token_iter
                    .peeking_next_if_eq(Token::MacroParamMark)
                    .ok_or(ParseDefinitionError::InvalidTokenMacro)?;
                let _ = token_iter.peeking_next_if_eq(Token::WS);
                let _ = token_iter
                    .peeking_next_if_eq(Token::Punct(Punctuation::RParen))
                    .ok_or(ParseDefinitionError::InvalidTokenMacro)?;
                let _ = token_iter.peeking_next_if_eq(Token::WS);
                has_arg = true;
            } else {
                has_arg = false;
            }
            let numeric;
            if has_arg {
                let _ = token_iter
                    .peeking_next_if_eq(Token::Punct(Punctuation::DefineAs))
                    .ok_or(ParseDefinitionError::InvalidTokenMacro)?;
                numeric = false;
            } else if let Some(_) =
                token_iter.peeking_next_if_eq(Token::Punct(Punctuation::DefineAs))
            {
                numeric = false
            } else {
                let _ = token_iter
                    .peeking_next_if_eq(Token::Punct(Punctuation::Op(Operator::Equal)))
                    .ok_or(ParseDefinitionError::InvalidMacro)?;
                numeric = true;
            }
            if numeric {
                let definition = Definition::Macro(MacroDefinition::Numeric {
                    name,
                    content_and_comments: Box::new(token_iter.collect()),
                });
                Ok(Some(definition))
            } else {
                let definition = Definition::Macro(MacroDefinition::Tokens {
                    name,
                    has_arg,
                    content: Box::new(token_iter.collect()),
                });
                Ok(Some(definition))
            }
        }
        Token::CtrlCode(ControlCode {
            kind: ControlCodeKind::DefineFormat,
            param,
        }) => {
            let tokens = *param.ok_or(ParseDefinitionError::InvalidFormat)?;
            let mut token_iter = tokens.into_iter().peekable();
            let _ = token_iter.peeking_next_if_eq(Token::WS);
            let name = match token_iter.next() {
                Some(Token::IdentOrKw(name)) => name,
                _ => {
                    return Err(ParseDefinitionError::InvalidFormat);
                }
            };
            let _ = token_iter.peeking_next_if_eq(Token::WS);
            let _ = token_iter
                .peeking_next_if_eq(Token::Punct(Punctuation::DefineAs))
                .ok_or(ParseDefinitionError::InvalidFormat)?;
            let _ = token_iter.peeking_next_if_eq(Token::WS);
            let target_ident = match token_iter.next() {
                Some(Token::IdentOrKw(ident)) => ident,
                _ => {
                    return Err(ParseDefinitionError::InvalidFormat);
                }
            };
            let comments = token_iter
                .peeking_take_while(|token| match token {
                    Token::WS => true,
                    Token::Comment(..) => true,
                    Token::CtrlCode(ControlCode {
                        kind: ControlCodeKind::FormatLineBreakLarge,
                        ..
                    }) => true,
                    _ => {
                        /*unimplemented!("token: {:?}", token) */
                        false
                    }
                })
                .collect();
            if token_iter.peek().is_some() {
                return Err(ParseDefinitionError::InvalidFormat);
            }
            let definition = Definition::Format(FormatDefinition::Format {
                name,
                target_ident,
                comments: Box::new(comments),
            });
            Ok(Some(definition))
        }
        Token::WS => Ok(None),
        _ => Err(ParseDefinitionError::UnknownDefinition),
    }
}
