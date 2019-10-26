use super::WEB;
use std::collections::BTreeMap;
use std::io;
use thiserror::Error;

enum MacroDefinition<'x> {
    Numeric {
        name: &'x [u8],
        content: &'x [u8],
    },
    Tokens {
        name: &'x [u8],
        has_arg: bool,
        content: &'x [u8],
    },
}

impl<'x> MacroDefinition<'x> {
    fn name(&self) -> &'x [u8] {
        match self {
            MacroDefinition::Numeric { name, .. } => *name,
            MacroDefinition::Tokens { name, .. } => *name,
        }
    }
}

#[derive(Error, Debug)]
pub enum WEBParseError {
    #[error("parse error")]
    AnyWEBParseError,
}

use WEBParseError::AnyWEBParseError;

trait OkOrParseError<T> {
    fn ok_or_parse_error(self) -> Result<T, WEBParseError>;
}

impl<T> OkOrParseError<T> for Option<T> {
    fn ok_or_parse_error(self) -> Result<T, WEBParseError> {
        match self {
            Some(v) => Ok(v),
            None => Err(WEBParseError::AnyWEBParseError),
        }
    }
}

/*
fn parse_macro_definition(l: &[u8]) -> Result<MacroDefinition<'_>, WEBParseError> {
    use super::lexer::ascii_char::*;
    use super::lexer::complementary;
    use super::lexer::tokenize::*;
    let is_not_alphanumeric_char = complementary(is_alphanumeric_char);
    let (_, l) =
        parse_fixed_look_ahead_1(l, b"@d", is_not_alphanumeric_char).ok_or_parse_error()?;
    let (_, l) = parse_maybe_whitespace(l);
    let (name, l) = parse_identifier(l).ok_or_parse_error()?;
    let (has_arg, l) = parse_maybe_fixed_look_ahead_1(l, b"(#)", |_| true);
    let has_arg = !has_arg.is_empty();
    let (_, l) = parse_maybe_whitespace(l);
    let (double_eq, l) = parse_maybe_fixed_look_ahead_1(l, b"==", |_| true);
    let double_eq = !double_eq.is_empty();
    let (single_eq, l) = if !double_eq {
        if has_arg {
            let _: () = None.ok_or_parse_error()?;
        }
        let (single_eq, l) = parse_fixed_look_ahead_1(l, b"=", |_| true).ok_or_parse_error()?;
        let single_eq = !single_eq.is_empty();
        assert!(single_eq);
        (single_eq, l)
    } else {
        (false, l)
    };
    let definition;
    if single_eq {
        definition = MacroDefinition::Numeric { name, content: l }
    } else {
        assert!(double_eq);
        definition = MacroDefinition::Tokens {
            name,
            has_arg,
            content: l,
        };
    }
    Ok(definition)
}
*/

#[derive(Default)]
struct MacroDefinitionSet<'x>(BTreeMap<&'x [u8], MacroDefinition<'x>>);

impl<'x> MacroDefinitionSet<'x> {
    fn load_from(web_data: &WEB<'x>) -> Result<Self, WEBParseError> {
        let mut set = MacroDefinitionSet::default();
        for (_module_idx, module_text) in web_data.text_in_modules.iter().enumerate() {
            for definition_text in module_text.macro_in_definitions.iter() {
                unimplemented!();
                //let definition = parse_macro_definition(definition_text)?;
                //set.insert(definition)?;
            }
        }

        Ok(set)
    }

    fn insert(&mut self, definition: MacroDefinition<'x>) -> Result<(), WEBParseError> {
        let name = definition.name();
        self.0.insert(name, definition);
        Ok(())
    }
}

impl<'x> WEB<'x> {
    pub fn tangle(&self, output: &mut dyn io::Write) -> Result<(), WEBTangleProcessError> {
        // collect macro definitions.
        let macro_set = MacroDefinitionSet::load_from(self)?;
        for (&k, v) in macro_set.0.iter() {
            output.write_all(b"[")?;
            output.write_all(k)?;
            output.write_all(b"]")?;
            match v {
                MacroDefinition::Numeric { content, .. } => {
                    output.write_all(b" = ")?;
                    output.write_all(content)?;
                    output.write_all(b"\n")?;
                }
                MacroDefinition::Tokens {
                    has_arg, content, ..
                } => {
                    if *has_arg {
                        output.write_all(b" _")?;
                    }
                    output.write_all(b"\n")?;
                    output.write_all(b"    => ")?;
                    output.write_all(content)?;
                    output.write_all(b"\n")?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum WEBTangleProcessError {
    #[error("web data parse error")]
    ParseError(#[from] WEBParseError),
    #[error("web output io error")]
    IOError(#[from] io::Error),
}
