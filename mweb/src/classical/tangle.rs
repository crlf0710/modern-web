use super::WEB;
use std::collections::BTreeMap;
use std::io;
use thiserror::Error;

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
#[derive(Default)]
struct MacroDefinitionSet<'x>(BTreeMap<&'x [u8], MacroDefinition<'x>>);

impl<'x> MacroDefinitionSet<'x> {
    fn load_from(web_data: &WEB<'x>) -> Result<Self, WEBParseError> {
        let mut set = MacroDefinitionSet::default();
        for (_module_idx, module_text) in web_data.text_in_modules.iter().enumerate() {
            for definition_text in module_text.definitions.iter() {
                // unimplemented!();
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
*/

impl<'x> WEB<'x> {
    pub fn tangle(&self, output: &mut dyn io::Write) -> Result<(), WEBTangleProcessError> {
        // collect macro definitions.
        /*
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
        */
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
