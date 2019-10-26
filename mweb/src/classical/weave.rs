use super::WEB;
use std::io;
use thiserror::Error;

impl<'x> WEB<'x> {
    pub fn weave(&self, output: &mut dyn io::Write) -> Result<(), WEBWeaveProcessError> {
        output.write_all(b"\\input webmac\n")?;
        /*
        for limbo_text in &self.text_in_limbo.lines {
            output.write_all(limbo_text)?;
            output.write_all(b"\n")?;
        }

        //debug only
        for (module_idx, module_text) in self.text_in_modules.iter().enumerate() {
            use super::ModuleType;
            match &module_text.module_type {
                ModuleType::Starred { group_title } => {
                    output.write_all(b"-----------------------------------\n")?;
                    output.write_all(group_title)?;
                    output.write_all(b"\n")?;
                    write!(output, "Module {number}.\n", number = module_idx + 1)?;
                }
                ModuleType::Normal => {
                    write!(output, "Module {number}.\n", number = module_idx + 1)?;
                }
            }
        }
        */
        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum WEBWeaveProcessError {
    #[error("IO writing error")]
    IOError(#[from] io::Error),
    #[error("not yet implemented")]
    NotYetImplemented,
}
