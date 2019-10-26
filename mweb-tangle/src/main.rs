use anyhow::{anyhow, Context, Result};
use clap::{App, Arg};
use mweb::classical::WEB;
use std::io;

fn main() -> Result<()> {
    use std::path::Path;
    let matches = App::new("mweb-tangle")
        .arg(
            Arg::with_name("OUTPUT")
                .help("Sets the output file to use")
                .short("o")
                .takes_value(true)
                .value_name("FILE"),
        )
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .get_matches();
    let input_path = matches
        .value_of_os("INPUT")
        .map(|input_path| Path::new(input_path))
        .ok_or_else(|| anyhow!("input file not specified"))?;
    let input_data = std::fs::read(input_path)
        .with_context(|| format!("failed to read data from {}", input_path.display()))?;

    let web_data = WEB::lex(&input_data)?;

    let stdout = io::stdout();
    let mut stdout_lock = None;
    let mut output_file = None;
    let output_path = matches
        .value_of_os("OUTPUT")
        .map(|output_path| Path::new(output_path));
    let output_data = match output_path {
        Some(output_path) => {
            use std::fs::OpenOptions;
            let file = OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(output_path)
                .with_context(|| format!("failed to write file {}", output_path.display()))?;
            output_file = Some(file);
            output_file.as_mut().unwrap() as &mut dyn io::Write
        }
        None => {
            stdout_lock = Some(stdout.lock());
            stdout_lock.as_mut().unwrap() as &mut dyn io::Write
        }
    };

    web_data.tangle(output_data)?;

    drop(output_data);
    drop(stdout_lock);
    drop(output_file);
    Ok(())
}
