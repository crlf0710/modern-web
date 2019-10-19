use anyhow::{anyhow, Context, Result};
use clap::{App, Arg};
use mweb::classical::WEB;

fn main() -> Result<()> {
    use std::path::Path;
    let matches = App::new("mweb-tangle")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .get_matches();
    let path = matches
        .value_of_os("INPUT")
        .ok_or_else(|| anyhow!("input file not specified"))?;
    let path = Path::new(path);
    let input_data = std::fs::read(path)
        .with_context(|| format!("failed to read data from {}", path.display()))?;

    let web_data = WEB::parse(&input_data)?;
    //println!("{:?}", input_data);
    Ok(())
}
