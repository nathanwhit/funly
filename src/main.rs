use funly::compile::compile;
use std::{
    fs,
    io::{self, Read},
};
use tracing_subscriber::{layer::SubscriberExt, EnvFilter};
use tracing_tree::HierarchicalLayer;

use argh::FromArgs;

#[derive(FromArgs)]
/// Args
struct Args {
    #[argh(option)]
    /// path to program file
    path: Option<String>,
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;
    tracing::subscriber::set_global_default(
        tracing_subscriber::registry()
            .with(tracing_error::ErrorLayer::default())
            .with(EnvFilter::from_default_env())
            .with(HierarchicalLayer::default().with_indent_lines(true)),
    )?;
    let args: Args = argh::from_env();
    let input = match args.path {
        Some(path) => {
            let input = fs::read_to_string(path)?;
            input
        }
        None => {
            let mut buf = Vec::new();
            let stdin = io::stdin();
            stdin.lock().read_to_end(&mut buf)?;
            let input = String::from_utf8(buf)?;
            input
        }
    };

    compile(&input)?;

    Ok(())
}
