use std::io::{self, Read};

use funly::{ast::AstContext, parse::parser};

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;
    let mut buf = Vec::new();
    let stdin = io::stdin();
    stdin.lock().read_to_end(&mut buf)?;
    let input = String::from_utf8(buf)?;
    println!("{input}");
    let mut ctx = AstContext::new();
    println!("{:?}", &input[12..13]);
    let program = parser::program(&input, &mut ctx)?;
    println!("{program:?}");
    Ok(())
}
