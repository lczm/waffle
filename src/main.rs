use std::{
    fs,
    io::{self, Write},
    path::{Path, PathBuf},
};

use clap::Parser;
use miette::{IntoDiagnostic, WrapErr};
use waffle::{VM, interpret};

#[derive(Parser)]
#[command(version, about = "A Lisp bytecode interpreter")]
struct Cli {
    #[arg(
        value_name = "FILE",
        required_unless_present = "repl",
        conflicts_with = "repl"
    )]
    file: Option<PathBuf>,

    #[arg(short, long)]
    repl: bool,
}

fn main() -> miette::Result<()> {
    let cli = Cli::parse();

    if cli.repl {
        run_repl().into_diagnostic()?;
    } else if let Some(path) = cli.file {
        run_file(&path)?;
    }

    Ok(())
}

fn run_file(path: &Path) -> miette::Result<()> {
    let source = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to read {}", path.display()))?;
    let mut vm = VM::new();
    let value = interpret(&mut vm, &source)?;
    println!("{value}");
    Ok(())
}

fn run_repl() -> io::Result<()> {
    let mut vm = VM::new();
    loop {
        print!("waffle> ");
        io::stdout().flush()?;

        let mut input = String::new();
        let bytes_read = io::stdin().read_line(&mut input)?;

        // ctrl-d on linux/mac to break out of the repl
        if bytes_read == 0 {
            println!();
            break;
        }

        if input.trim().is_empty() {
            continue;
        }

        match interpret(&mut vm, &input) {
            Ok(value) => println!("{value}"),
            Err(error) => eprintln!("{error:?}"),
        }
    }

    Ok(())
}
