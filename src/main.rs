use std::io::{self, Write};

use waffle::interpret;

fn main() -> io::Result<()> {
    // repl
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

        match interpret(&input) {
            Ok(value) => println!("{value}"),
            Err(error) => eprintln!("{error:?}"),
        }
    }

    Ok(())
}
