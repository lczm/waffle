use waffle::interpret;

fn main() -> miette::Result<()> {
    // let source = "(+ 1 (* 2 5))"
    let source = "(+ 1 (+ 2 3))";
    // let function_source = "(defun add (a b) (+ a b))";
    // let function_invalid_source = "(defun add (a b) (+ a; b))";

    interpret(source)?;
    // println!("{}", interpret(source)?);

    Ok(())
}
