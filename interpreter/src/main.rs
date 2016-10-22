#![feature(alloc, heap_api, ptr_eq)]

extern crate alloc;
extern crate rustc_serialize;
extern crate docopt;
extern crate rustyline;

pub mod ops;
pub mod gc;
pub mod primops;
pub mod value;
pub mod refs;
pub mod read;
pub mod analyze;
pub mod interpreter;
pub mod write;

use interpreter::{Interpreter, CtrResult};
use value::Any;
use refs::ValueHandle;
use write::ContextValue;
use analyze::{analyze, ast_to_sexpr};

use docopt::Docopt;
use rustyline::Editor;
use rustyline::error::ReadlineError;

const USAGE: &'static str = "
Usage: ctri [--help | --ana] -e <expr>
       ctri [--help | --ana]
";

#[derive(RustcDecodable)]
struct Args {
    flag_help: bool,
    flag_ana: bool,
    arg_expr: Option<String>,
}

impl Args {
    fn act(&self, itp: &mut Interpreter, sexp: ValueHandle<Any>) -> CtrResult<Any> {
        if self.flag_ana {
            analyze(itp, sexp).and_then(|ast| ast_to_sexpr(itp, ast.borrow()))
        } else {
            analyze(itp, sexp).and_then(|ast| itp.interpret(ast.borrow()))
        }
    }

    fn act_and_print(&self, itp: &mut Interpreter, estr: String) {
        let mut st = read::ParseState::new(estr);
        match read::read(itp, &mut st) {
            Ok(Some(v)) => {
                if let Ok(sexp) = self.act(itp, v.borrow()) {
                    println!("{}", ContextValue::new(sexp.borrow(), itp));
                } else {
                    unimplemented!();
                }
            }
            Ok(None) => {}
            Err(e) => println!("ReadError: {:?}", e),
        }
    }

    fn run(&self) {
        let mut itp = Interpreter::new();

        if let Some(ref estr) = self.arg_expr {
            self.act_and_print(&mut itp, estr.clone());
        } else {
            let mut ed = Editor::<()>::new();
            loop {
                let line = ed.readline("ctr> ");
                match line {
                    Ok(estr) => {
                        self.act_and_print(&mut itp, estr);
                    }
                    Err(ReadlineError::Interrupted) |
                    Err(ReadlineError::Eof) => {
                        break;
                    }
                    Err(err) => {
                        println!("Error: {:?}", err);
                        break;
                    }
                }
            }
        }
    }
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .map(|d| d.help(true))
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());
    args.run();
}
