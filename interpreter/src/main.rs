#![feature(alloc, heap_api, ptr_eq)]

extern crate alloc;
extern crate rustc_serialize;
extern crate docopt;
extern crate rustyline;
extern crate arrayvec;

#[macro_use]
pub mod value;
pub mod ops;
pub mod gc;
pub mod primops;
pub mod refs; // FIXME: all non-static methods should be on these
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
Usage: ctri [--help | --sexp | --ana] -e <expr>
       ctri [--help | --sexp | --ana]
";

#[derive(RustcDecodable)]
struct Args {
    flag_help: bool,
    flag_sexp: bool,
    flag_ana: bool,
    arg_expr: Option<String>,
}

impl Args {
    fn act(&self, itp: &mut Interpreter, sexp: ValueHandle<Any>) -> CtrResult<Any> {
        if self.flag_ana {
            analyze(itp, sexp).and_then(|ast| ast_to_sexpr(itp, ast.borrow()))
        } else if self.flag_sexp {
            Ok(sexp.root())
        } else {
            let env = itp.global_env.clone();
            analyze(itp, sexp).and_then(|ast| itp.interpret(ast.borrow(), env))
        }
    }

    fn act_and_print(&self, itp: &mut Interpreter, estr: String) {
        let mut st = read::ParseState::new(estr);
        match read::read(itp, &mut st) {
            Ok(Some(v)) => {
                match self.act(itp, v.borrow()) {
                    Ok(sexp) => println!("{}", ContextValue::new(sexp.borrow(), itp)),
                    Err(e) => println!("{:?}", e)
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
                        ed.add_history_entry(&estr);
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
