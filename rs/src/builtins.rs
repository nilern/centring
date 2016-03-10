use reader::Parser;
use value::{Value, ValueRef, List};
use interpreter::Interpreter;
use std::rc::Rc;
use std::fs::File;
use std::io::Read;

pub fn add_2i(_: &mut Interpreter, args: Vec<ValueRef>) -> ValueRef {
    Rc::new(Value::Int(args[0].get_int().unwrap() + args[1].get_int().unwrap()))
}

pub fn prepend_ls(_: &mut Interpreter, args: Vec<ValueRef>) -> ValueRef {
    Rc::new(Value::List(List::Pair {
        first: args[0].clone(),
        rest: Rc::new(args[1].get_list().unwrap())
    }))
}

pub fn load(itp: &mut Interpreter, args: Vec<ValueRef>) -> ValueRef {
    let filename = args[0].get_str().unwrap();
    let mut f = File::open(filename).unwrap();
    let mut input = String::new();
    f.read_to_string(&mut input).unwrap();
    let (sexprs, _) = Parser::new(&input).parse_exprs();
    let mut res = Rc::new(Value::Tuple(vec![]));
    for sexpr in sexprs {
        let expr = itp.expand_all(sexpr);
        res = itp.eval(&expr);
    }
    res
}
    
