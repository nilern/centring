use reader::Parser;
use value::{Value, ValueRef, List};
use eval::{Interpreter, Expr};
use std::rc::Rc;
use std::fs::File;
use std::io::Read;

pub fn add_2i(_: &mut Interpreter, args: Vec<ValueRef>) -> ValueRef {
    let a = args[0].get_int().unwrap();
    let b = args[1].get_int().unwrap();
    Rc::new(Value::Int(a + b))
}

pub fn prepend_ls(_: &mut Interpreter, args: Vec<ValueRef>) -> ValueRef {
    let v = args[0].clone();
    let ls = args[1].get_list().unwrap();
    Rc::new(Value::List(List::Pair {
        first: v.clone(),
        rest: Rc::new(ls)
    }))
}

pub fn load(itp: &mut Interpreter, args: Vec<ValueRef>) -> ValueRef {
    let filename = args[0].get_str().unwrap();
    let mut f = File::open(filename).unwrap();
    let mut input = String::new();
    f.read_to_string(&mut input);
    let (sexprs, _) = Parser::new(&input).parse_exprs();
    let mut res = Rc::new(Value::Tuple(vec![]));
    let mut expr = Expr::Const(res.clone());
    for sexpr in sexprs {
        expr = itp.expand_all(sexpr);
        res = itp.eval(&expr);
    }
    res
}
    
