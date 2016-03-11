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

pub fn set_module(itp: &mut Interpreter, args: Vec<ValueRef>) -> ValueRef {
    let mod_name = args[0].get_str().unwrap();
    itp.set_mod(mod_name);
    Rc::new(Value::Tuple(vec![]))
}

pub fn record_type(itp: &mut Interpreter, args: Vec<ValueRef>) -> ValueRef {
    let name = args[0].get_string().unwrap();
    let supertyp = args[1].clone();
    if ! supertyp.is_abs_type() { panic!() }
    let field_names = args[2..].iter().map(|s| s.get_string().unwrap()).collect();
    itp.create_record_type(name, Some(supertyp), field_names)
}

pub fn abstract_type(itp: &mut Interpreter, args: Vec<ValueRef>) -> ValueRef {
    let name = args[0].get_string().unwrap();
    let supertyp = args[1].clone();
    if ! supertyp.is_abs_type() { panic!() }
    itp.create_abstract_type(name, Some(supertyp))
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
    
