use reader::Parser;
use value::{Value, ValueRef, List};
use interpreter::Interpreter;
use std::rc::Rc;
use std::fs::File;
use std::io::Read;

pub type NativeFnCode = fn(&mut Interpreter, Vec<ValueRef>, Vec<ValueRef>)
                           -> ValueRef;

pub fn add_2i(_: &mut Interpreter, args: Vec<ValueRef>, _: Vec<ValueRef>)
              -> ValueRef {
    Rc::new(Value::Int(args[0].get_int().unwrap() + args[1].get_int().unwrap()))
}

pub fn sub_2i(_: &mut Interpreter, args: Vec<ValueRef>, _: Vec<ValueRef>)
              -> ValueRef {
    Rc::new(Value::Int(args[0].get_int().unwrap() - args[1].get_int().unwrap()))
}

pub fn mul_2i(_: &mut Interpreter, args: Vec<ValueRef>, _: Vec<ValueRef>)
              -> ValueRef {
    Rc::new(Value::Int(args[0].get_int().unwrap() * args[1].get_int().unwrap()))
}

pub fn lt_2i(_: &mut Interpreter, args: Vec<ValueRef>, _: Vec<ValueRef>)
             -> ValueRef {
    Rc::new(Value::Bool(args[0].get_int().unwrap() < args[1].get_int().unwrap()))
}

pub fn prepend_ls(_: &mut Interpreter, args: Vec<ValueRef>,
                  _: Vec<ValueRef>) -> ValueRef {
    Rc::new(Value::List(List::Pair {
        first: args[0].clone(),
        rest: Rc::new(args[1].get_list().unwrap())
    }))
}

pub fn fld_left_pair(_: &mut Interpreter, args: Vec<ValueRef>,
                   _: Vec<ValueRef>) -> ValueRef {
    if let Value::List(List::Pair { ref first, .. }) = *args[0] {
        first.clone()
    } else {
        panic!()
    }
}

pub fn fld_right_pair(_: &mut Interpreter, args: Vec<ValueRef>,
                   _: Vec<ValueRef>) -> ValueRef {
    if let Value::List(List::Pair { ref rest, .. }) = *args[0] {
        Rc::new(Value::List((**rest).clone()))
    } else {
        panic!()
    }
}

pub fn set_module(itp: &mut Interpreter, args: Vec<ValueRef>,
                  _: Vec<ValueRef>) -> ValueRef {
    let mod_name = args[0].get_str().unwrap();
    itp.set_mod(mod_name);
    Rc::new(Value::Tuple(vec![]))
}

pub fn require(itp: &mut Interpreter, args: Vec<ValueRef>, _: Vec<ValueRef>)
               -> ValueRef {
    itp.require(args[0].get_str().unwrap(), args[1].get_string().unwrap());
    Rc::new(Value::Tuple(vec![]))
}

pub fn refer(itp: &mut Interpreter, args: Vec<ValueRef>,
             varvals: Vec<ValueRef>) -> ValueRef {
    itp.refer(args[0].get_str().unwrap(),
              varvals.iter().map(|v| v.get_string().unwrap()).collect());
    Rc::new(Value::Tuple(vec![]))
}

pub fn record_type(itp: &mut Interpreter, args: Vec<ValueRef>,
                   varvals: Vec<ValueRef>) -> ValueRef {
    let name = args[0].get_string().unwrap();
    let supertyp = args[1].clone();
    if ! supertyp.is_abs_type() { panic!() }
    let field_names = varvals.iter().map(|s| s.get_string().unwrap()).collect();
    itp.create_record_type(name, Some(supertyp), field_names)
}

pub fn abstract_type(itp: &mut Interpreter, args: Vec<ValueRef>,
                     varvals: Vec<ValueRef>) -> ValueRef {
    let name = args[0].get_string().unwrap();
    let supertyp = args[1].clone();
    if ! supertyp.is_abs_type() { panic!() }
    itp.create_abstract_type(name, Some(supertyp))
}

pub fn type_of(itp: &mut Interpreter, args: Vec<ValueRef>, _: Vec<ValueRef>)
               -> ValueRef {
    itp.type_of(args[0].as_ref())
}

pub fn supertype(itp: &mut Interpreter, args: Vec<ValueRef>, _: Vec<ValueRef>)
                 -> ValueRef {
    itp.supertype(args[0].as_ref()).unwrap_or_else(
        || itp.load_global("centring.lang", "Any").unwrap())
}

pub fn isa(itp: &mut Interpreter, args: Vec<ValueRef>, _: Vec<ValueRef>)
           -> ValueRef {
    Rc::new(Value::Bool(itp.isa(args[0].clone(), args[1].clone())))
}

pub fn egal(itp: &mut Interpreter, args: Vec<ValueRef>, _: Vec<ValueRef>)
            -> ValueRef {
    Rc::new(Value::Bool(args[0] == args[1]))
}

pub fn apply(itp: &mut Interpreter, args: Vec<ValueRef>, varvals: Vec<ValueRef>)
             -> ValueRef {
    let mut new_args = varvals.clone();
    if let Some(ref expandee) = new_args.pop() {
        if let Value::Tuple(ref vs) = **expandee {
            new_args.extend(vs.clone().into_iter());
        }
    } else {
        panic!();
    }
    itp.call(args[0].clone(), new_args)
}

pub fn load(itp: &mut Interpreter, args: Vec<ValueRef>, _: Vec<ValueRef>)
            -> ValueRef {
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
    
