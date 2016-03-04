use std::rc::Rc;
use std::collections::HashMap;

pub type ValueRef = Rc<Value>;

#[derive(Debug)]
pub enum Value {
    Int(isize),
    Bool(bool),
    Char(char),
    Symbol(Option<String>, String),

    Tuple(Vec<ValueRef>),
    Array(Vec<ValueRef>),
    Pair { first: ValueRef, rest: ValueRef },
    EmptyList,

    Record { typ: ValueRef, vals: Vec<ValueRef> },
    Singleton { typ: ValueRef },

    AbstractType { name: String, supertyp: ValueRef },
    RecordType { name: String, supertyp: ValueRef, field_names: Vec<String> },
    SingletonType { name: String, supertyp: ValueRef },
    BuiltInType { name: String, supertyp: ValueRef },

    Fn {
        name: String,
        formal_names: Vec<String>,
        formal_types: Vec<ValueRef>,
        body: ValueRef,
        env: Rc<Environment>
    },
    NativeFn {
        name: String,
        formal_types: Vec<ValueRef>,
        code: fn(Interpreter, Vec<ValueRef>) -> ValueRef
    }
}

#[derive(Debug)]
pub struct Environment {
    bindings: HashMap<String, ValueRef>,
    parent: Option<Rc<Environment>>
}

pub struct Interpreter;

// Value Utils

pub fn prepend(v: ValueRef, coll: ValueRef) -> Value {
    match *coll {
        Value::EmptyList | Value::Pair { .. } => Value::Pair {
            first: v,
            rest: coll
        },
        _ => panic!()
    }
}

pub fn vec_to_list(vs: Vec<ValueRef>) -> Value {
    let mut res = Value::EmptyList;
    for v in vs.into_iter().rev() {
        res = Value::Pair { first: v, rest: Rc::new(res) };
    }
    res
}
