use std::rc::Rc;
use std::fmt;
use std::fmt::{Debug, Formatter};

use vm::CodeObject;
use builtins::NativeFnCode;

#[derive(Debug)]
pub enum Value {
    Int(isize),
    Bool(bool),
    Char(char),

    Symbol(Symbol),
    Keyword(Symbol),

    Tuple(Vec<ValueRef>),

    Record(Record),
    Singleton(Singleton),

    RecordType(FieldType),
    SingletonType(FieldlessType),
    AbstractType(FieldlessType),
    BuiltInType(FieldlessType),

    Fn(Closure),
    NativeFn(NativeFn),
    MultiFn(MultiFn)
}

pub type ValueRef = Rc<Value>;

#[derive(Debug)]
pub struct Symbol(Option<String>, String);

#[derive(Debug)]
pub struct Record {
    typ: ValueRef,
    vals: Vec<ValueRef>
}

#[derive(Debug)]
pub struct Singleton {
    typ: ValueRef
}

#[derive(Debug)]
pub struct FieldType {
    name: ValueRef,
    supertyp: ValueRef,
    field_names: Vec<String>
}

#[derive(Debug)]
pub struct FieldlessType {
    name: ValueRef,
    supertyp: ValueRef
}

#[derive(Debug)]
pub struct Closure {
    pub codeobj: Rc<CodeObject>,
    pub clovers: Rc<Vec<ValueRef>>
}

pub struct NativeFn {
    pub code: NativeFnCode
}

impl Debug for NativeFn {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "#<NativeFn @ {:p}>", self)
    }
}

#[derive(Debug)]
pub struct MultiFn {
    name: ValueRef,
    // methods: RefCell<HashMap<(Vec<TypeMatcher>, Option<TypeMatcher>), ValueRef>>
}

#[derive(Debug)]
enum TypeMatcher {
    Isa(ValueRef),
    Identical(ValueRef)
}

impl Value {
    pub fn unwrap_int(&self) -> isize {
        if let Value::Int(i) = *self {
            i
        } else {
            panic!()
        }
    }
    
    pub fn unwrap_bool(&self) -> bool {
        if let Value::Bool(b) = *self {
            b
        } else {
            panic!()
        }
    }

    pub fn unwrap_codeobj(&self) -> Rc<CodeObject> {
        if let Value::Fn(Closure { ref codeobj, .. }) = *self {
            codeobj.clone()
        } else {
            panic!()
        }
    }

    pub fn unwrap_clovers(&self) -> Rc<Vec<ValueRef>> {
        if let Value::Fn(Closure { ref clovers, .. }) = *self {
            clovers.clone()
        } else {
            panic!()
        }
    }
}
