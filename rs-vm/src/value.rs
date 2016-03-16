use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use bytecode::Bytecode;
use builtins::NativeFnCode;

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

pub struct Symbol(Option<String>, String);

pub struct Record {
    typ: ValueRef,
    vals: Vec<ValueRef>
}

pub struct Singleton {
    typ: ValueRef
}

pub struct FieldType {
    name: ValueRef,
    supertyp: ValueRef,
    field_names: Vec<String>
}

pub struct FieldlessType {
    name: ValueRef,
    supertyp: ValueRef
}

pub struct Closure {
    pub code: Rc<CodeObject>,
    pub clovers: Rc<Vec<ValueRef>>
}

pub struct CodeObject {
    pub instrs: Rc<Vec<Bytecode>>,
    pub consts: Rc<Vec<ValueRef>>,
    pub codeobjs: Rc<Vec<Rc<CodeObject>>>
}

pub struct NativeFn {
    pub code: NativeFnCode
}

pub struct MultiFn {
    name: ValueRef,
    methods: RefCell<HashMap<(Vec<TypeMatcher>, Option<TypeMatcher>), ValueRef>>
}

enum TypeMatcher {
    Isa(ValueRef),
    Identical(ValueRef)
}

impl Value {
    pub fn unwrap_bool(&self) -> bool {
        if let Value::Bool(b) = *self {
            b
        } else {
            panic!()
        }
    }
}
