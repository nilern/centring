use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use environment::EnvRef;
use bytecode::Bytecode;
use builtins::NativeFnCode;

enum Value {
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

    Fn(Fn),
    NativeFn(NativeFn),
    MultiFn(MultiFn)
}

pub type ValueRef = Rc<Value>;

struct Symbol(Option<String>, String);

struct Record {
    typ: ValueRef,
    vals: Vec<ValueRef>
}

struct Singleton {
    typ: ValueRef
}

struct FieldType {
    name: ValueRef,
    supertyp: ValueRef,
    field_names: Vec<String>
}

struct FieldlessType {
    name: ValueRef,
    supertyp: ValueRef
}

struct Fn {
    name: ValueRef,
    formal_names: Vec<String>,
    vararg_name: Option<String>,
    formal_types: Vec<TypeMatcher>,
    vararg_type: Option<TypeMatcher>,
    code: Vec<Bytecode>,
    env: EnvRef
}

struct NativeFn {
    name: ValueRef,
    formal_types: Vec<TypeMatcher>,
    vararg_type: Option<TypeMatcher>,
    code: NativeFnCode
}

struct MultiFn {
    name: ValueRef,
    methods: RefCell<HashMap<(Vec<TypeMatcher>, Option<TypeMatcher>), ValueRef>>
}

enum TypeMatcher {
    Isa(ValueRef),
    Identical(ValueRef)
}
