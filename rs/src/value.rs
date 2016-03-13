use std::rc::Rc;
use std::iter::FromIterator;
use std::hash::{Hash, Hasher};

use environment::EnvRef;
use interpreter::Interpreter;
use eval::Expr;

pub type ValueRef = Rc<Value>;

pub enum Value {
    Int(isize),
    Bool(bool),
    Char(char),
    Symbol(Option<String>, String),
    Keyword(Option<String>, String),

    Tuple(Vec<ValueRef>),
    List(List<ValueRef>),
    String(String),

    Record { typ: ValueRef, vals: Vec<ValueRef> },
    Singleton { typ: ValueRef },

    AbstractType { name: ValueRef, supertyp: Option<ValueRef> },
    RecordType { name: ValueRef, supertyp: Option<ValueRef>,
                 field_names: Vec<String> },
    SingletonType { name: ValueRef, supertyp: Option<ValueRef> },
    BuiltInType { name: ValueRef, supertyp: Option<ValueRef> },

    Fn {
        name: String,
        formal_names: Vec<String>,
        vararg_name: Option<String>,
        formal_types: Vec<TypeMatcher>,
        vararg_type: Option<ValueRef>,
        body: Rc<Expr>,
        env: EnvRef
    },
    NativeFn {
        name: String,
        formal_types: Vec<TypeMatcher>,
        code: fn(&mut Interpreter, Vec<ValueRef>) -> ValueRef
    },
    Macro(ValueRef)
}

impl Value {
    pub fn name(&self) -> Option<&str> {
        if let Value::Symbol(_, ref name) = *self { Some(name) } else { None }
    }

    pub fn is_symbol(&self) -> bool {
        if let Value::Symbol(..) = *self { true } else { false }
    }

    pub fn is_macro(&self) -> bool {
        if let Value::Macro(_) = *self { true } else { false }
    }

    pub fn is_abs_type(&self) -> bool {
        if let Value::AbstractType {..} = *self { true } else { false }
    }
        
    pub fn get_int(&self) -> Option<isize> {
        if let Value::Int(i) = *self { Some(i) } else { None }
    }

    pub fn get_bool(&self) -> Option<bool> {
        if let Value::Bool(b) = *self { Some(b) } else { None }
    }

    pub fn get_list(&self) -> Option<List<ValueRef>> {
        if let Value::List(ref ls) = *self { Some(ls.clone()) } else { None }
    }

    pub fn get_str(&self) -> Option<&str> {
        if let Value::String(ref s) = *self { Some(s) } else { None }
    }

    pub fn as_kw(&self) -> Option<Value> {
        if let Value::Symbol(ref mod_name, ref name) = *self {
            Some(Value::Keyword(mod_name.clone(), name.clone()))
        } else {
            None
        }
    }

    pub fn as_id(&self) -> Option<Expr> {
        match *self {
            Value::Keyword(None, ref name) => Some(Expr::Local(name.clone())),
            Value::Keyword(Some(ref mod_name), ref name) =>
                Some(Expr::Global(mod_name.clone(), name.clone())),
            _ => None
        }
    }

    pub fn get_string(&self) -> Option<String> {
        if let Value::String(ref s) = *self { Some(s.clone()) } else { None }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Value::Int(i) => i.hash(state),
            Value::Bool(b) => b.hash(state),
            Value::Char(c) => c.hash(state),
            Value::Symbol(ref mod_name, ref name) |
            Value::Keyword(ref mod_name, ref name) => {
                mod_name.hash(state);
                name.hash(state);
            },

            Value::Tuple(ref vs) => vs.hash(state),
            Value::List(ref vs) => vs.hash(state),
            Value::String(ref s) => s.hash(state),

            Value::Record { ref typ, ref vals } => {
                typ.hash(state);
                vals.hash(state);
            },
            Value::Singleton { ref typ } => typ.hash(state),

            Value::AbstractType { ref name, ref supertyp } |
            Value::SingletonType { ref name, ref supertyp } |
            Value::BuiltInType { ref name, ref supertyp } => {
                name.hash(state);
                supertyp.hash(state);
            },
            Value::RecordType { ref name, ref supertyp, ref field_names } => {
                name.hash(state);
                supertyp.hash(state);
                field_names.hash(state);
            },

            Value::Fn { ref name, ref formal_names, ref vararg_name,
                        ref formal_types, ref vararg_type, .. } => {
                name.hash(state);
                formal_names.hash(state);
                vararg_name.hash(state);
                formal_types.hash(state);
                vararg_type.hash(state);
            },
            Value::NativeFn { ref name, ref formal_types, ref code } => {
                name.hash(state);
                formal_types.hash(state);
                code.hash(state);
            },
            Value::Macro(ref expander) => expander.hash(state)
        }   
    }
}

#[derive(Hash)]
pub enum TypeMatcher {
    Isa(ValueRef),
    Identical(ValueRef),
}

#[derive(Debug, Clone)]
pub enum List<T> {
    Pair { rest: Rc<List<T>>, first: T },
    Empty
}

impl<T: Hash> Hash for List<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for v in self.iter() {
            v.hash(state)
        }
    }
}

// List Iteration

impl<T> List<T> {
    pub fn iter(&self) -> ListIter<T> {
        ListIter(self)
    }
}

pub fn prepend<T>(v: T, ls: &Rc<List<T>>) -> List<T> {
    List::Pair {
        first: v,
        rest: ls.clone()
    }
}

pub struct ListIter<'a, T>(&'a List<T>) where T: 'a;

impl<'a, T> Iterator for ListIter<'a, T> {
    type Item = &'a T;
    
    fn next(&mut self) -> Option<&'a T> {
        if let List::Pair { ref first, ref rest } = *self.0 {
            self.0 = rest;
            Some(first)
        } else {
            None
        }
    }
}

impl<T> FromIterator<T> for List<T> {
    fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> List<T> {
        let mut it = iter.into_iter();
        if let Some(v) = it.next() {
            prepend(v, &Rc::new(FromIterator::from_iter(it)))
        } else {
            List::Empty
        }
    }
}
