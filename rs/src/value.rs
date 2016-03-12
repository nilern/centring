use std::rc::Rc;
use std::iter::FromIterator;

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

pub enum TypeMatcher {
    Isa(ValueRef),
    Identical(ValueRef),
}

#[derive(Debug, Clone)]
pub enum List<T> {
    Pair { rest: Rc<List<T>>, first: T },
    Empty
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
