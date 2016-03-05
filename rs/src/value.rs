use std::rc::Rc;
use std::collections::HashMap;
use std::iter::FromIterator;

use eval::Interpreter;

pub type ValueRef = Rc<Value>;

#[derive(Debug)]
pub enum Value {
    Int(isize),
    Bool(bool),
    Char(char),
    Symbol(Option<String>, String),

    Tuple(Vec<ValueRef>),
    Array(Vec<ValueRef>),
    List(List<ValueRef>),

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

#[derive(Debug)]
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

struct ListIter<'a, T>(&'a List<T>) where T: 'a;

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
