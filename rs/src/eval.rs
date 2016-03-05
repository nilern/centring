use std::collections::HashMap;
use std::rc::Rc;

use value::{Value, ValueRef};

pub enum Expr {
    Const(ValueRef),

    Id(ValueRef),
    Def {
        name: String,
        val: Box<Expr>
    },
    
    Do(Vec<Expr>),
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>
    }
}

pub fn analyze(v: ValueRef) -> Expr {
    match *v {
        Value::Int(_) | Value::Bool(_) | Value::Char(_) => Expr::Const(v.clone()),

        Value::Symbol(..) => Expr::Id(v.clone()),

        Value::List(ref ls) => {
            let mut it = ls.iter();
            if let Value::Symbol(Some(ref mod_name), ref name) =
                **it.next().unwrap() {
                if mod_name == "centring.lang" {
                    match name.as_ref() {
                        "do" => Expr::Do(it.map(|v| analyze(v.clone())).collect()),
                        "def" => match **it.next().unwrap() {
                            Value::Symbol(None, ref name) =>
                                Expr::Def {
                                    name: name.clone(),
                                    val: Box::new(analyze(it.next()
                                                          .unwrap()
                                                          .clone()))
                                },
                            _ => panic!()
                        },
                        _ => panic!()
                    }
                } else {
                    panic!()
                }
            } else {
                panic!()
            }
        },  
        
        _ => panic!()
    }
}

pub struct Interpreter {
    env: HashMap<String, ValueRef>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: HashMap::new()
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> ValueRef {
        match *expr {
            Expr::Const(ref v) => v.clone(),

            Expr::Id(ref sym) => self.load(sym),
            Expr::Def { ref name, ref val } => {
                let v = self.eval(val);
                self.store(&name, v)
            },

            Expr::Do(ref es) =>
                if es.is_empty() {
                    Rc::new(Value::Tuple(vec![]))
                } else {
                    let mut res = None;
                    for e in es {
                        res = Some(self.eval(e))
                    }
                    res.unwrap()
                },
            Expr::If { ref cond, ref then, ref els } =>
                match *self.eval(cond) {
                    Value::Bool(true) => self.eval(then),
                    Value::Bool(false) => self.eval(els),
                    _ => panic!()
                }
        }
    }

    fn load(&self, sym: &Value) -> ValueRef {
        match *sym {
            Value::Symbol(_, ref name) => {
                if let Some(v) = self.env.get(name) {
                    v.clone()
                } else {
                    panic!()
                }
            },
            _ => panic!()
        }
    }

    fn store(&mut self, name: &str, val: ValueRef) -> ValueRef {
        self.env.insert(name.to_string(), val);
        Rc::new(Value::Tuple(vec![]))
    }
}
