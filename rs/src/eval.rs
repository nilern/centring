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
    },

    Fn {
        name: Option<String>,
        formal_names: Vec<String>,
        formal_types: Vec<ValueRef>,
        body: Box<Expr>
    },
    Call {
        op: Box<Expr>,
        args: Vec<Expr>
    }
}

pub fn analyze(v: &ValueRef) -> Expr {
    match **v {
        Value::Int(_) | Value::Bool(_) | Value::Char(_) => Expr::Const(v.clone()),

        Value::Symbol(..) => Expr::Id(v.clone()),

        Value::List(ref ls) => {
            let mut it = ls.iter();
            let op = it.next().unwrap();
            if let Value::Symbol(Some(ref mod_name), ref name) = **op {
                if mod_name == "centring.lang" {
                    return match name.as_ref() {
                        "quote" => Expr::Const(it.next().unwrap().clone()),
                        "def" => match **it.next().unwrap() {
                            Value::Symbol(None, ref name) =>
                                Expr::Def {
                                    name: name.clone(),
                                    val: Box::new(analyze(it.next()
                                                          .unwrap()))
                                },
                            _ => panic!()
                        },
                        "do" => Expr::Do(it.map(analyze).collect()),
                        "if" => Expr::If {
                            cond: Box::new(analyze(it.next().unwrap())),
                            then: Box::new(analyze(it.next().unwrap())),
                            els: Box::new(analyze(it.next().unwrap()))
                        },
                        _ => Expr::Call {
                            op: Box::new(analyze(op)),
                            args: it.map(analyze).collect()
                        }
                    }
                }
            }
            Expr::Call {
                op: Box::new(analyze(op)),
                args: it.map(analyze).collect()
            }
        },  
        
        _ => panic!()
    }
}

pub struct Interpreter {
    env: Env
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Env {
                bindings: HashMap::new(),
                parent: None
            }
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
                },
            _ => panic!()
        }
    }

    fn load(&self, sym: &Value) -> ValueRef {
        match *sym {
            Value::Symbol(None, ref name) =>
                self.env.lookup(name).unwrap().clone(),
            _ => panic!()
        }
    }

    fn store(&mut self, name: &str, val: ValueRef) -> ValueRef {
        self.env.extend(name, val);
        Rc::new(Value::Tuple(vec![]))
    }
}

#[derive(Debug)]
pub struct Env {
    bindings: HashMap<String, ValueRef>,
    parent: Option<Rc<Env>>
}

impl Env {
    fn lookup(&self, k: &str) -> Option<ValueRef> {
        if let Some(v) = self.bindings.get(k) {
            Some(v.clone())
        } else if let Some(ref penv) = self.parent {
            penv.lookup(k)
        } else {
            None
        }
    }

    fn extend(&mut self, k: &str, v: ValueRef) {
        self.bindings.insert(k.to_string(), v);
    }
}
