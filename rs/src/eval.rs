use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use value::{Value, ValueRef};

#[derive(Debug)]
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
        formal_types: Vec<Expr>,
        body: Rc<Expr>
    },
    Call {
        op: Box<Expr>,
        args: Vec<Expr>
    }
}

pub fn analyze(v: &ValueRef) -> Expr {
    match **v {
        Value::Symbol(..) => Expr::Id(v.clone()),
        Value::List(ref ls) => {
            let mut it = ls.iter().peekable();
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
                        "fn" => {
                            let name = it.peek()
                                .and_then(|v| (*v).name())
                                .map(ToString::to_string);
                            if name.is_some() {
                                it.next();
                            }
                            if let Value::List(ref lss) = **it.next().unwrap() {
                                Expr::Fn {
                                    name: name,
                                    formal_names: lss.iter()
                                        .map(|v| v.name().unwrap().to_string())
                                        .collect(),
                                    formal_types: vec![],
                                    body: Rc::new(analyze(it.next().unwrap()))
                                }
                            } else {
                                panic!()
                            }
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
        _ => Expr::Const(v.clone())
    }
}

pub struct Interpreter {
    env: Rc<RefCell<Env>>,
    envstack: Vec<Rc<RefCell<Env>>>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Rc::new(RefCell::new(Env {
                bindings: HashMap::new(),
                parent: None
            })),
            envstack: vec![]
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

            Expr::Fn { ref name,
                       ref formal_names, ref formal_types,
                       ref body } =>
                Rc::new(Value::Fn {
                    name: name.clone().unwrap_or("fn".to_string()),
                    formal_names: formal_names.clone(),
                    formal_types: formal_types.iter().map(|t| self.eval(t))
                        .collect(),
                    body: body.clone(),
                    env: self.env.clone()
                }),
                
            Expr::Call { ref op, ref args } => {
                let evop = self.eval(op);
                let evargs: Vec<ValueRef> = args.iter().map(|e| self.eval(e))
                    .collect();
                self.call(evop, evargs)
            }
        }
    }

    fn load(&self, sym: &Value) -> ValueRef {
        match *sym {
            Value::Symbol(None, ref name) =>
                self.env.borrow_mut().lookup(name).unwrap().clone(),
            _ => panic!()
        }
    }

    fn store(&mut self, name: &str, val: ValueRef) -> ValueRef {
        self.env.borrow_mut().extend(name, val);
        Rc::new(Value::Tuple(vec![]))
    }

    fn call(&mut self, op: ValueRef, args: Vec<ValueRef>) -> ValueRef {
        if let Value::Fn { ref formal_names, ref body, ref env, .. } = *op {
            // Store the old env:
            self.envstack.push(self.env.clone());
            // Make an env that extends that of the closure:
            self.env = Rc::new(RefCell::new(Env::new(env)));
            // Extend the env with the args:
            for (k, v) in formal_names.iter().zip(args.into_iter()) {
                self.store(k, v);
            }
            // Eval body in the extended env:
            let res = self.eval(body);
            // Restore previous env:
            self.env = self.envstack.pop().unwrap();
            res
        } else {
            panic!()
        }
    }
}

#[derive(Debug)]
pub struct Env {
    bindings: HashMap<String, ValueRef>,
    parent: Option<Rc<RefCell<Env>>>
}

impl Env {
    fn new(parent: &Rc<RefCell<Env>>) -> Env {
        Env {
            bindings: HashMap::new(),
            parent: Some(parent.clone())
        }
    }
    
    fn lookup(&self, k: &str) -> Option<ValueRef> {
        if let Some(v) = self.bindings.get(k) {
            Some(v.clone())
        } else if let Some(ref penv) = self.parent {
            penv.borrow().lookup(k)
        } else {
            None
        }
    }

    fn extend(&mut self, k: &str, v: ValueRef) {
        self.bindings.insert(k.to_string(), v);
    }
}
