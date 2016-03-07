use std::collections::{HashMap, HashSet};
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
    },

    Macro(Box<Expr>),
    AtExpansion(Box<Expr>)
}

impl Expr {
    pub fn get_const(&self) -> Option<ValueRef> {
        match *self {
            Expr::Const(ref v) => Some(v.clone()),
            _ => None
        }
    }
}

pub struct Interpreter {
    env: EnvRef,
    envstack: Vec<EnvRef>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Rc::new(RefCell::new(Env {
                bindings: HashMap::new(),
                parent: None,
                non_macros: HashSet::new()
            })),
            envstack: vec![]
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> ValueRef {
        match *expr {
            Expr::Const(ref v) => v.clone(),

            Expr::Id(ref sym) => self.load(sym).unwrap(),
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
            Expr::Macro(ref expf) =>
                Rc::new(Value::Macro(self.eval(expf))),
                
            Expr::Call { ref op, ref args } => {
                let evop = self.eval(op);
                let evargs = args.iter().map(|e| self.eval(e)).collect();
                self.call(evop, evargs)
            },

            Expr::AtExpansion(_) => panic!()
        }
    }

    pub fn load(&self, sym: &Value) -> Option<ValueRef> {
        match *sym {
            Value::Symbol(None, ref name) =>
                self.env.borrow().lookup(name),
            Value::Symbol(Some(_), _) => None,
            _ => panic!()
        }
    }

    pub fn store(&mut self, name: &str, val: ValueRef) -> ValueRef {
        self.env.borrow_mut().extend(name, val);
        Rc::new(Value::Tuple(vec![]))
    }

    pub fn shadow_macro(&mut self, name: String) {
        self.env.borrow_mut().shadow_macro(name);
    }

    pub fn allow_macro(&mut self, name: &str) {
        self.env.borrow_mut().allow_macro(name);
    }

    pub fn call(&mut self, op: ValueRef, args: Vec<ValueRef>) -> ValueRef {
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

pub type EnvRef = Rc<RefCell<Env>>;

#[derive(Debug)]
pub struct Env {
    bindings: HashMap<String, ValueRef>,
    parent: Option<EnvRef>,
    non_macros: HashSet<String>
}

impl Env {
    pub fn new(parent: &EnvRef) -> Env {
        Env {
            bindings: HashMap::new(),
            parent: Some(parent.clone()),
            non_macros: HashSet::new()
        }
    }
    
    pub fn lookup(&self, k: &str) -> Option<ValueRef> {
        if let Some(v) = self.bindings.get(k) {
            Some(v.clone())
        } else if let Some(ref penv) = self.parent {
            penv.borrow().lookup(k)
        } else {
            None
        }
    }

    pub fn extend(&mut self, k: &str, v: ValueRef) {
        self.bindings.insert(k.to_string(), v);
    }

    pub fn shadow_macro(&mut self, name: String) {
        self.non_macros.insert(name);
    }

    pub fn allow_macro(&mut self, name: &str) {
        self.non_macros.remove(name);
    }
}
