use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::cell::RefCell;
use std::cmp::Ordering;

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
        vararg_name: Option<String>,
        formal_types: Vec<Expr>,
        vararg_type: Option<Box<Expr>>,
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
    envstack: Vec<EnvRef>,
    mod_registry: HashMap<String, EnvRef>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Rc::new(RefCell::new(Environment::Env {
                bindings: HashMap::new(),
                parent: None,
                non_macros: HashSet::new(),
            })),
            envstack: vec![],
            mod_registry: HashMap::new()
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

            Expr::Do(ref es) => {
                let mut res = Rc::new(Value::Tuple(vec![]));
                for e in es {
                    res = self.eval(e);
                }
                res
            },
            Expr::If { ref cond, ref then, ref els } =>
                match *self.eval(cond) {
                    Value::Bool(true) => self.eval(then),
                    Value::Bool(false) => self.eval(els),
                    _ => panic!()
                },

            Expr::Fn { ref name,
                       ref formal_names, ref vararg_name,
                       ref formal_types, ref vararg_type,
                       ref body } =>
                Rc::new(Value::Fn {
                    name: name.clone().unwrap_or("fn".to_string()),
                    formal_names: formal_names.clone(),
                    vararg_name: vararg_name.clone(),
                    formal_types: formal_types.iter().map(|t| self.eval(t))
                        .collect(),
                    vararg_type: vararg_type.as_ref().map(|t| self.eval(t)),
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
            Value::Symbol(None, ref name) => self.env.borrow().lookup(name),
            Value::Symbol(Some(_), _) => None,
            _ => panic!()
        }
    }

    pub fn store(&mut self, name: &str, val: ValueRef) -> ValueRef {
        self.env.borrow_mut().extend(name.to_string(), val);
        Rc::new(Value::Tuple(vec![]))
    }

    pub fn type_of(&self, val: &Value) -> ValueRef {
        match *val {
            Value::Record { ref typ, .. } => typ.clone(),
            Value::Singleton { ref typ } => typ.clone(),
            _ => panic!()
        }
    }

    pub fn load_macro(&self, sym: &Value) -> Option<ValueRef> {
        match *sym {
            Value::Symbol(None, ref name) => self.env.borrow().lookup_macro(name),
            Value::Symbol(Some(_), _) => None,
            _ => panic!()
        }
    }

    pub fn shadow_macro(&mut self, name: String) {
        self.env.borrow_mut().shadow_macro(name);
    }

    pub fn allow_macro(&mut self, name: &str) {
        self.env.borrow_mut().allow_macro(name);
    }

    pub fn call(&mut self, op: ValueRef, mut args: Vec<ValueRef>) -> ValueRef {
        match *op {
            Value::Fn { ref formal_names, ref vararg_name, ref body, ref env,
                        .. } => {
                // Store the old env:
                self.envstack.push(self.env.clone());
                // Make an env that extends that of the closure:
                self.env = Rc::new(RefCell::new(Environment::new(env)));
                // Extend the env with the args:
                // TODO: check arg counts and types
                let formalc = formal_names.len();
                match formalc.cmp(&args.len()) {
                    Ordering::Equal => {
                        for (k, v) in formal_names.iter().zip(args.into_iter()) {
                            self.store(k, v);
                        }
                        if let Some(ref vname) = *vararg_name {
                            self.store(vname, Rc::new(Value::Tuple(vec![])));
                        }
                    },
                    Ordering::Less => {
                        // Too many args, but a vararg will work:
                        if let Some(ref vname) = *vararg_name {
                            let varvals = args.split_off(formalc);
                            for (k, v) in
                                formal_names.iter().zip(args.into_iter())
                            {
                                self.store(k, v);
                            }
                            self.store(vname, Rc::new(Value::Tuple(varvals)));
                        } else {
                            panic!()
                        }
                    },
                    Ordering::Greater => // Too few args, no good in any case:
                        panic!()
                }
                // Eval body in the extended env:
                let res = self.eval(body);
                // Restore previous env:
                self.env = self.envstack.pop().unwrap();
                res
            },
            Value::NativeFn { ref code, .. } => code(self, args),
            _ => panic!()
        }
    }
}

pub type EnvRef = Rc<RefCell<Environment>>;

pub enum Environment {
    Env {
        bindings: HashMap<String, ValueRef>,
        parent: Option<EnvRef>,
        non_macros: HashSet<String>
    },
    Mod {
        bindings: HashMap<String, ValueRef>,
        refers: HashMap<String, ValueRef>,
        aliases: HashMap<String, EnvRef>,
        non_macros: HashSet<String>,
        name: String
    }
}

impl Environment {
    pub fn new(parent: &EnvRef) -> Environment {
        Environment::Env {
            bindings: HashMap::new(),
            parent: Some(parent.clone()),
            non_macros: HashSet::new()
        }
    }
    
    fn lookup(&self, k: &str) -> Option<ValueRef> {
        match *self {
            Environment::Env { ref bindings, ref parent, .. } =>
                if let Some(v) = bindings.get(k) {
                    Some(v.clone())
                } else if let Some(ref penv) = *parent {
                    penv.borrow().lookup(k)
                } else {
                    None
                },
            Environment::Mod { ref bindings, ref refers, .. } =>
                if let Some(v) = bindings.get(k) {
                    Some(v.clone())
                } else if let Some(v) = refers.get(k) {
                    Some(v.clone())
                } else {
                    None
                }
        }
    }

    fn extend(&mut self, k: String, v: ValueRef) {
        match *self {
            Environment::Env { ref mut bindings, .. } => bindings.insert(k, v),
            Environment::Mod { ref mut bindings, .. } => bindings.insert(k, v)
        };
    }
    
    fn lookup_macro(&self, name: &str) -> Option<ValueRef> {
        // TODO: also ignore shadowed macros
        self.lookup(name).and_then(
            |v| if v.is_macro() {
                Some(v)
            } else {
                None
            })
    }

    fn shadow_macro(&mut self, name: String) {
        match *self {
            Environment::Env { ref mut non_macros, .. } => non_macros.insert(name),
            Environment::Mod { ref mut non_macros, .. } => non_macros.insert(name),
        };
    }

    fn allow_macro(&mut self, name: &str) {
        match *self {
            Environment::Env { ref mut non_macros, .. } => non_macros.remove(name),
            Environment::Mod { ref mut non_macros, .. } => non_macros.remove(name),
        };
    }
}
