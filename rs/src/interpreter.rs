use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::cell::RefCell;
use std::cmp::Ordering;

use value::{Value, ValueRef};
use environment::{Environment, EnvRef};

pub struct Interpreter {
    env: EnvRef,
    envstack: Vec<EnvRef>,
    mod_registry: HashMap<String, EnvRef>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut itp = Interpreter {
            env: Rc::new(RefCell::new(Environment::Env {
                bindings: HashMap::new(),
                parent: None,
                non_macros: HashSet::new(),
            })),
            envstack: vec![],
            mod_registry: HashMap::new()
        };
        itp.store_to_mod("centring.core", "Option.Some",
                         Rc::new(Value::RecordType {
                             name: Rc::new(Value::Symbol(
                                 Some("centring.core".to_string()),
                                 "Option.Some".to_string())),
                             field_names: vec!["v".to_string()],
                             supertyp: Rc::new(Value::Bool(false))
                         }));
        itp
    }

// Getters
    
    pub fn current_env(&self) -> &EnvRef {
        &self.env
    }

    pub fn current_mod(&self) -> &EnvRef {
        if self.envstack.is_empty() {
            &self.env
        } else {
            &self.envstack[0]
        }
    }

// Allocation

    pub fn create_record(&self, typ: ValueRef, args: Vec<ValueRef>) -> ValueRef {
        if let Value::RecordType { ref field_names, .. } = *typ {
            if field_names.len() == args.len() {
                return Rc::new(Value::Record {
                    typ: typ.clone(),
                    vals: args
                })
            }
        }
        panic!()
    }

// Loads and Stores

    pub fn load(&self, sym: &Value) -> Option<ValueRef> {
        match *sym {
            Value::Symbol(None, ref name) => self.env.borrow().lookup(name),
            Value::Symbol(Some(ref mod_name), ref name) =>
                self.load_from_mod(mod_name, name),
            _ => panic!()
        }
    }

    pub fn store(&mut self, name: &str, val: ValueRef) -> ValueRef {
        self.env.borrow_mut().extend(name.to_string(), val);
        Rc::new(Value::Tuple(vec![]))
    }

    pub fn load_from_mod(&self, mod_name: &str, name: &str) -> Option<ValueRef> {
        if let Environment::Mod { ref aliases, .. } = *self.current_mod().borrow() {
            if let Some(ref md) = aliases.get(mod_name) {
                return md.borrow().lookup(name)
            }
        }
        if let Some(ref md) = self.mod_registry.get(mod_name) {
            md.borrow().lookup(name)
        } else {
            None
        }
    }

    pub fn store_to_mod(&mut self, mod_name: &str, name: &str, val: ValueRef) {
        if let Some(ref md) = self.mod_registry.get(mod_name) {
            md.borrow_mut().extend(name.to_string(), val);
            return;
        }
        let mut md = Environment::new_mod(mod_name.to_string());
        md.extend(name.to_string(), val);
        self.mod_registry.insert(mod_name.to_string(), Rc::new(RefCell::new(md)));
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

// Types
    
    pub fn type_of(&self, val: &Value) -> ValueRef {
        match *val {
            Value::Record { ref typ, .. } => typ.clone(),
            Value::Singleton { ref typ } => typ.clone(),
            _ => panic!()
        }
    }

// Call

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
            Value::RecordType { .. } => self.create_record(op.clone(), args),
            _ => panic!()
        }
    }
}
