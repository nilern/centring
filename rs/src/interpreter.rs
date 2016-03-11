use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::cell::RefCell;
use std::cmp::Ordering;

use value::{Value, ValueRef};
use environment::{Environment, EnvRef};
use builtins::{set_module, record_type, abstract_type};

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
        itp.store_global("centring.lang", "set-module!",
                         Rc::new(Value::NativeFn {
                             name: "set-module!".to_string(),
                             formal_types: vec![],
                             code: set_module
                         }));
        itp.store_global("centring.lang", "record-type",
                         Rc::new(Value::NativeFn {
                             name: "record-type".to_string(),
                             formal_types: vec![],
                             code: record_type
                         }));
        itp.store_global("centring.lang", "abstract-type",
                         Rc::new(Value::NativeFn {
                             name: "abstract-type".to_string(),
                             formal_types: vec![],
                             code: abstract_type
                         }));
        itp.store_global("centring.lang", "Any",
                         Rc::new(Value::AbstractType {
                             name: Rc::new(Value::Symbol(
                                 Some("centring.lang".to_string()),
                                 "Any".to_string())),
                             supertyp: None
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

    pub fn current_mod_name(&self) -> Option<String> {
        if let Environment::Mod { ref name, .. }  = *self.current_mod().borrow() {
            Some(name.to_string())
        } else {
            None
        }
    }

    pub fn lexical_mod(&self) -> Option<EnvRef> {
        Environment::lexical_mod(self.env.clone())
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

    pub fn load_local(&self, name: &str) -> Option<ValueRef> {
        self.env.borrow().lookup(name)
    }

    pub fn store_local(&mut self, name: &str, val: ValueRef) -> ValueRef {
        self.env.borrow_mut().extend(name.to_string(), val);
        Rc::new(Value::Tuple(vec![]))
    }

    pub fn load_global(&self, mod_name: &str, name: &str) -> Option<ValueRef> {
        if let Some(ref md) = self.lexical_mod() {
            if let Environment::Mod { ref aliases, .. } = *md.borrow() {
                if let Some(ref amd) = aliases.get(mod_name) {
                    return amd.borrow().lookup(name)
                }
            }
        }
        if let Some(ref md) = self.mod_registry.get(mod_name) {
            md.borrow().lookup(name)
        } else {
            None
        }
    }

    pub fn store_global(&mut self, mod_name: &str, name: &str, val: ValueRef) {
        if let Some(ref md) = self.mod_registry.get(mod_name) {
            md.borrow_mut().extend(name.to_string(), val);
            return;
        }
        let md = self.fresh_mod(mod_name.to_string());
        md.borrow_mut().extend(name.to_string(), val);
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

    pub fn create_record_type(&self, name: String, supertyp: Option<ValueRef>,
                              field_names: Vec<String>) -> ValueRef {
        Rc::new(Value::RecordType {
            name: Rc::new(Value::Symbol(self.current_mod_name(), name)),
            field_names: field_names,
            supertyp: supertyp
        })
    }

    pub fn create_abstract_type(&self, name: String, supertyp: Option<ValueRef>)
                                -> ValueRef {
        Rc::new(Value::AbstractType {
            name: Rc::new(Value::Symbol(self.current_mod_name(), name)),
            supertyp: supertyp
        })
    }
    
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
                            self.store_local(k, v);
                        }
                        if let Some(ref vname) = *vararg_name {
                            self.store_local(vname, Rc::new(Value::Tuple(vec![])));
                        }
                    },
                    Ordering::Less => {
                        // Too many args, but a vararg will work:
                        if let Some(ref vname) = *vararg_name {
                            let varvals = args.split_off(formalc);
                            for (k, v) in
                                formal_names.iter().zip(args.into_iter())
                            {
                                self.store_local(k, v);
                            }
                            self.store_local(vname,
                                             Rc::new(Value::Tuple(varvals)));
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

// Module Management

    pub fn fresh_mod(&mut self, mod_name: String) -> EnvRef {
        let md = Rc::new(RefCell::new(Environment::new_mod(mod_name.clone())));
        self.mod_registry.insert(mod_name, md.clone());
        md
    }

    pub fn get_mod(&self, mod_name: &str) -> Option<EnvRef> {
        self.mod_registry.get(mod_name).map(Clone::clone)
    }

    pub fn set_mod(&mut self, mod_name: &str) {
        if self.envstack.is_empty() {
            self.envstack.clear();
            self.env = if let Some(ref md) = self.get_mod(mod_name) {
                md.clone()
            } else {
                self.fresh_mod(mod_name.to_string())
            };
        } else {
            panic!()
        }
    }
}
