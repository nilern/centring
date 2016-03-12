use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use value::ValueRef;

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

    pub fn new_mod(name: String) -> Environment {
        Environment::Mod {
            bindings: HashMap::new(),
            refers: HashMap::new(),
            aliases: HashMap::new(),
            non_macros: HashSet::new(),
            name: name
        }
    }

    pub fn lexical_mod(env: EnvRef) -> Option<EnvRef> {
        let mut res = Some(env);
        while let Some(env) = res.clone() {
            match *env.borrow() {
                Environment::Mod { .. } => return res,
                Environment::Env { ref parent, .. } => res = parent.clone()
            }
        }
        res
    }
    
    pub fn lookup(&self, k: &str) -> Option<ValueRef> {
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

    pub fn extend(&mut self, k: String, v: ValueRef) {
        match *self {
            Environment::Env { ref mut bindings, .. } => bindings.insert(k, v),
            Environment::Mod { ref mut bindings, .. } => bindings.insert(k, v)
        };
    }
    
    pub fn lookup_macro(&self, name: &str) -> Option<ValueRef> {
        // TODO: also ignore shadowed macros
        self.lookup(name).and_then(
            |v| if v.is_macro() {
                Some(v)
            } else {
                None
            })
    }

    pub fn shadow_macro(&mut self, name: String) {
        match *self {
            Environment::Env { ref mut non_macros, .. } =>
                non_macros.insert(name),
            Environment::Mod { ref mut non_macros, .. } =>
                non_macros.insert(name),
        };
    }

    pub fn allow_macro(&mut self, name: &str) {
        match *self {
            Environment::Env { ref mut non_macros, .. } =>
                non_macros.remove(name),
            Environment::Mod { ref mut non_macros, .. } =>
                non_macros.remove(name),
        };
    }

    pub fn add_alias(&mut self, md: EnvRef, alias: String) {
        if let Environment::Mod { ref mut aliases, .. } = *self {
            aliases.insert(alias, md);
        } else {
            panic!();
        }
    }

    pub fn refer(&mut self, md: EnvRef, names: Vec<String>) {
        if let Environment::Mod { ref mut refers, .. } = *self {
            for name in names {
                let val = md.borrow().lookup(&name).unwrap();
                refers.insert(name, val);
            }
        } else {
            panic!()
        }
    }
}
