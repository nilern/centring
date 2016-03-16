use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use value::ValueRef;

pub type EnvRef = Rc<RefCell<Environment>>;

enum Environment {
    Env(Env),
    Mod(Mod)
}

struct Env {
    bindings: HashMap<String, ValueRef>,
    parent: EnvRef
}

struct Mod {
    name: String,
    bindings: HashMap<String, ValueRef>,
    aliases: HashMap<String, EnvRef>,
    refers: HashMap<String, ValueRef>
}
