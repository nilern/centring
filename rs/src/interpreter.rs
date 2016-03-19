use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::repeat;
use std::cmp::min;
use std::cmp::Ordering;

use value::{Value, ValueRef, TypeMatcher, List};
use environment::{Environment, EnvRef};
use builtins;
use builtins::NativeFnCode;

pub struct Interpreter {
    env: EnvRef,
    envstack: Vec<EnvRef>,
    mod_registry: HashMap<String, EnvRef>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut itp = Interpreter {
            env: Rc::new(RefCell::new(Environment::Mod {
                bindings: HashMap::new(),
                refers: HashMap::new(),
                aliases: HashMap::new(),
                non_macros: HashSet::new(),
                name: "user.repl".to_string()
            })),
            envstack: vec![],
            mod_registry: HashMap::new()
        };
        
        let any = Rc::new(Value::AbstractType {
            name: Rc::new(Value::Symbol(
                Some("centring.lang".to_string()), "Any".to_string())),
            supertyp: None
        });
        let ctr_string = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "String".to_string())),
            supertyp: Some(any.clone())
        });
        let int_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "Int".to_string())),
            supertyp: Some(any.clone())
        });
        let bool_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "Bool".to_string())),
            supertyp: Some(any.clone())
        });
        let char_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "Char".to_string())),
            supertyp: Some(any.clone())
        });
        let symbol_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "Symbol".to_string())),
            supertyp: Some(any.clone())
        });
        
        let tuple_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "Tuple".to_string())),
            supertyp: Some(any.clone())
        });
        let list_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "List".to_string())),
            supertyp: Some(any.clone())
        });
        let elist_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "List.Empty".to_string())),
            supertyp: Some(list_type.clone())
        });
        let plist_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "List.Pair".to_string())),
            supertyp: Some(list_type.clone())
        });
        
        let type_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "Type".to_string())),
            supertyp: Some(any.clone())
        });
        let builtin_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "BuiltInType".to_string())),
            supertyp: Some(type_type.clone())
        });

        let fn_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "Fn".to_string())),
            supertyp: Some(any.clone())
        });
        let natfn_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "NativeFn".to_string())),
            supertyp: Some(any.clone())
        });
        let multifn_type = Rc::new(Value::BuiltInType {
            name: Rc::new(Value::Symbol(Some("centring.lang".to_string()),
                                        "MultiFn".to_string())),
            supertyp: Some(any.clone())
        });
        
        itp.store_global("centring.lang", "Any", any.clone());
        itp.store_global("centring.lang", "String", ctr_string.clone());
        itp.store_global("centring.lang", "Int", int_type.clone());
        itp.store_global("centring.lang", "Bool", bool_type.clone());
        itp.store_global("centring.lang", "Char", char_type.clone());
        itp.store_global("centring.lang", "Symbol", symbol_type.clone());
        
        itp.store_global("centring.lang", "Tuple", tuple_type.clone());
        itp.store_global("centring.lang", "List", list_type.clone());
        itp.store_global("centring.lang", "List.Empty", elist_type.clone());
        itp.store_global("centring.lang", "List.Pair", plist_type.clone());
        
        itp.store_global("centring.lang", "Type", type_type.clone());
        itp.store_global("centring.lang", "BuiltInType", builtin_type.clone());
        
        itp.store_global("centring.lang", "Fn", fn_type.clone());
        itp.store_global("centring.lang", "NativeFn", natfn_type.clone());
        itp.store_global("centring.lang", "MultiFn", multifn_type.clone());

        itp.store_global("centring.lang", "set-module!",
                         Rc::new(Value::NativeFn {
                             name: "set-module!".to_string(),
                             formal_types: vec![
                                 TypeMatcher::Isa(ctr_string.clone())],
                             vararg_type: None,
                             code: builtins::set_module
                         }));
        itp.store_global("centring.lang", "require",
                         Rc::new(Value::NativeFn {
                             name: "require".to_string(),
                             formal_types: vec![
                                 TypeMatcher::Isa(ctr_string.clone()),
                                 TypeMatcher::Isa(ctr_string.clone())],
                             vararg_type: None,
                             code: builtins::require
                         }));
        itp.store_global("centring.lang", "refer",
                         Rc::new(Value::NativeFn {
                             name: "refer".to_string(),
                             formal_types: vec![
                                 TypeMatcher::Isa(ctr_string.clone())],
                             vararg_type: Some(
                                 TypeMatcher::Isa(ctr_string.clone())),
                             code: builtins::refer
                         }));
        itp.store_global("centring.lang", "record-type",
                         Rc::new(Value::NativeFn {
                             name: "record-type".to_string(),
                             formal_types: vec![],
                             vararg_type: None,
                             code: builtins::record_type
                         }));
        itp.store_global("centring.lang", "abstract-type",
                         Rc::new(Value::NativeFn {
                             name: "abstract-type".to_string(),
                             formal_types: vec![],
                             vararg_type: None,
                             code: builtins::abstract_type
                         }));
        itp.store_global("centring.lang", "type",
                         Rc::new(Value::NativeFn {
                             name: "type".to_string(),
                             formal_types: vec![
                                 TypeMatcher::Isa(any.clone())],
                             vararg_type: None,
                             code: builtins::type_of
                         }));
        itp.store_global("centring.lang", "supertype",
                         Rc::new(Value::NativeFn {
                             name: "supertype".to_string(),
                             formal_types: vec![
                                 TypeMatcher::Isa(type_type.clone())],
                             vararg_type: None,
                             code: builtins::supertype
                         }));
        itp.store_global("centring.lang", "isa?",
                         Rc::new(Value::NativeFn {
                             name: "isa?".to_string(),
                             formal_types: vec![
                                 TypeMatcher::Isa(type_type.clone()),
                                 TypeMatcher::Isa(any.clone())],
                             vararg_type: None,
                             code: builtins::isa
                         }));
        itp.store_global("centring.lang", "=",
                         Rc::new(Value::NativeFn {
                             name: "=".to_string(),
                             formal_types: vec![
                                 TypeMatcher::Isa(any.clone()),
                                 TypeMatcher::Isa(any.clone())],
                             vararg_type: None,
                             code: builtins::egal
                         }));

        let mathops: [(&str, NativeFnCode); 4] =
            [("+", builtins::add_2i),
             ("-", builtins::sub_2i),
             ("*", builtins::mul_2i),
             ("<", builtins::lt_2i)];
        for &(name, code) in mathops.into_iter() {
            itp.store_global("centring.lang", name, Rc::new(Value::NativeFn {
                name: name.to_string(),
                formal_types: vec![TypeMatcher::Isa(int_type.clone()),
                                   TypeMatcher::Isa(int_type.clone())],
                vararg_type: None,
                code: code
            }));
        }
        
        itp.store_global("centring.lang", "prepend", Rc::new(Value::NativeFn {
            name: "prepend".to_string(),
            formal_types: vec![TypeMatcher::Isa(any.clone()),
                               TypeMatcher::Isa(list_type.clone())],
            vararg_type: None,
            code: builtins::prepend_ls
        }));
        itp.store_global("centring.lang", ".left", Rc::new(Value::NativeFn {
            name: ".left".to_string(),
            formal_types: vec![TypeMatcher::Isa(plist_type.clone())],
            vararg_type: None,
            code: builtins::fld_left_pair
        }));
        itp.store_global("centring.lang", ".right", Rc::new(Value::NativeFn {
            name: "prepend".to_string(),
            formal_types: vec![TypeMatcher::Isa(plist_type.clone())],
            vararg_type: None,
            code: builtins::fld_right_pair
        }));
        itp.store_global("centring.lang", "apply", Rc::new(Value::NativeFn {
            name: "apply".to_string(),
            formal_types: vec![TypeMatcher::Isa(any.clone())],
            vararg_type: Some(TypeMatcher::Isa(any.clone())),
            code: builtins::apply
        }));
        itp.store_global("centring.lang", "load", Rc::new(Value::NativeFn {
            name: "load".to_string(),
            formal_types: vec![TypeMatcher::Isa(ctr_string.clone())],
            vararg_type: None,
            code: builtins::load
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
            Value::Symbol(None, ref name) =>
                self.env.borrow().lookup_macro(name),
            Value::Symbol(Some(ref mod_name), ref name) =>
                if let Some(ref md) = self.mod_registry.get(mod_name) {
                    md.borrow().lookup_macro(name)
                } else {
                    None
                },
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
            Value::Int(_) => self.load_global("centring.lang", "Int").unwrap(),
            Value::Bool(_) => self.load_global("centring.lang", "Bool").unwrap(),
            Value::Char(_) => self.load_global("centring.lang", "Char").unwrap(),
            Value::Symbol(..) =>
                self.load_global("centring.lang", "Symbol").unwrap(),

            Value::Tuple(_) =>
                self.load_global("centring.lang", "Tuple").unwrap(),
            Value::List(List::Empty) =>
                self.load_global("centring.lang", "List.Empty").unwrap(),
            Value::List(List::Pair { .. }) =>
                self.load_global("centring.lang", "List.Pair").unwrap(),
            Value::String(_) =>
                self.load_global("centring.lang", "String").unwrap(),

            Value::BuiltInType { .. } =>
                self.load_global("centring.lang", "BuiltInType").unwrap(),

            Value::Fn { .. } =>
                self.load_global("centring.lang", "Fn").unwrap(),
            Value::NativeFn { .. } =>
                self.load_global("centring.lang", "NativeFn").unwrap(),
            Value::MultiFn { .. } =>
                self.load_global("centring.lang", "MultiFn").unwrap(),
            
            _ => panic!("{} has an unknown type", val)
        }
    }

    pub fn isa(&self, formal_type: ValueRef, arg: ValueRef) -> bool {
        self.type_dist(formal_type, self.type_of(arg.as_ref())).is_some()
    }

    pub fn supertype(&self, typ: &Value) -> Option<ValueRef> {
        match *typ {
            Value::RecordType { ref supertyp, .. } => supertyp.clone(),
            Value::SingletonType { ref supertyp, .. } => supertyp.clone(),
            Value::AbstractType { ref supertyp, .. } => supertyp.clone(),
            Value::BuiltInType { ref supertyp, .. } => supertyp.clone(),
            _ => panic!()
        }
    }

    fn type_dist(&self, formal_type: ValueRef, arg_type: ValueRef)
                     -> Option<usize> {
        let faddr = formal_type.as_ref() as *const Value;
        let mut typ = arg_type;
        let mut dist = Some(0);
        loop {
            if typ.as_ref() as *const Value == faddr {
                return dist;
            }
            match self.supertype(typ.as_ref()) {
                None => return None,
                Some(st) => {
                    typ = st;
                    dist = dist.map(|d| d + 1);
                }
            }
        }
    }

// Call

    pub fn call(&mut self, op: ValueRef, mut args: Vec<ValueRef>) -> ValueRef {
        let mut varvals = vec![];
        // Check argument counts and types, split off values for vararg:
        match *op {
            Value::Fn { ref formal_types, ref vararg_type, .. } |
            Value::NativeFn { ref formal_types, ref vararg_type, .. } => {
                let formalc = formal_types.len();
                let argc = args.len();
                // Too few args?
                if formalc > argc {
                    panic!()
                }
                // Too many args?
                if formalc < argc {
                    // Vararg can still handle it:
                    if vararg_type.is_some() {
                        varvals = args.split_off(formalc);
                    } else {
                        panic!()
                    }
                }
                // Check types of positionals:
                for (tm, v) in formal_types.iter().zip(args.iter()) {
                    if self.arg_dist(tm, v.clone()).is_none() {
                        panic!()
                    }
                }
                // Check the types of vararg contents:
                if let Some(ref tm) = *vararg_type {
                    for v in varvals.iter() {
                        if self.arg_dist(tm, v.clone()).is_none() {
                            panic!()
                        }
                    }
                }
            },
            Value::MultiFn { ref methods, .. } => {
                let meths = methods.borrow();
                let mut match_tups: Vec<_> = meths.iter().filter_map(
                    |(k, meth)| {
                        let ddist = self.dispatch_dist(&k.0, &k.1, &args, true);
                        if ddist >= 0 {
                            Some((ddist, k.0.clone(), k.1.clone(), meth))
                        } else {
                            None
                        }}).collect();
                // No method for this at all?
                if match_tups.is_empty() {
                    panic!()
                }
                let mtup = if match_tups.len() == 1 {
                    // One applicable method:
                    &match_tups[0]
                } else {
                    // Several applicable methods:
                    match_tups.sort_by(dispatch_cmp);
                    // Ambiguity?
                    if dispatch_eq(&match_tups[0], &match_tups[1]) {
                        panic!()
                    }
                    &match_tups[0]
                };
                // Split off vararg contents:
                if mtup.2.is_some() {
                    varvals = args.split_off(mtup.1.len());
                }
                return self.call_unchecked(mtup.3.clone(), args, varvals);
            },
            _ => { }
        }
        self.call_unchecked(op, args, varvals)
    }

    fn call_unchecked(&mut self, op: ValueRef, args: Vec<ValueRef>,
                      varvals: Vec<ValueRef>) -> ValueRef {
        match *op {
            Value::Fn { ref formal_names, ref vararg_name,
                        ref body, ref env, .. } => {
                // Store the old env:
                self.envstack.push(self.env.clone());
                // Make an env that extends that of the closure:
                self.env = Rc::new(RefCell::new(Environment::new(env)));
                // Extend the env with the args:
                for (n, v) in formal_names.iter().zip(args.into_iter()) {
                    self.store_local(n, v);
                }
                // Fill vararg if needed:
                if let Some(ref vname) = *vararg_name {
                    self.store_local(vname, Rc::new(Value::Tuple(varvals)));
                }        
                // Eval body in the extended env:
                let res = self.eval(body);
                // Restore previous env:
                self.env = self.envstack.pop().unwrap();
                // Return value:
                res
            },
            Value::NativeFn { ref code, .. } => code(self, args, varvals),
            // FIXME: use the MultiFn call(t :RecordType & field-vals) for this:
            Value::RecordType { .. } => self.create_record(op.clone(), args),
            _ => panic!()
        }
    }

    fn arg_dist(&self, matcher: &TypeMatcher, arg: ValueRef) -> Option<usize> {
        match *matcher {
            TypeMatcher::Isa(ref typ) =>
                self.type_dist(typ.clone(), self.type_of(arg.as_ref())),
            TypeMatcher::Identical(ref typ) =>
                if &**typ as *const _ == &*arg as *const _ {
                    Some(0)
                } else {
                    None
                }
        }
    }

    fn dispatch_dist(&self, formal_tms: &Vec<TypeMatcher>,
                     vararg_tm: &Option<TypeMatcher>,
                     argv: &Vec<ValueRef>, short_circ: bool) -> isize {
        let mut ddist = 0;
        let mut args = argv.iter().peekable();
        let mut tms = formal_tms.iter().peekable();
        loop {
            if args.peek().is_none() { // Out of args:
                if tms.peek().is_none() { // Matched exactly, we are done:
                    return ddist;
                } else if short_circ { // Too few args!:
                    return -1;
                } else { // Too few args, here in detail:
                    return combine_dists(ddist, -(tms.count() as isize));
                }
            } else { // Args left:
                if tms.peek().is_none() { // But no formals:
                    if let Some(ref tm) = *vararg_tm { // Vararg is utilized:
                        return combine_dists(
                            ddist,
                            self.dispatch_dist(
                                // Expand to the missing args:
                                &repeat(tm.clone())
                                    .take(argv.len() - formal_tms.len())
                                    .collect(),
                                // So that no further expansion takes place:
                                &None,
                                &args.map(Clone::clone).collect(),
                                short_circ));
                    } else if short_circ { // Too many args!:
                        return -1;
                    } else { // Too many args, here in detail:
                        return combine_dists(ddist, -(args.count() as isize));
                    }
                }
                // Both left, we go on:
                let v = args.next().unwrap(); // this is safe since we peeked
                let tm = tms.next().unwrap(); // this also
                let arg_dist = self.arg_dist(tm, v.clone())
                    .map(|u| u as isize).unwrap_or(-1);
                ddist = combine_dists(ddist, arg_dist);
            }
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

    pub fn require(&mut self, mod_name: &str, alias: String) {
        let md = self.get_mod(mod_name).unwrap();
        self.current_mod().borrow_mut().add_alias(md, alias);
    }

    pub fn refer(&mut self, mod_name: &str, names: Vec<String>) {
        let md = self.get_mod(mod_name).unwrap();
        self.current_mod().borrow_mut().refer(md, names);
    }
}

fn combine_dists(d1: isize, d2: isize) -> isize {
    if (d1 < 0) == (d2 < 0) {
        d1 + d2
    } else {
        min(d1, d2)
    }
}

fn dispatch_cmp(&(ref dd1, _, ref vtm1, _):
                &(isize, Vec<TypeMatcher>, Option<TypeMatcher>, &ValueRef),
                &(ref dd2, _, ref vtm2, _):
                &(isize, Vec<TypeMatcher>, Option<TypeMatcher>, &ValueRef))
                -> Ordering {
    match dd1.cmp(&dd2) {
        Ordering::Equal => match (vtm1, vtm2) {
            (&Some(_), &None) => Ordering::Greater,
            (&None, &Some(_)) => Ordering::Less,
            _ => Ordering::Equal
        },
        ord => ord
    }
}

fn dispatch_eq(&(ref dd1, _, ref vtm1, _):
               &(isize, Vec<TypeMatcher>, Option<TypeMatcher>, &ValueRef),
               &(ref dd2, _, ref vtm2, _):
               &(isize, Vec<TypeMatcher>, Option<TypeMatcher>, &ValueRef))
               -> bool {
    dd1 == dd2 && vtm1.is_some() == vtm2.is_some()
}
