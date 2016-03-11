use value::{Value, ValueRef, List};
use interpreter::Interpreter;
use eval::Expr;
use std::rc::Rc;

fn parse_formals(formals: ValueRef) -> (Vec<String>, Option<String>) {
    if let Value::List(ref ls) = *formals {
        let mut formal_names = vec![];
        let mut vararg_name = None;
        let mut iter = ls.iter();
        let mut va_expected = false;
        while let Some(formal) = iter.next() {
            match **formal {
                Value::Symbol(None, ref name) =>
                    if let "&" = name.as_ref() {
                        va_expected = true;
                    } else if !va_expected {
                        formal_names.push(name.clone());
                    } else {
                        vararg_name = Some(name.clone());
                    },
                _ => panic!()
            }
        }
        if va_expected && vararg_name.is_none() {
            panic!()
        }
        (formal_names, vararg_name)
    } else {
        panic!()
    }
}

pub fn shallow_analyze(sexpr: ValueRef) -> Expr {
    match *sexpr {
        Value::Symbol(Some(ref mod_name), ref name) =>
            Expr::Global(mod_name.clone(), name.clone()),
        Value::Symbol(None, ref name) => Expr::Local(name.clone()),
        Value::List(ref ls) => {
            let mut it = ls.iter().peekable();
            let op = it.next().unwrap();
            if let Value::Symbol(Some(ref mod_name), ref name) = **op {
                if mod_name == "centring.sf" {
                    return match name.as_ref() {
                        "quote" => Expr::Const(it.next().unwrap().clone()),
                        "def" => match **it.next().unwrap() {
                            Value::Symbol(None, ref name) =>
                                Expr::Def {
                                    name: name.clone(),
                                    val: Box::new(Expr::Const(
                                        it.next().unwrap().clone()))
                                },
                            _ => panic!()
                        },
                        "at-expansion" =>
                            Expr::AtExpansion(
                                Box::new(
                                    Expr::Const(it.next().unwrap().clone()))),
                        "do" => Expr::Do(
                            it.map(|v| Expr::Const(v.clone())).collect()),
                        "if" => Expr::If {
                            cond: Box::new(Expr::Const(
                                it.next().unwrap().clone())),
                            then: Box::new(Expr::Const(
                                it.next().unwrap().clone())),
                            els: Box::new(Expr::Const(
                                it.next().unwrap().clone()))
                        },
                        "fn" => {
                            let name = it.peek()
                                .and_then(|v| (*v).name())
                                .map(ToString::to_string);
                            if name.is_some() {
                                it.next();
                            }
                            let (formal_names, vararg_name) =
                                parse_formals(it.next().unwrap().clone());
                            Expr::Fn {
                                name: name,
                                formal_names: formal_names,
                                vararg_name: vararg_name,
                                formal_types: vec![],
                                vararg_type: None,
                                body: Rc::new(Expr::Const(
                                    it.next().unwrap().clone()))
                            }
                        },
                        "macro" =>
                            Expr::Macro(
                                Box::new(Expr::Const(
                                    it.next().unwrap().clone()))),
                        _ => Expr::Call {
                            op: Box::new(Expr::Const(op.clone())),
                            args: it.map(|v| Expr::Const(v.clone())).collect()
                        }
                    }
                }
            }
            Expr::Call {
                op: Box::new(Expr::Const(op.clone())),
                args: it.map(|v| Expr::Const(v.clone())).collect()
            }
        },
        _ => Expr::Const(sexpr.clone())
    }
}

impl Interpreter {
    pub fn expand_all(&mut self, sexpr: ValueRef) -> Expr {
        let expr = shallow_analyze(self.expand(sexpr));
        match expr {
            Expr::Const(_) | Expr::Local(_) | Expr::Global(..) => expr,
            Expr::Def { name, val } => {
                self.shadow_macro(name.clone());
                let res = Expr::Def {
                    name: name.clone(),
                    val: Box::new(self.expand_all(val.get_const().unwrap()))
                };
                self.allow_macro(&name);
                res
            },
            Expr::Do(stmts) =>
                return Expr::Do(stmts.into_iter().map(
                    |e|
                    if let Expr::Const(cse) = e {
                        self.expand_all(cse)
                    } else {
                        panic!()
                    }
                ).collect()),
            Expr::If { cond, then, els } =>
                Expr::If {
                    cond: Box::new(self.expand_all(cond.get_const().unwrap())),
                    then: Box::new(self.expand_all(then.get_const().unwrap())),
                    els: Box::new(self.expand_all(els.get_const().unwrap()))
                },
            Expr::Fn { name, formal_names, vararg_name, formal_types, vararg_type,
                       body } => {
                for name in formal_names.iter() {
                    self.shadow_macro(name.clone());
                }
                let res = Expr::Fn {
                    name: name.clone(),
                    formal_names: formal_names.clone(),
                    vararg_name: vararg_name.clone(),
                    formal_types: formal_types,
                    vararg_type: vararg_type,
                    body: Rc::new(self.expand_all(body.get_const().unwrap()))
                };
                for name in formal_names.iter() {
                    self.allow_macro(&name);
                }
                res
            },
            Expr::Call { op, args } =>
                Expr::Call {
                    op: Box::new(self.expand_all(op.get_const().unwrap())),
                    args: args.into_iter().map(
                        |e|
                        if let Expr::Const(cse) = e {
                            self.expand_all(cse)
                        } else {
                            panic!()
                        }
                    ).collect()
                },
            Expr::Macro(expf) =>
                Expr::Macro(Box::new(self.expand_all(expf.get_const().unwrap()))),
            Expr::AtExpansion(e) => {
                let expr = self.expand_all(e.get_const().unwrap());
                Expr::Const(self.eval(&expr))
            }
        }
    }
    
    pub fn expand(&mut self, sexpr: ValueRef) -> ValueRef {
        let mut se = sexpr;
        while let (expansion, true) = self.expand_once(se.clone()) {
            se = expansion;
        }
        se
    }
    
    pub fn expand_once(&mut self, sexpr: ValueRef) -> (ValueRef, bool) {
        // We only bother with lists...
        if let Value::List(ref ls @ List::Pair { .. }) = *sexpr {
            let mut it = ls.iter();
            // ...whose head...
            if let Some(ref sym) = it.next() {
                // ...is a symbol
                if sym.is_symbol() {
                    // ...that names a macro:
                    if let Some(mac) = self.load_macro(sym) {
                        if let Value::Macro(ref op) = *mac {
                            return (self.call(op.clone(),
                                              it.map(Clone::clone).collect()),
                                    true)
                        }
                    }
                }
            }
        }
        (sexpr, false)
    }
}
