use std::rc::Rc;

use value::{Value, ValueRef};
use interpreter::Interpreter;

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

impl Interpreter {
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
                    env: self.current_env().clone()
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
}
