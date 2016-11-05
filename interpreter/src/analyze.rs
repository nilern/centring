use interpreter::{Interpreter, CtrResult, CtrError};
use value::{ConcreteType, Any, ListPair, ListEmpty, Symbol, Def, Expr, Stmt, Ctrl, Do, Var, Const};
use refs::{Root, ValueHandle};
use primops;

use std::cmp::Ordering::Greater;

pub fn analyze(itp: &mut Interpreter, v: ValueHandle<Any>) -> CtrResult<Any> {
    typecase!(v, itp; {
        p: ListPair => {
            typecase!(p.first().borrow(), itp; {
                op: Symbol => {
                    let opstr = op.to_string();
                    if opstr.starts_with("##sf#") {
                        return analyze_sf(itp, &opstr[5..], p.rest().borrow());
                    } else if opstr.starts_with("##intr#") {
                        return analyze_intr(itp, &opstr[7..], p.rest().borrow());
                    } else {
                        unimplemented!()
                    }
                },
                _ => {
                    unimplemented!()
                }
            })
        },
        name: Symbol => {
            Ok(Var::new(itp, name.root()).as_any_ref())
        },
        _ => {
            Ok(Const::new(itp, v.root()).as_any_ref())
        }
    })
}

fn analyze_sf(itp: &mut Interpreter, opstr: &str, args: ValueHandle<Any>) -> CtrResult<Any> {
    match opstr {
        "def" => {
            typecase!(args, itp; {
                pair: ListPair => {
                    if let Some(name) = pair.first().borrow().downcast::<Symbol>(itp) {
                        let rest = pair.rest().downcast::<ListPair>(itp);
                        if let Some(value) = rest.clone().map(|ls| ls.first()) {
                            if rest.map(|ls| ls.rest())
                                   .and_then(|ls| ls.downcast::<ListEmpty>(itp))
                                   .is_some() {
                                let value_ast = try!(analyze(itp, value.borrow()));
                                return Ok(Def::new(itp, name.root(), value_ast).as_any_ref());
                            }
                        }
                    }
                    Err(CtrError::DefArgs)
                },
                ListEmpty => {
                    Err(CtrError::DefArgs)
                },
                _ => {
                    Err(CtrError::ImproperList(args.root()))
                }
            })
        },
        "do" => {
            typecase!(args, itp; {
                pair: ListPair => {
                    let mut argv: Vec<Root<Any>> = pair.iter(itp).collect();
                    for stmt in argv.iter_mut() {
                        *stmt = try!(analyze(itp, (*stmt).borrow()));
                    }
                    Ok(Do::new(itp, &argv).as_any_ref())
                },
                ListEmpty => {
                    Ok(Do::new(itp, &[]).as_any_ref())
                },
                _ => {
                    Err(CtrError::ImproperList(args.root()))
                }
            })
        },
        sf => Err(CtrError::UnknownSf(String::from(sf)))
    }
}

fn analyze_intr(itp: &mut Interpreter, opstr: &str, args: ValueHandle<Any>) -> CtrResult<Any> {
    if let Some(&op) = itp.exprfns.get(opstr) {
        typecase!(args, itp; {
            pair: ListPair => {
                let mut argv: Vec<Root<Any>> = pair.iter(itp).collect();
                for stmt in argv.iter_mut() {
                    *stmt = try!(analyze(itp, (*stmt).borrow()));
                }
                Ok(Expr::new(itp, op, argv.into_iter()).as_any_ref())
            },
            ListEmpty => {
                let argv = Vec::new();
                Ok(Expr::new(itp, op, argv.into_iter()).as_any_ref())
            },
            _ => { Err(CtrError::ImproperList(args.root())) }
        })
    } else if let Some(&op) = itp.stmtfns.get(opstr) {
        typecase!(args, itp; {
            pair: ListPair => {
                let mut argv: Vec<Root<Any>> = pair.iter(itp).collect();
                for stmt in argv.iter_mut() {
                    *stmt = try!(analyze(itp, (*stmt).borrow()));
                }
                Ok(Stmt::new(itp, op, argv.into_iter()).as_any_ref())
            },
            ListEmpty => {
                let argv = Vec::new();
                Ok(Stmt::new(itp, op, argv.into_iter()).as_any_ref())
            },
            _ => { Err(CtrError::ImproperList(args.root())) }
        })
    } else if let Some(&op) = itp.ctrlfns.get(opstr) {
        typecase!(args, itp; {
            pair: ListPair => {
                let mut argv: Vec<Root<Any>> = pair.iter(itp).collect();
                for stmt in argv.iter_mut() {
                    *stmt = try!(analyze(itp, (*stmt).borrow()));
                }
                let mut it = argv.into_iter();
                Ok(Ctrl::new(itp, op, it.next().unwrap(), it).as_any_ref())
            },
            ListEmpty => {
                Err(CtrError::Argc {
                    expected: (Greater, 0),
                    received: 0,
                })
            },
            _ => { Err(CtrError::ImproperList(args.root())) }
        })
    } else {
        Err(CtrError::UnknownIntr(String::from(opstr)))
    }
}

pub fn ast_to_sexpr(itp: &mut Interpreter, ast: ValueHandle<Any>) -> CtrResult<Any> {
    typecase!(ast, itp; {
        c: Const => {
            Ok(c.val())
        },
        d: Do => {
            let mut res = ListEmpty::new(itp).as_any_ref();
            for i in (0..ast.alloc_len()).rev() {
                res = ListPair::new(itp, d.stmts(i).unwrap(), res).as_any_ref()
            }
            let dosym = Symbol::new(itp, "$do");
            Ok(ListPair::new(itp, dosym.as_any_ref(), res).as_any_ref())
        },
        _ => {
            unimplemented!()
        }
    })
}
