use interpreter::{Interpreter, CtrResult, CtrError};
use value::{CtrValue, ConcreteType, Any,
            ListPair, ListEmpty, Symbol, Array, FnNode, Def, Expr, Stmt, Ctrl, Do, Var, Const};
use refs::{Root, ValueHandle};

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
        name: Symbol => { Ok(Var::new(itp, name.root()).as_any_ref()) },
        _ => { Ok(Const::new(itp, v.root()).as_any_ref()) }
    })
}

fn analyze_sf(itp: &mut Interpreter, opstr: &str, args: ValueHandle<Any>) -> CtrResult<Any> {
    match opstr {
        "fn" => {
            fn analyze_case(itp: &mut Interpreter, case: ValueHandle<ListPair>)
                            -> Result<Vec<Root<Any>>, CtrError> {
                let mut elems = Vec::with_capacity(2);
                elems.extend(case.iter(itp));
                if elems.len() != 2 {
                    return Err(CtrError::FnCase);
                }
                let cond = try!(analyze(itp, elems[0].borrow())); // todo: dnf
                let body = try!(analyze(itp, elems[1].borrow()));
                Ok(vec![cond, body])
            }

            typecase!(args, itp; {
                pair: ListPair => {
                    let mut argv: Vec<_> = pair.iter(itp).collect();
                    if argv.len() < 3 {
                        return Err(CtrError::FnArgs);
                    }
                    let name = try!(argv[0].clone().downcast::<Symbol>(itp));
                    let formal = try!(argv[1].clone().downcast::<Symbol>(itp));
                    for case in argv.iter_mut().skip(2) {
                        let case_pair = try!(case.clone().downcast::<ListPair>(itp));
                        let case_vec = try!(analyze_case(itp, case_pair.borrow()));
                        *case = Array::new(itp, case_vec.into_iter()).as_any_ref();
                    }
                    Ok(FnNode::new(itp, name, formal, argv[2..].into_iter().cloned()).as_any_ref())
                },
                ListEmpty => { Err(CtrError::FnArgs) },
                _ => { Err(CtrError::ImproperList(args.root())) }
            })
        },
        "def" => {
            typecase!(args, itp; {
                pair: ListPair => {
                    let mut argv = Vec::with_capacity(2);
                    argv.extend(pair.iter(itp));
                    if argv.len() != 2 {
                        return Err(CtrError::DefArgs);
                    }
                    let name = try!(argv[0].clone().downcast::<Symbol>(itp));
                    let value = try!(analyze(itp, argv[1].borrow()));
                    Ok(Def::new(itp, name, value).as_any_ref())
                },
                ListEmpty => { Err(CtrError::DefArgs) },
                _ => { Err(CtrError::ImproperList(args.root())) }
            })
        },
        "do" => {
            analyze_list(itp, args).map(|stmts| Do::new(itp, stmts.into_iter()).as_any_ref())
        },
        sf => Err(CtrError::UnknownSf(String::from(sf)))
    }
}

fn analyze_intr(itp: &mut Interpreter, opstr: &str, args: ValueHandle<Any>) -> CtrResult<Any> {
    if let Some(&op) = itp.exprfns.get(opstr) {
        analyze_list(itp, args).map(|args| Expr::new(itp, op, args.into_iter()).as_any_ref())
    } else if let Some(&op) = itp.stmtfns.get(opstr) {
        analyze_list(itp, args).map(|args| Stmt::new(itp, op, args.into_iter()).as_any_ref())
    } else if let Some(&op) = itp.ctrlfns.get(opstr) {
        analyze_list(itp, args).and_then(|args|
            if !args.is_empty() {
                let mut it = args.into_iter();
                Ok(Ctrl::new(itp, op, it.next().unwrap(), it).as_any_ref())
            } else {
                Err(CtrError::Argc { expected: (Greater, 0), received: 0, })
            })
    } else {
        Err(CtrError::UnknownIntr(String::from(opstr)))
    }
}

pub fn ast_to_sexpr(itp: &mut Interpreter, ast: ValueHandle<Any>) -> CtrResult<Any> {
    typecase!(ast, itp; {
        c: Const => { Ok(c.val()) },
        d: Do => {
            let mut res = ListEmpty::new(itp).as_any_ref();
            for i in (0..ast.alloc_len()).rev() {
                res = ListPair::new(itp, d.stmts(i).unwrap(), res).as_any_ref()
            }
            let dosym = Symbol::new(itp, "$do");
            Ok(ListPair::new(itp, dosym.as_any_ref(), res).as_any_ref())
        },
        _ => { unimplemented!() }
    })
}

fn analyze_list(itp: &mut Interpreter, args: ValueHandle<Any>)
                -> Result<Vec<Root<Any>>, CtrError> {
    typecase!(args, itp; {
        pair: ListPair => {
            pair.iter(itp)
                .collect::<Vec<_>>().into_iter() // HACK
                .map(|stmt| analyze(itp, stmt.borrow()))
                .collect()
        },
        ListEmpty => { Ok(Vec::new()) },
        _ => { Err(CtrError::ImproperList(args.root())) }
    })
}
