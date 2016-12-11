use interpreter::{ITP, CtrResult, CtrError};
use value::{CtrValue, ConcreteType, Any,
            ListPair, ListEmpty, Symbol, Array, FnNode, App, Def, Expr, Stmt, Ctrl, Do, Var, Const};
use refs::{Root, ValueHandle};

use std::cmp::Ordering::Greater;

pub fn analyze(v: ValueHandle<Any>) -> CtrResult<Any> {
    typecase!(v; {
        p: ListPair => {
            let op = p.first();
            typecase!(op.borrow(); {
                op: Symbol => {
                    let opstr = op.to_string();
                    if opstr.starts_with("##sf#") {
                        analyze_sf(&opstr[5..], p.rest().borrow())
                    } else if opstr.starts_with("##intr#") {
                        analyze_intr(&opstr[7..], p.rest().borrow())
                    } else {
                        analyze_call(op.as_any_ref(), p.rest().borrow())
                    }
                },
                _ => {
                    analyze_call(op.borrow(), p.rest().borrow())
                }
            })
        },
        name: Symbol => { Ok(Var::new(name.root()).as_any_ref()) },
        _ => { Ok(Const::new(v.root()).as_any_ref()) }
    })
}

fn analyze_call(callee: ValueHandle<Any>, args: ValueHandle<Any>) -> CtrResult<Any> {
    let callee = try!(analyze(callee));
    let arg = try!(analyze_list(args).map(|args| {
        let (rec, array_t) = ITP.with(|itp| {
            let itp = itp.borrow();
            (*itp.exprfns.get("rec").unwrap(), itp.array_t.clone().as_any_ref())
        });
        let array_t_ast = Const::new(array_t).as_any_ref();
        let mut argv = vec![array_t_ast];
        argv.extend(args.into_iter());
        Expr::new(rec, argv.into_iter()).as_any_ref()
    }));
    Ok(App::new(callee, arg).as_any_ref())
}

fn analyze_sf(opstr: &str, args: ValueHandle<Any>) -> CtrResult<Any> {
    match opstr {
        "fn" => {
            fn analyze_case(case: ValueHandle<ListPair>)
                            -> Result<Vec<Root<Any>>, CtrError> {
                let mut elems = Vec::with_capacity(2);
                elems.extend(case.iter());
                if elems.len() != 2 {
                    return Err(CtrError::FnCase);
                }
                let cond = try!(analyze(elems[0].borrow())); // todo: dnf
                let body = try!(analyze(elems[1].borrow()));
                Ok(vec![cond, body])
            }

            typecase!(args; {
                pair: ListPair => {
                    let mut argv: Vec<_> = pair.iter().collect();
                    if argv.len() < 3 {
                        return Err(CtrError::FnArgs);
                    }
                    let name = try!(argv[0].clone().downcast::<Symbol>());
                    let formal = try!(argv[1].clone().downcast::<Symbol>());
                    for case in argv.iter_mut().skip(2) {
                        let case_pair = try!(case.clone().downcast::<ListPair>());
                        let case_vec = try!(analyze_case(case_pair.borrow()));
                        *case = Array::new(case_vec.into_iter()).as_any_ref();
                    }
                    Ok(FnNode::new(name, formal, argv[2..].into_iter().cloned()).as_any_ref())
                },
                ListEmpty => { Err(CtrError::FnArgs) },
                _ => { Err(CtrError::ImproperList(args.root())) }
            })
        },
        "def" => {
            typecase!(args; {
                pair: ListPair => {
                    let mut argv = Vec::with_capacity(2);
                    argv.extend(pair.iter());
                    if argv.len() != 2 {
                        return Err(CtrError::DefArgs);
                    }
                    let name = try!(argv[0].clone().downcast::<Symbol>());
                    let value = try!(analyze(argv[1].borrow()));
                    Ok(Def::new(name, value).as_any_ref())
                },
                ListEmpty => { Err(CtrError::DefArgs) },
                _ => { Err(CtrError::ImproperList(args.root())) }
            })
        },
        "do" => {
            analyze_list(args).map(|stmts| Do::new(stmts.into_iter()).as_any_ref())
        },
        sf => Err(CtrError::UnknownSf(String::from(sf)))
    }
}

fn analyze_intr(opstr: &str, args: ValueHandle<Any>) -> CtrResult<Any> {
    let (exprfn, stmtfn, ctrlfn) = ITP.with(|itp| {
        (itp.borrow().exprfns.get(opstr).map(|&f| f),
         itp.borrow().stmtfns.get(opstr).map(|&f| f),
         itp.borrow().ctrlfns.get(opstr).map(|&f| f))
    });

    if let Some(op) = exprfn {
        analyze_list(args).map(|args| Expr::new(op, args.into_iter()).as_any_ref())
    } else if let Some(op) = stmtfn {
        analyze_list(args).map(|args| Stmt::new(op, args.into_iter()).as_any_ref())
    } else if let Some(op) = ctrlfn {
        analyze_list(args).and_then(|args|
            if !args.is_empty() {
                let mut it = args.into_iter();
                Ok(Ctrl::new(op, it.next().unwrap(), it).as_any_ref())
            } else {
                Err(CtrError::Argc { expected: (Greater, 0), received: 0, })
            })
    } else {
        Err(CtrError::UnknownIntr(String::from(opstr)))
    }
}

pub fn ast_to_sexpr(ast: ValueHandle<Any>) -> CtrResult<Any> {
    typecase!(ast; {
        c: Const => { Ok(c.val()) },
        d: Do => {
            let mut res = ListEmpty::new().as_any_ref();
            for i in (0..ast.alloc_len()).rev() {
                res = ListPair::new(d.stmts(i).unwrap(), res).as_any_ref()
            }
            let dosym = Symbol::new("$do");
            Ok(ListPair::new(dosym.as_any_ref(), res).as_any_ref())
        },
        _ => { unimplemented!() }
    })
}

fn analyze_list(args: ValueHandle<Any>) -> Result<Vec<Root<Any>>, CtrError> {
    typecase!(args; {
        pair: ListPair => {
            pair.iter()
                .collect::<Vec<_>>().into_iter() // HACK
                .map(|stmt| analyze(stmt.borrow()))
                .collect()
        },
        ListEmpty => { Ok(Vec::new()) },
        _ => { Err(CtrError::ImproperList(args.root())) }
    })
}
