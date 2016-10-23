use interpreter::{Interpreter, CtrResult, CtrError};
use value::{Downcast, Any, ListPair, ListEmpty, Symbol, Do, Const};
use refs::{Root, ValueHandle};

pub fn analyze(itp: &mut Interpreter, v: ValueHandle<Any>) -> CtrResult<Any> {
    let op: Option<ValueHandle<ListPair>> = v.downcast(itp);
    if let Some(p) = op {
        let oop: Option<Root<Symbol>> = unsafe { Root::<Any>::new(p.head).downcast(itp) };
        if let Some(op) = oop {
            let opstr = op.borrow().to_string();
            if opstr.starts_with("##sf#") {
                return analyze_sf(itp, &opstr[5..], unsafe { Root::<Any>::new(p.tail).borrow() })
            } else {
                unimplemented!()
            }
        } else {
            unimplemented!()
        }
    }
    Ok(itp.alloc_const(v).as_any_ref())
}

fn analyze_sf(itp: &mut Interpreter, opstr: &str, args: ValueHandle<Any>) -> CtrResult<Any> {
    match opstr {
        "do" => {
            let mut args = args.root();
            let mut argv = Vec::new();
            loop {
                let oe: Option<Root<ListEmpty>> = args.downcast(itp);
                if let Some(_) = oe {
                    let stmt_handles: Vec<ValueHandle<Any>> =
                        argv.iter().map(Root::borrow).collect();
                    return Ok(itp.alloc_do(&stmt_handles).as_any_ref())
                }

                let op: Option<Root<ListPair>> = args.downcast(itp);
                if let Some(p) = op.clone() {
                    let stmt = try!(analyze(itp, unsafe { Root::<Any>::new(p.head).borrow() }));
                    argv.push(stmt);
                    args = unsafe { Root::new(p.tail) };
                    continue;
                }

                return Err(CtrError::ImproperList(args));
            }
        },
        sf => Err(CtrError::UnknownSf(String::from(sf)))
    }
}

pub fn ast_to_sexpr(itp: &mut Interpreter, ast: ValueHandle<Any>) -> CtrResult<Any> {
    let oc: Option<ValueHandle<Const>> = ast.downcast(itp);
    if let Some(c) = oc {
        return Ok(unsafe { Root::new(c.val) });
    }
    let od: Option<ValueHandle<Do>> = ast.downcast(itp);
    if let Some(d) = od {
        let mut res = itp.alloc_nil().as_any_ref();
        for i in (0..ast.alloc_len()).rev() {
            res = itp.alloc_pair(d.stmts(i).unwrap().borrow(), res.borrow()).as_any_ref();
        }
        let dosym = itp.alloc_symbol("$do");
        return Ok(itp.alloc_pair(dosym.borrow().as_any_ref(), res.borrow()).as_any_ref());
    }
    unimplemented!()
}
