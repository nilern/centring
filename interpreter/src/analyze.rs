use interpreter::{Interpreter, CtrResult, CtrError};
use value::{ConcreteType, Any, ListPair, ListEmpty, Symbol, Do, Const};
use refs::{Root, ValueHandle};

pub fn analyze(itp: &mut Interpreter, v: ValueHandle<Any>) -> CtrResult<Any> {
    if let Some(p) = v.downcast::<ListPair>(itp) {
        let head = unsafe { Root::<Any>::new(p.head) };
        if let Some(op) = head.borrow().downcast::<Symbol>(itp) {
            let opstr = op.to_string();
            if opstr.starts_with("##sf#") {
                analyze_sf(itp, &opstr[5..], unsafe { Root::<Any>::new(p.tail).borrow() })
            } else {
                unimplemented!()
            }
        } else {
            unimplemented!()
        }
    } else {
        Ok(itp.alloc_const(v).as_any_ref())
    }
}

fn analyze_sf(itp: &mut Interpreter, opstr: &str, args: ValueHandle<Any>) -> CtrResult<Any> {
    match opstr {
        "do" => {
            let mut args = args.root();
            let mut argv = Vec::new();
            loop {
                if args.borrow().instanceof(ListEmpty::typ(itp)) {
                    let stmt_handles: Vec<ValueHandle<Any>> =
                        argv.iter().map(Root::borrow).collect();
                    return Ok(itp.alloc_do(&stmt_handles).as_any_ref());
                } else if let Some(p) = args.clone().borrow().downcast::<ListPair>(itp) {
                    let stmt = try!(analyze(itp, unsafe { Root::<Any>::new(p.head).borrow() }));
                    argv.push(stmt);
                    args = unsafe { Root::new(p.tail) };
                } else {
                    return Err(CtrError::ImproperList(args));
                }
            }
        },
        sf => Err(CtrError::UnknownSf(String::from(sf)))
    }
}

pub fn ast_to_sexpr(itp: &mut Interpreter, ast: ValueHandle<Any>) -> CtrResult<Any> {
    if let Some(c) = ast.downcast::<Const>(itp) {
        Ok(unsafe { Root::new(c.val) })
    } else if let Some(d) = ast.downcast::<Do>(itp) {
        let mut res = itp.alloc_nil().as_any_ref();
        for i in (0..ast.alloc_len()).rev() {
            res = itp.alloc_pair(d.stmts(i).unwrap().borrow(), res.borrow()).as_any_ref();
        }
        let dosym = itp.alloc_symbol("$do");
        Ok(itp.alloc_pair(dosym.borrow().as_any_ref(), res.borrow()).as_any_ref())
    } else {
        unimplemented!()
    }
}
