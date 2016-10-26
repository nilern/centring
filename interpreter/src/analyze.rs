use interpreter::{Interpreter, CtrResult, CtrError};
use value::{ConcreteType, Any, ListPair, ListEmpty, Symbol, Do, Const};
use refs::{Root, ValueHandle};

pub fn analyze(itp: &mut Interpreter, v: ValueHandle<Any>) -> CtrResult<Any> {
    if let Some(p) = v.downcast::<ListPair>(itp) {
        if let Some(op) = p.first().borrow().downcast::<Symbol>(itp) {
            let opstr = op.to_string();
            if opstr.starts_with("##sf#") {
                analyze_sf(itp, &opstr[5..], p.rest().borrow())
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
            if let Some(pair) = args.downcast::<ListPair>(itp) {
                let mut argv: Vec<Root<Any>> = pair.iter(itp).collect();
                let mut stmt_handles = vec![];
                for stmt in argv.iter_mut() {
                    *stmt = try!(analyze(itp, (*stmt).borrow()));
                    stmt_handles.push((*stmt).borrow());
                }
                Ok(itp.alloc_do(&stmt_handles).as_any_ref())
            } else if args.instanceof(ListEmpty::typ(itp)) {
                Ok(itp.alloc_do(&[]).as_any_ref())
            } else {
                return Err(CtrError::ImproperList(args.root()));
            }
        },
        sf => Err(CtrError::UnknownSf(String::from(sf)))
    }
}

pub fn ast_to_sexpr(itp: &mut Interpreter, ast: ValueHandle<Any>) -> CtrResult<Any> {
    if let Some(c) = ast.downcast::<Const>(itp) {
        Ok(c.val())
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
