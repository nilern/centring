use interpreter::{Interpreter, CtrResult, CtrError};
use value::{ConcreteType, Any, ListPair, ListEmpty, Symbol, Def, Expr, Do, Const};
use refs::{Root, ValueHandle};
use primops;

pub fn analyze(itp: &mut Interpreter, v: ValueHandle<Any>) -> CtrResult<Any> {
    if let Some(p) = v.downcast::<ListPair>(itp) {
        if let Some(op) = p.first().borrow().downcast::<Symbol>(itp) {
            let opstr = op.to_string();
            if opstr.starts_with("##sf#") {
                analyze_sf(itp, &opstr[5..], p.rest().borrow())
            } else if opstr.starts_with("##intr#") {
                analyze_intr(itp, &opstr[7..], p.rest().borrow())
            } else {
                unimplemented!()
            }
        } else {
            unimplemented!()
        }
    } else {
        Ok(Const::new(itp, v.root()).as_any_ref())
    }
}

fn analyze_sf(itp: &mut Interpreter, opstr: &str, args: ValueHandle<Any>) -> CtrResult<Any> {
    match opstr {
        "def" => {
            if let Some(pair) = args.downcast::<ListPair>(itp) {
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
            } else if args.instanceof(ListEmpty::typ(itp)) {
                Err(CtrError::DefArgs)
            } else {
                Err(CtrError::ImproperList(args.root()))
            }
        },
        "do" => {
            if let Some(pair) = args.downcast::<ListPair>(itp) {
                let mut argv: Vec<Root<Any>> = pair.iter(itp).collect();
                for stmt in argv.iter_mut() {
                    *stmt = try!(analyze(itp, (*stmt).borrow()));
                }
                Ok(Do::new(itp, &argv).as_any_ref())
            } else if args.instanceof(ListEmpty::typ(itp)) {
                Ok(Do::new(itp, &[]).as_any_ref())
            } else {
                Err(CtrError::ImproperList(args.root()))
            }
        },
        sf => Err(CtrError::UnknownSf(String::from(sf)))
    }
}

fn analyze_intr(itp: &mut Interpreter, opstr: &str, args: ValueHandle<Any>) -> CtrResult<Any> {
    let op = match opstr {
        "iadd" => primops::iadd,
        intr => return Err(CtrError::UnknownIntr(String::from(intr)))
    };
    if let Some(pair) = args.downcast::<ListPair>(itp) {
        let mut argv: Vec<Root<Any>> = pair.iter(itp).collect();
        for stmt in argv.iter_mut() {
            *stmt = try!(analyze(itp, (*stmt).borrow()));
        }
        Ok(Expr::new(itp, op, argv.into_iter()).as_any_ref())
    } else if args.instanceof(ListEmpty::typ(itp)) {
        let argv = Vec::new();
        Ok(Expr::new(itp, op, argv.into_iter()).as_any_ref())
    } else {
        return Err(CtrError::ImproperList(args.root()));
    }
}

pub fn ast_to_sexpr(itp: &mut Interpreter, ast: ValueHandle<Any>) -> CtrResult<Any> {
    if let Some(c) = ast.downcast::<Const>(itp) {
        Ok(c.val())
    } else if let Some(d) = ast.downcast::<Do>(itp) {
        let mut res = ListEmpty::new(itp).as_any_ref();
        for i in (0..ast.alloc_len()).rev() {
            res = ListPair::new(itp, d.stmts(i).unwrap(), res).as_any_ref()
        }
        let dosym = Symbol::new(itp, "$do");
        Ok(ListPair::new(itp, dosym.as_any_ref(), res).as_any_ref())
    } else {
        unimplemented!()
    }
}
