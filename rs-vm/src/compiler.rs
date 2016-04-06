
#[derive(Debug)]
pub enum Sexpr {
    Int(isize),

    Symbol(Symbol),

    List(Vec<Sexpr>)
}

#[derive(Debug)]
pub struct Symbol(pub Option<String>, pub String);

#[derive(Debug)]
pub enum CPS {
    Primop(Symbol, Vec<CPS>),
    Halt(Box<CPS>),
    
    Var(Symbol),
    Const(Sexpr)
}

pub fn cps(sexp: Sexpr) -> CPS {
    cps_k(sexp, halt_k)
}

pub fn halt_k(cexp: CPS) -> CPS {
    CPS::Halt(Box::new(cexp))
}

pub fn cps_k<K>(sexp: Sexpr, k: K) -> CPS
    where K: FnOnce(CPS) -> CPS {
    match sexp {
        Sexpr::Int(_) => k(CPS::Const(sexp)),
        Sexpr::Symbol(sym) => k(CPS::Var(sym)),
        _ => panic!()
        // Sexpr::List(vals) => {
        //     let mut it = vals.into_iter();
        //     let op = it.next().unwrap();
        //     match op {
        //         Sexpr::Symbol(Symbol(Some(md), name)) =>
        //             if let "centring.intr" = md.as_ref() {
        //                 cps_list(it, |cexps|
        //                          CPS::Primop(Symbol(None, name.clone()), cexps))
        //             } else {
        //                 panic!()
        //             },
        //         _ => panic!()
        //     }
        // }
    }
}

pub fn cps_list<I: Iterator<Item=Sexpr>, K>(mut sexps: I, k: K) -> CPS
    where K: Fn(Vec<CPS>) -> CPS {
    cps_l(sexps, vec![], k)
}

fn cps_l<I: Iterator<Item=Sexpr>, K>(mut sexps: I, mut res: Vec<CPS>, k: K) -> CPS
    where K: Fn(Vec<CPS>) -> CPS {
    if let Some(sexp) = sexps.next() {
        cps_k(sexp, |cexp| { res.push(cexp); cps_l(sexps, res, k) })
    } else {
        k(res)
    }
}
