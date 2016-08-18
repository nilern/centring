# Data Representation

# AST

    enum AST {
        Fn {
            name: symbol,
            formal: symbol,
            cases: vector<pair<AST, AST>>
        },
        Primop {
            op: Primop,
            args: vector<AST>,
            conts: Option<vector<AST>>
        },
        Closure {
            env: Env,
            expr: AST
        },
        Id(Symbol),
        Const(Value)
    }

# Primops

    enum Primop {
        ExprOp((vector<Value>) -> Value),
        StmtOp((vector<Value>) -> Value),
        CtrlOp((vector<Value>, vector<AST>) -> AST)
    }

# CEK

    enum Cont {
        Primop {
            op: Primop,
            vals: vector<Value>,
            asts: vector<AST>,
            index: fixnum,
            conts: Option<vector<AST>>,
            env: Env,
            cont: Cont
        },
        Halt
    }
