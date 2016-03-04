use value::Value;
use std::fmt;
use std::rc::Rc;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(true) => write!(f, "#t"),
            Value::Bool(false) => write!(f, "#f"),
            Value::Char(c) => {
                match c {
                    '\n' => write!(f, "\\newline"),
                    ' ' => write!(f, "\\space"),
                    '\t' => write!(f, "\\tab"),
                    '\r' => write!(f, "\\return"),
                    _ => write!(f, "\\{}", c)
                }
            },  
            Value::Symbol(Some(ref mod_name), ref name) =>
                write!(f, "{}/{}", mod_name, name),
            Value::Symbol(None, ref name) => write!(f, "{}", name),

            Value::Tuple(ref vs) => {
                try!(write!(f, "#("));
                for v in vs { try!(write!(f, " {}", v)) }
                write!(f, ")")
            },
            Value::Array(ref vs) => {
                try!(write!(f, "#["));
                for v in vs { try!(write!(f, " {}", v)) }
                write!(f, "]")
            },
            Value::Pair { first: ref v, rest: ref vs } => {
                try!(write!(f, "("));
                let mut h = v;
                let mut t = vs;
                loop {
                    try!(write!(f, "{}", h));
                    match **t {
                        Value::EmptyList => break,
                        Value::Pair { first: ref v, rest: ref vs } => {
                            h = v;
                            t = vs;
                        },
                        _ => { try!(write!(f, " . {}", t)); break }
                    }
                    try!(write!(f, " "));
                }
                write!(f, ")")
            },
            Value::EmptyList => write!(f, "()"),

            Value::Singleton { ref typ } => write!(f, "#=({})", typ),
            Value::Record { ref typ, vals: ref vs } => {
                try!(write!(f, "#=({}", typ));
                for v in vs { try!(write!(f, " {}", v)) }
                write!(f, ")")
            },

            Value::AbstractType { ref name, .. } => write!(f, "{}", name),
            Value::SingletonType { ref name, .. } => write!(f, "{}", name),
            Value::RecordType { ref name, .. } => write!(f, "{}", name),
            Value::BuiltInType { ref name, .. } => write!(f, "{}", name),

            Value::Fn { ref name, formal_types: ref ftps, .. } => {
                try!(write!(f, "#<Fn {} (", name));
                for ftp in ftps { try!(write!(f, " {}", ftp)) }
                write!(f, ")>")
            },
            Value::NativeFn { ref name, formal_types: ref ftps, .. } => {
                try!(write!(f, "#<NativeFn {} (", name));
                for ftp in ftps { try!(write!(f, " {}", ftp)) }
                write!(f, ")>")
            }
        }
    }
}
