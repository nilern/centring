use value::{Value, TypeMatcher};
use std::fmt;

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
            Value::Keyword(Some(ref mod_name), ref name) =>
                write!(f, ":{}/{}", mod_name, name),
            Value::Keyword(None, ref name) => write!(f, ":{}", name),

            Value::Tuple(ref vs) => {
                try!(write!(f, "#("));
                let mut it = vs.iter();
                if let Some(v) = it.next() {
                    try!(write!(f, "{}", v));
                }
                for v in it {
                    try!(write!(f, " {}", v))
                }
                write!(f, ")")
            },
            Value::List(ref vs) => {
                try!(write!(f, "("));
                let mut it = vs.iter();
                if let Some(v) = it.next() {
                    try!(write!(f, "{}", v));
                }
                for v in it {
                    try!(write!(f, " {}", v))
                }
                write!(f, ")")
            },
            Value::String(ref s) => write!(f, "{:?}", s),

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
            },
            Value::MultiFn { ref name, ref methods } =>
                write!(f, "#<MultiFn {} with {} methods>", name, methods.len()),
            Value::Macro(ref expander) =>
                write!(f, "#<Macro {}>", expander)
        }
    }
}

impl fmt::Display for TypeMatcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TypeMatcher::Isa(ref typ) => write!(f, "{}", typ),
            TypeMatcher::Identical(ref typ) => write!(f, "(= {})", typ)
        }
    }
}
