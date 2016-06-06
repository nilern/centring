
class Value(object):
    pass

class Type(Value):
    def __init__(self, name):
        self._namesym = name

    def type(self):
        return Type._type

class Symbol(Value):
    def __init__(self, ns, name=None):
        if name is None:
            self._ns = None
            self._name = ns
        else:
            self._ns = ns
            self._name = name

Type._type = Type(Symbol(u"centring.lang", u"Type"))
Symbol._type = Type(Symbol(u"centring.lang", u"Symbol"))

class Int(Value):
    _type = Type(Symbol(u"centring.lang", u"Int"))
    
    def __init__(self, i):
        self._int = i

    def type(self):
        return Int._type

    def unwrap_int(self):
        return self._int

class Bool(Value):
    _type = Type(Symbol(u"centring.lang", u"Bool"))
    
    def __init__(self, b):
        self._bool = b

    def type(self):
        return Bool._type

    def unwrap_bool(self):
        return self._bool

class Record(Value):
    def __init__(self, type, field_vals):
        self._type = type
        self._fields = field_vals

    def type(self):
        return self._type

    def rec_ref(self, i):
        return self._fields[i]

