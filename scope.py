from syntax import *

class Scope:
    def __init__(self, name=None, value=None, tail=None):
        assert tail is None or isinstance(tail, Scope)
        self.tail = tail
        self.name = name
        self.value = value

    def __getitem__(self, name):
        while self:
            if name == self.name:
                return self.value
            else:
                self = self.tail
        else:
            raise KeyError(name)

    def __contains__(self, name):
        try:
            self[name]
        except KeyError:
            return False
        else:
            return True

    def __iter__(self):
        while self:
            yield self.name, self.value
            self = self.tail

    def __repr__(self):
        return 'Scope([%s])' % ', '.join(map(repr, self))

    def bind(self, name, value):
        return Scope(name, value, self)

def scopeexpr(self, s):
    if isinstance(self, Call):
        scopeexpr(self.sym, s)
        for e in self.exprs:
            scopeexpr(e, s)
        return s
    elif isinstance(self, Num):
        pass
    elif isinstance(self, Str):
        pass
    elif isinstance(self, Sym):
        self.scope = s
        if self not in self.scope:
            assert False, "Note in scope!"
    else:
        raise NotImplementedError("scopeexpr not implemented for %r" % self)

def scopestmt(self, s):
    if isinstance(self, Let):
        scopeexpr(self.expr, s)
        return s.bind(self.sym, self)
    elif isinstance(self, Return):
        scopeexpr(self.exprs[0], s)
        return s
    else:
        return scopeexpr(self, s)

def scopedecl(self, s):
    if isinstance(self, Fun):
        self.scope = s
        s = s.bind(self.sym, self)

        ns = s
        for st in self.stmts:
            ns = scopestmt(st, ns)

        return s
    elif isinstance(self, Extern):
        return s.bind(self.sym, self)
    elif isinstance(self, Export):
        return s
    else:
        raise NotImplementedError("scopestmt not implemented for %r" % self)

def scope(ptree):
    s = Scope()

    for d in ptree:
        s = scopedecl(d, s)

    return s
