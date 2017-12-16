from syntax import *
from type import *

class ScopeException(Exception):
    pass

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

    def __bool__(self):
        return not (self.name is None and self.tail is None)

    def __iter__(self):
        all = []
        while self:
            all.append((self.name, self.value))
            self = self.tail
        return reversed(all)

    def __repr__(self):
        return 'Scope([%s])' % ', '.join(map(repr, self))

    def bind(self, name, value):
        assert isinstance(name, Sym)
        return Scope(name, value, self)

def scopetype(self, s):
    if isinstance(self, IntT):
        pass
    elif isinstance(self, FunT):
        pass
    else:
        raise NotImplementedError("scopetype not implemented for %r" % self)

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
        if self not in s:
            raise ScopeException("Symbol not in scope %s" % self)

        self.scope = s
        self.local = s[self].sym.local
    else:
        raise NotImplementedError("scopeexpr not implemented for %r" % self)

def scopestmt(self, s):
    if isinstance(self, Let):
        self.scope = s
        scopeexpr(self.expr, s)
        return s.bind(self.sym, self)
    if isinstance(self, Def):
        scopetype(self.type, s)
        return s.bind(self.sym, self)
    elif isinstance(self, Return):
        scopeexpr(self.exprs[0], s)
        return s
    else:
        return scopeexpr(self, s)

def scopedecl(self, s):
    if isinstance(self, Fun):
        self.scope = s
        self.sym.local = False
        s = s.bind(self.sym, self)
        ns = s

        for arg in self.args:
            arg.sym = arg # hmm
            ns = ns.bind(arg, arg)

        for stmt in self.stmts:
            ns = scopestmt(stmt, ns)

        return s
    elif isinstance(self, Extern):
        self.sym.local = False
        return s.bind(self.sym, self)
    elif isinstance(self, Export):
        return s
    elif isinstance(self, Def):
        scopetype(self.type, s)
        self.sym.local = False
        return s.bind(self.sym, self)
    else:
        raise NotImplementedError("scopedecl not implemented for %r" % self)

def scopecheck(ptree):
    s = Scope()

    for d in ptree:
        s = scopedecl(d, s)

    return s
