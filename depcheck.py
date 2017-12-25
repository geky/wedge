from wsyntax import *
from wtypes import *

class Deps:
    def __init__(self):
        self.matches = set()
        self.targets = []

    def append(self, sym):
        assert isinstance(sym, Sym)
        assert hasattr(sym, 'type')
        assert hasattr(sym, 'impl')

        if (sym, sym.type) in self.matches:
            return

        self.matches.add((sym, sym.type))
        self.targets.append(sym)
        depdecl(sym.impl, self)

    def __contains__(self, sym):
        return (sym, sym.type) in self.syms

    def __iter__(self):
        return iter(self.targets)

    def __repr__(self):
        return 'Deps(%r)' % self.targets

def depexpr(self, d):
    if isinstance(self, Call):
        d.append(self.sym)
        for e in self.exprs:
            depexpr(e, d)
    elif isinstance(self, Num):
        pass
    elif isinstance(self, Str):
        pass
    elif isinstance(self, Sym):
        pass
    else:
        raise NotImplementedError("depexpr not implemented for %r" % self)

def depexprs(self, d):
    for expr in self:
        depexpr(expr, d)

def depstmt(self, d):
    if isinstance(self, Let):
        for expr in self.exprs:
            depexpr(expr, d)
    elif isinstance(self, Def):
        pass
    elif isinstance(self, Return):
        depexprs(self.exprs, d)
    elif isinstance(self, Expr):
        depexprs(self.exprs, d)
    else:
        raise NotImplementedError("depstmt not implemented for %r" % self)

def depdecl(self, d):
    if isinstance(self, Fun):
        for s in self.stmts:
            depstmt(s, d)
    elif isinstance(self, Extern):
        pass
    elif isinstance(self, Type):
        for s in self.stmts:
            depstmt(s, d)
    else:
        raise NotImplementedError("depdecl not implemented for %r" % self)

def depcheck(scope):
    targets = Deps()
    for sym in scope:
        if hasattr(sym, 'impl') and hasattr(sym, 'export'):
            targets.append(sym)

    return targets
