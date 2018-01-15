from wsyntax import *
from wtypes import *
from scopecheck import Var

class Deps:
    def __init__(self):
        self.matches = set()
        self.targets = []

    def append(self, var):
        assert isinstance(var, Var)
        assert hasattr(var, 'sym')
        assert hasattr(var, 'type')
        assert hasattr(var, 'impl')

        if (var.sym, var.type) in self.matches:
            return

        self.matches.add((var.sym, var.type))
        self.targets.append(var)
        depdecl(var.impl, self)

    def __contains__(self, var):
        return (var.sym, var.type) in self.vars

    def __iter__(self):
        return iter(self.targets)

    def __repr__(self):
        return 'Deps(%r)' % self.targets

def depexpr(self, d):
    if isinstance(self, Call):
        d.append(self.callee.var)
        for e in self.exprs:
            depexpr(e, d)
    elif isinstance(self, Init):
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
    elif isinstance(self, Type) or isinstance(self, Struct):
        for s in self.stmts:
            depstmt(s, d)
    elif isinstance(self, RawFunImpl):
        pass
    else:
        raise NotImplementedError("depdecl not implemented for %r" % self)

def depcheck(scope):
    targets = Deps()
    for var in scope:
        if hasattr(var, 'impl') and hasattr(var, 'export'):
            targets.append(var)

    return targets
