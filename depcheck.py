from syntax import *

class Deps:
    def __init__(self):
        self.syms = set()
        self.targets = []

    def append(self, sym, scope=None):
        assert isinstance(sym, Sym)
        assert scope or hasattr(sym, 'scope')

        scope = scope or sym.scope
        v = scope[sym]

        if sym in self.syms:
            return

        self.syms.add(sym)
        self.targets.append((sym, v))
        depdecl(v, self)

    def __contains__(self, sym):
        return sym in self.syms

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
    else:
        raise NotImplementedError("depdecl not implemented for %r" % self)

def depcheck(scope):
    targets = Deps()
    for name, d in scope:
        if isinstance(d, Fun) and d.scope.isexported(d.sym):
            targets.append(name, scope)
            depdecl(d, targets)

    return [v for _, v in targets]
