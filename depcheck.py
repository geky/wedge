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

def depstmt(self, d):
    if isinstance(self, Let):
        depexpr(self.expr, d)
    elif isinstance(self, Def):
        pass
    elif isinstance(self, Return):
        depexpr(self.exprs[0], d)
    else:
        depexpr(self, d)

def depdecl(self, d):
    if isinstance(self, Fun):
        for s in self.stmts:
            depstmt(s, d)
    else:
        raise NotImplementedError("depdecl not implemented for %r" % self)

def depcheck(scope):
    targets = Deps()
    for name, d in scope:
        if hasattr(d, 'export') and d.export:
            depdecl(d, targets)
            targets.append(name, scope)

    return [v for _, v in targets]
