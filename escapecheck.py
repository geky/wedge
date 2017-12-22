from syntax import *
from type import *
from scopecheck import scopeexpr
from typecheck import typeexpr

def escapeexpr(self, home):
    if isinstance(self, Call):
        if home:
            self.home = home

        self.sym = escapeexpr(self.sym, None)
        self.exprs = [escapeexpr(expr, None) for expr in self.exprs]
        return self
    elif isinstance(self, Num):
        if home:
            self.home = home
        return self
    elif isinstance(self, Str):
        if home:
            self.home = home
        return self
    elif isinstance(self, Sym):
        if home:
            expr = Call(Sym("%s_copy" % self.type.name), [self])
            scopeexpr(expr, self.scope)
            typeexpr(expr) # TODO assert type?
            return expr
        return self
    else:
        raise NotImplementedError("escapeexpr not implemented for %r" % self)

def escapeexprs(self, homes):
    if len(self) == 1 and isinstance(self[0], Call):
        if homes and homes[0]:
            self[0].homes = homes
            self[0].home = homes[0]
        return self
    else:
        return [escapeexpr(expr, home)
            for expr, home in zip(self, homes)]

def escapestmt(self):
    if isinstance(self, Let):
        self.exprs = escapeexprs(self.exprs, self.syms)
        return self
    elif isinstance(self, Def):
        return self
    elif isinstance(self, Return):
        self.exprs = escapeexprs(self.exprs, [self for _ in self.exprs])
        return self
    elif isinstance(self, Expr):
        self.exprs = escapeexprs(self.exprs, [])
        return self
    else:
        raise NotImplementedError("escapestmt not implemented for %r" % self)

def escapedecl(self):
    if isinstance(self, Fun):
        self.stmts = [escapestmt(stmt)
            for stmt in self.stmts]
    elif isinstance(self, Extern):
        pass
    elif isinstance(self, Export):
        pass
    elif isinstance(self, Def):
        pass
    elif isinstance(self, Type):
        pass
    else:
        raise NotImplementedError("escapedecl not implemented for %r" % self)

def escapecheck(scope):
    for sym in scope:
        escapedecl(sym.decl)