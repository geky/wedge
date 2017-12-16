from syntax import *
from type import *

class TypeException(Exception):
    pass

def typeassert(v):
    if not hasattr(v, 'type') or not v.type:
        raise TypeException("No type for %s" % v)

def typeexpr(self):
    if isinstance(self, Call):
        ftype = typeexpr(self.sym)
        if not isinstance(ftype, FunT):
            raise TypeException("Trying to call non-function %s" % ftype)

        self.type = ftype.rets[0]

        if len(self.exprs) != len(ftype.args):
            raise TypeException("mismatched call %s and %s" %
                (self.exprs, ftype.args))
        for expr, expected in zip(self.exprs, ftype.args):
            type = typeexpr(expr)
            if type != expected:
                raise TypeException("argument does not match type %s != %s" %
                    (type, expected))

        return self.type
    elif isinstance(self, Num):
        self.type = IntT()
        return self.type
    elif isinstance(self, Sym):
        v = self.lookup()
        typeassert(v)
        self.type = v.type
        return v.type
    else:
        raise NotImplementedError("typeexpr not implemented for %r" % self)

def typestmt(self):
    if isinstance(self, Let):
        expected = None
        if self.sym in self.scope and isinstance(self.scope[self.sym], Def):
            expected = self.scope[self.sym].type

        self.type = typeexpr(self.expr)
        if expected and self.type != expected:
            raise TypeException("mismatched type %s != %s" % (self.type, expected))

        return self.type
    elif isinstance(self, Def):
        typeassert(self)
        return self.type
    elif isinstance(self, Return):
        self.type = typeexpr(self.exprs[0])
        return self.type
    else:
        typeexpr(self)

def typedecl(self):
    if isinstance(self, Fun):
        if self.sym in self.scope and isinstance(self.scope[self.sym], Def):
            self.type = self.scope[self.sym].type
        typeassert(self)

        if not isinstance(self.type, FunT):
            raise TypeException("Not a function type %s" % self.type)

        if len(self.args) != len(self.type.args):
            raise TypeException("mismatched function arguments %s and %s" %
                (self.args, self.type.args))
        for arg, type in zip(self.args, self.type.args):
            arg.type = type

        for stmt in self.stmts:
            typestmt(stmt)

        return self.type
    elif isinstance(self, Extern):
        typeassert(self)
        return self.type
    elif isinstance(self, Def):
        typeassert(self)
        return self.type
    else:
        raise NotImplementedError("typedecl not implemented for %r" % self)

def typecheck(scope):
    for name, decl in scope:
        typedecl(decl)

    return scope
