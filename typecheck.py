from syntax import *
from type import *

class TypeException(Exception):
    pass

def typeassert(v):
    if not hasattr(v, 'type') or not v.type:
        raise TypeException("No type for %s line %d" % v)

def typeexprn(self):
    if isinstance(self, Call):
        ftype = typeexpr(self.sym)
        if not isinstance(ftype, FunT):
            raise TypeException("Trying to call non-function %s line %d" % (ftype, self.line))

        self.types = ftype.rets

        if len(self.exprs) != len(ftype.args):
            raise TypeException("mismatched call %s and %s" %
                (self.exprs, ftype.args))
        for expr, expected in zip(self.exprs, ftype.args):
            type = typeexpr(expr)
            if type != expected:
                raise TypeException(
                    "argument does not match type %s != %s line %d" % (type, expected, self.line))

        return self.types
    elif isinstance(self, Num):
        self.type = IntT()
        return [self.type]
    elif isinstance(self, Sym):
        self.type = self.gettype()
        return [self.type]
    else:
        raise NotImplementedError("typeexpr not implemented for %r" % self)

def typeexpr(self):
    # TODO, should there be discard work?
    types = typeexprn(self)
    return types[0]

def typeexprs(self):
    if len(self) == 0:
        return []
    if len(self) == 1:
        return typeexprn(self[0])
    else:
        return [typeexpr(expr) for expr in self]

def typestmt(self):
    if isinstance(self, Let):
        self.types = typeexprs(self.exprs)
        if len(self.types) != len(self.syms):
            raise TypeException("mismatched let %s and %s line %d" % (self.syms, self.exprs, self.line))
            
        for sym, type in zip(self.syms, self.types):
            if sym in self.scope and isinstance(self.scope[sym], Def):
                expected = self.scope[sym].type
            else:
                expected = None

            if expected and type != expected:
                raise TypeException(
                    "mismatched type %s != %s" % (type, expected))

            sym.type = type
    elif isinstance(self, Def):
        typeassert(self)
        self.sym.type = type
    elif isinstance(self, Return):
        rets = self.scope.getfun().type.rets
        self.types = typeexprs(self.exprs)

        if len(self.types) != len(rets):
            raise TypeException("mismatched return %s and %s line %d" % (self.types, rets, self.line))

        for type, expected in zip(self.types, rets):
            if expected and type != expected:
                raise TypeException(
                    "mismatched type %s != %s" % (type, expected))
    elif isinstance(self, Expr):
        typeexprs(self.exprs)
    else:
        raise NotImplementedError("typestmt not implemented for %r" % self)

def typedecl(self):
    if isinstance(self, Fun):
        if self.sym in self.scope and isinstance(self.scope[self.sym], Def):
            self.type = self.scope[self.sym].type
        typeassert(self)
        if not isinstance(self.type, FunT):
            raise TypeException("Not a function type %s line %d" % (self.type, self.line))

        if len(self.args) != len(self.type.args):
            raise TypeException("mismatched function arguments %s and %s line %d" %
                (self.args, self.type.args, self.line))
        for arg, type in zip(self.args, self.type.args):
            arg.type = type

        for stmt in self.stmts:
            typestmt(stmt)

        self.sym.type = self.type
    elif isinstance(self, Extern):
        typeassert(self)
        self.sym.type = self.type
    elif isinstance(self, Export):
        self.type = None
        self.sym.type = self.type
    elif isinstance(self, Def):
        typeassert(self)
        self.sym.type = self.type
    else:
        raise NotImplementedError("typedecl not implemented for %r" % self)

def typecheck(scope):
    for name, decl in scope:
        typedecl(decl)

    return scope
