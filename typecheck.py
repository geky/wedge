from syntax import *
from type import *
from eval import eval
from util import CompileException


class TypeException(CompileException):
    pass

def typeexpect(self, expected, line=None):
    if expected is None:
        return self
    elif isinstance(expected, list) and isinstance(self, list):
        if len(expected) != len(self):
            raise TypeException("mismatched types %s and %s" %
                (self, expected), line or self)
        return [typeexpect(s, t, line) for s, t in zip(self, expected)]
    elif isinstance(expected, IntT) and isinstance(self, IntT):
        return self
    elif isinstance(expected, FunT) and isinstance(self, FunT):
        return FunT(typeexpect(self.args, expected.args, line),
            typeexpect(self.rets, expected.rets, line))
    elif isinstance(self, Sym):
        if not isinstance(self.type, TypeT):
            raise TypeException("not a type %r" %
                self.type, line or self)

        self.type = typeexpect(self.type, expected, line)
        return self.type
    else:
        raise TypeException("mismatched types %s and %s" %
            (self, expected), line or self)

def typeframe(self, expected=None):
    if isinstance(self, Call):
        argtypes = [typeexpr(expr) for expr in self.exprs]
        ftype = typeexpr(self.sym, FunT(argtypes, None))
        rettypes = typeexpect(ftype.rets, expected)

        self.types = rettypes
        self.type = rettypes[0] # Hmm
        return self.types
    elif isinstance(self, Num):
        self.type = typeexpect([IntT()], expected, self)[0]
        return [self.type]
    elif isinstance(self, Sym):
        self.type = typeexpect([self.type], expected, self)[0]
        return [self.type]
    else:
        raise NotImplementedError("typeexpr not implemented for %r" % self)

def typeexpr(self, expected=None):
    return typeframe(self, [expected])[0]

def typeexprs(self, expected=None):
    if len(self) == 1:
        return typeframe(self[0], expected)
    elif expected is None:
        return [typeexpr(expr) for expr in self]
    else:
        if len(self) != len(expected):
            raise TypeException("mismatched types %s and %s" %
                (self, expected), self)
        return [typeexpr(expr, t) for expr, t in zip(self, expected)]

def typestmt(self):
    if isinstance(self, Let):
        expected = [getattr(sym, 'type', None) for sym in self.syms]
        types = typeexprs(self.exprs, expected)

        for sym, type in zip(self.syms, types):
            sym.type = type
    elif isinstance(self, Def):
        assert self.type
        self.sym.type = eval(self.type)
    elif isinstance(self, Return):
        expected = self.scope['return', 'types']
        self.types = typeexprs(self.exprs, expected)
    elif isinstance(self, Expr):
        typeexprs(self.exprs)
    else:
        raise NotImplementedError("typestmt not implemented for %r" % self)

def typedecl(self):
    if isinstance(self, Fun):
        type = typeexpect(self.sym.type, FunT([None for _ in self.args], None))

        self.ret.types = type.rets
        for arg, argtype in zip(self.args, type.args):
            arg.type = argtype

        for stmt in self.stmts:
            typestmt(stmt)

        self.sym.type = type
    elif isinstance(self, Type):
        for stmt in self.stmts:
            typestmt(stmt)

        self.sym.type = TypeT()
        self.ctor.sym.type = FunT([stmt.sym.type for stmt in self.stmts], [self.sym])
        typedecl(self.ctor)
    elif isinstance(self, Extern):
        assert self.type
        self.sym.type = eval(self.type)
    elif isinstance(self, Export):
        pass
    elif isinstance(self, Def):
        assert self.type
        self.sym.type = eval(self.type)
    else:
        raise NotImplementedError("typedecl not implemented for %r" % self)

def typecheck(scope):
    for sym in scope:
        typedecl(sym.decl)
