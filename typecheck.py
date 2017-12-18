from syntax import *
from type import *
from util import CompileException


class TypeException(CompileException):
    pass

def typecompat(self, other):
    self = typetype(self)
    other = typetype(other)

    if self is None or other is None:
        return True
    elif isinstance(self, list) and isinstance(other, list):
        if len(self) != len(other):
            return False
        else:
            return all(typecompat(s, o) for s, o in zip(self, other))
    else:
        return self == other

def typec(self, other, line=None):
    if not typecompat(self, other):
        raise TypeException("mismatched types %r and %r" % (self, other), line)

    return self

def typetype(self):
    if self is None:
        return self
    elif isinstance(self, list):
        return [typetype(s) for s in self]
    elif isinstance(self, IntT):
        return self
    elif isinstance(self, FunT):
        return self
    elif isinstance(self, StructT):
        return self
    elif isinstance(self, Sym):
        assert self.type == TypeT()
        if not isinstance(self.type, TypeT):
            raise TypeException("not a type %r" % self.type, self)
        return self
    else:
        raise NotImplementedError("typetype not implemented for %r" % self)

def typeof(self):
    if self is None:
        return None
    elif isinstance(self, list):
        return [typeof(s) for s in self]
    else:
        return typetype(self.type)

def typeexprn(self):
    if isinstance(self, Call):
        ftype = typeexpr(self.sym)
        if isinstance(ftype, TypeT):
            self.sym = self.scope['%s.ctor' % self.sym.name]
            ftype = self.sym.type

        if not isinstance(ftype, FunT):
            raise TypeException("trying to call non-function %r" % ftype, self)

        for expr in self.exprs:
            typeexpr(expr)

        typec(typeof(self.exprs), ftype.args, self)
        self.types = ftype.rets
        self.type = self.types[0] # Hmm
        return self.types
    elif isinstance(self, Num):
        self.type = IntT()
        return [self.type]
    elif isinstance(self, Sym):
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
        types = typeexprs(self.exprs)
        if len(types) != len(self.syms):
            raise TypeException("mismatched let %s and %s" %
                (self.syms, self.exprs), self.line)
            
        for sym, type in zip(self.syms, types):
            expected = getattr(sym, 'type', None)
            if expected and type != expected:
                raise TypeException("mismatched types %s and %s" %
                    (type, expected), self)

            sym.type = type
    elif isinstance(self, Def):
        assert self.type
        self.sym.type = typetype(self.type)
    elif isinstance(self, Return):
        rets = self.scope['return', 'types']
        self.types = typeexprs(self.exprs)
        if len(self.types) != len(rets):
            raise TypeException("mismatched return %s and %s" %
                (self.types, rets), self)

        for type, expected in zip(self.types, rets):
            if expected and type != expected:
                raise TypeException("mismatched types %s and %s" %
                    (type, expected), self)
    elif isinstance(self, Expr):
        typeexprs(self.exprs)
    else:
        raise NotImplementedError("typestmt not implemented for %r" % self)

def typedecl(self):
    if isinstance(self, Fun):
        type = self.sym.type
        if not isinstance(type, FunT):
            raise TypeException("not a function type %r" % type, self)
        if len(self.args) != len(type.args):
            raise TypeException("mismatched function arguments %s and %s" %
                (self.args, type.args), self)

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
        self.sym.type = typetype(self.type)
    elif isinstance(self, Export):
        pass
    elif isinstance(self, Def):
        assert self.type
        self.sym.type = typetype(self.type)
    else:
        raise NotImplementedError("typedecl not implemented for %r" % self)

def typecheck(scope):
    for name in scope:
        if hasattr(name, 'decl'):
            typedecl(name.decl)

    return scope
