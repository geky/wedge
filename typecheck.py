from wsyntax import *
from wtypes import *
from eval import eval
from util import CompileException


class TypeException(CompileException):
    pass

def typeexpect(self, expected, line=None):
    if expected is None:
        return self
    elif self is None:
        return expected
    elif isinstance(expected, list) and isinstance(self, list):
        if len(expected) != len(self):
            raise TypeException("mismatched types %s and %s" %
                (self, expected), line or self)
        return [typeexpect(s, t, line) for s, t in zip(self, expected)]
    elif isinstance(expected, TypeT) and isinstance(self, TypeT):
        return self
    elif isinstance(expected, IntT) and isinstance(self, IntT):
        return self
    elif isinstance(expected, FunT) and isinstance(self, FunT):
        return FunT(typeexpect(self.args, expected.args, line),
            typeexpect(self.rets, expected.rets, line))
    elif isinstance(expected, Sym) and isinstance(self, Sym):
        if not isinstance(self.type, TypeT):
            raise TypeException("not a type %r" %
                self.type, line or self)

        if self != expected:
            raise TypeException("mismatched types %s and %s" %
                (self, expected), line or self)

        return self
    else:
        raise TypeException("mismatched types %s and %s" %
            (self, expected), line or self)

def typeselect(self, overloads, expected, line=None):
    assert len(overloads) > 0

    if len(overloads) == 1:
        sym, type = overloads[0]
        type = typeexpect(type, expected, line)
        return sym, type
    else:
        options = []
        for sym, type in overloads:
            try:
                type = typeexpect(type, expected, line)
                options.append((sym, type))
            except TypeException:
                pass

        if len(options) == 0:
            raise TypeException(
                '\n'.join(["no valid overload for %r, %r" % (self, expected)] +
                    ['%r' % type for _, type in overloads]), line or self)
        elif len(options) > 1:
            raise TypeException(
                '\n'.join(["ambiguous overload for %r, %r" % (self, expected)] +
                    ['%r' % type for _, type in options]), line or self)
        else:
            return options[0]

def typeframe(self, expected=None):
    if isinstance(self, Call):
        # TODO, grody hack, should this go somewhere else?
        try:
            ftype = typeexpr(self.sym, None)
            if isinstance(ftype, TypeT):
                ctor = Sym('%s.ctor' % self.sym.name)
                self.scope.bind(ctor, type=None)
                self.sym = ctor
        except TypeException:
            pass

        argtypes = [typeexpr(expr) for expr in self.exprs]
        ftype = typeexpr(self.sym, FunT(argtypes, None))

        argtypes = [typeexpr(expr, arg)
            for expr, arg in zip(self.exprs, ftype.args)]
        rettypes = typeexpect(ftype.rets, expected)

        self.types = rettypes
        self.type = rettypes[0] # Hmm
        return self.types
    elif isinstance(self, Num):
        self.type = typeexpect([IntT()], expected, self)[0]
        return [self.type]
    elif isinstance(self, IntT):
        self.type = typeexpect([TypeT()], expected, self)[0]
        return [self.type]
    elif isinstance(self, FunT):
        for arg in self.args:
            typeexpr(arg, TypeT())
        for ret in self.rets:
            typeexpr(ret, TypeT())

        self.type = typeexpect([TypeT()], expected, self)[0]
        return [self.type]
    elif isinstance(self, Sym):
        if getattr(self, 'local', True):
            self.type = typeexpect([self.type], expected, self)[0]
            self.constraints.append(self.type)
            return [self.type]
        else:
            if not hasattr(self, 'type'):
                for decl in self.decls:
                    # Catches recursive types
                    # TODO decide if this is a hack or not
                    if self in typedecl.active:
                        raise TypeException("Can't resolve recursive type", self)

                    typedecl(decl)

                if not hasattr(self, 'type'):
                    raise TypeException("Unable to infer type %r" %
                        self, self)

            overloads = [(sym, sym.type) for sym in self.scope.filter(self, 'impl')]

            expected = typeexpect([None], expected, self) # TODO hm
            sym, type = typeselect(self, overloads, expected[0])
            self.scope = sym.nscope
            self.type = type
            self.constraints.append(self.type)
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
        for sym, expr in zip(self.syms, self.exprs):
            typeexpr(expr, TypeT())
            sym.type = eval(expr)
    elif isinstance(self, Return):
        expected = self.scope['return'].types
        self.types = typeexprs(self.exprs, expected)
        self.scope['return'].types = self.types
    elif isinstance(self, Expr):
        typeexprs(self.exprs)
    else:
        raise NotImplementedError("typestmt not implemented for %r" % self)

def typedecl(self):
    # TODO memoize?
    if isinstance(self, Fun):
        typedecl.active.add(self.sym)

        type = typeexpect(getattr(self.sym, 'type', None),
            FunT([None for _ in self.args], None))

        self.ret.types = type.rets
        for arg, argtype in zip(self.args, type.args):
            arg.type = argtype

        for stmt in self.stmts:
            typestmt(stmt)

        type.rets = self.ret.types
        if type.rets is None or any(ret is None for ret in type.rets):
            raise TypeException("could not infer return type for \"%s\"" %
                self.sym.name, self)

        for arg in self.args:
            for constraint in arg.constraints:
                arg.type = typeexpect(arg.type, constraint)
        type.args = [arg.type for arg in self.args]

        self.sym.type = type

        typedecl.active.remove(self.sym)
    elif isinstance(self, Type):
        for stmt in self.stmts:
            typestmt(stmt)

        self.sym.type = TypeT()
        self.ctor.sym.type = FunT([sym.type
            for sym in stmt.syms
            for stmt in self.stmts], [self.sym])
        typedecl(self.ctor)
    elif isinstance(self, Extern):
        for sym, expr in zip(self.syms, self.exprs):
            typeexpr(expr, TypeT())
            sym.type = eval(expr)
    elif isinstance(self, Export):
        pass
    elif isinstance(self, Def):
        for sym, expr in zip(self.syms, self.exprs):
            typeexpr(expr, TypeT())
            sym.type = eval(expr)
    else:
        raise NotImplementedError("typedecl not implemented for %r" % self)

typedecl.active = set()

def typecheck(scope):
    for sym in scope:
        typedecl(sym.decl)
