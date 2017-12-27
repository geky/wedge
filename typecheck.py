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
    elif isinstance(self, InterfaceT):
        if expected in self.impls:
            return self
        else:
            for sym, type in self.funs:
                # try to find implementation of expected functions
                # TODO make this cleaner?
                nsym = Sym(sym.name)
                sym.scope.bind(nsym)
                ntype = type.sub(self.sym, expected)
                res = typeexpr(nsym, ntype)

            self.impls.add(expected)
            return self
    elif isinstance(expected, InterfaceT):
        if self in expected.impls:
            return expected
        else:
            for sym, type in expected.funs:
                # try to find implementation of expected functions
                # TODO make this cleaner?
                nsym = Sym(sym.name)
                sym.scope.bind(nsym)
                ntype = type.sub(expected.sym, self)
                res = typeexpr(nsym, ntype)

            expected.impls.add(self)
            return expected
    else:
        raise TypeException("mismatched types %s and %s" %
            (self, expected), line or self)

def typeinterfy(overloads, line=None):
    assert len(overloads) > 1 and all(sym == overloads[0][0] for sym, _ in overloads[1:])
    if not (all(isinstance(fun, FunT) for _, fun in overloads) and
            all(len(fun.args) == len(overloads[0][1].args) for _, fun in overloads[1:]) and
            all(fun.rets == overloads[0][1].rets for _, fun in overloads[1:])):
        raise TypeException(
            '\n'.join(["not interface compatible"] +
                ["%r" % type for _, type in overloads]), line)

    ifaces = []
    impls = []
    args = []
    for argset in zip(*[fun.args for _, fun in overloads]):
        if not all(arg == argset[0] for arg in argset[1:]):
            sym = InterfaceT.getiid()
            ifaces.append(sym)
            impls.append(set(argset))
            args.append(sym)
        else:
            args.append(arg)

    if len(ifaces) != 1:
        raise TypeException(
            '\n'.join(["not interface compatible"] +
                ["%r" % type for _, type in overloads]), line)

    # TODO make scope work out? needs code injection
    fun = FunT(args, overloads[0][1].rets)
    iface = InterfaceT(ifaces[0], [fun], impls[0])
    return overloads[0][0], fun

def typeselect(self, overloads, expected, line=None):
    assert len(overloads) > 0
    original_overloads = overloads

    while True:
        options = []
        for sym, type in overloads:
            try:
                type = typeexpect(type, expected, line)
                options.append((sym, type))
            except TypeException:
                pass
            
        if len(options) == 1:
            return options[0]
        elif len(options) > 1:
            try:
                return typeinterfy(options)
            except TypeException:
                raise TypeException(
                    '\n'.join(["ambiguous overload for %r, %r" % (self, expected)] +
                        ['%r' % type for _, type in options]), line or self)

        expanded = []
        for sym, type in overloads:
            ntype, x = type.expand()
            if x:
                expanded.append((sym, ntype))

        if not expanded:
            break

        overloads = expanded

    raise TypeException(
        '\n'.join(["no valid overload for %r, %r" % (self, expected)] +
            ['%r' % type for _, type in original_overloads]), line or self)

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
            self.type = typeexpect([getattr(self, 'type', None)], expected, self)[0]
            self.constraints.append(self.type)
            return [self.type]
        else:
            overloads = []
            for sym in self.scope.filter(self, 'impl'):
                if not hasattr(sym, 'type'):
                    for decl in sym.decls:
                        # Catches recursive types
                        # TODO decide if this is a hack or not
                        if sym in typedecl.active:
                            raise TypeException("Can't resolve recursive type", sym)

                        typedecl(decl)

                if not hasattr(sym, 'type'):
                    raise TypeException("Unable to infer type %r" %
                        sym, sym)

                overloads.append((sym, sym.type))

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

def typestmt(self, decl):
    if isinstance(self, Let):
        expected = [getattr(sym, 'type', None) for sym in self.syms]
        types = typeexprs(self.exprs, expected)

        for sym, type in zip(self.syms, types):
            sym.type = type
    elif isinstance(self, Def):
        for sym, expr in zip(self.syms, self.exprs):
            typeexpr(expr, TypeT())
            sym.type = eval(expr)
    elif isinstance(self, Impl):
        typeexpr(self.sym, TypeT())
        interface = eval(self.sym)
        for sym, type in interface.funs:
            # try to find implementation of expected functions
            # TODO make this cleaner?
            nsym = Sym(sym.name)
            self.scope.bind(nsym)
            ntype = type.sub(interface.sym, decl.sym)
            res = typeexpr(nsym, ntype)

        interface.impls.add(decl.sym)
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
            typestmt(stmt, self)

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
        typedecl.active.add(self.sym)

        self.sym.type = TypeT()
        for stmt in self.stmts:
            typestmt(stmt, self)

        self.ctor.sym.type = FunT([sym.type
            for sym in stmt.syms
            for stmt in self.stmts if isinstance(stmt, Def)], [self.sym])
        typedecl(self.ctor)

        typedecl.active.remove(self.sym)
    elif isinstance(self, Interface):
        typedecl.active.add(self.sym)

        self.sym.type = TypeT()
        for stmt in self.stmts:
            typestmt(stmt, self)

        for fun, type in zip(self.funs, (sym.type
                for stmt in self.stmts
                for sym in stmt.syms)):
            fun.sym.type = type
            fun.args = [Sym('_') for _ in type.args]
            for arg in fun.args:
                self.scope.bind(arg)
            typedecl(fun)

        self.sym.value = InterfaceT(self.sym,
            [(fun.sym, fun.sym.type) for fun in self.funs])

        typedecl.active.remove(self.sym)
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
