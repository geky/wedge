from wsyntax import *
from wtypes import *
from util import CompileException
from scopecheck import Var
from itertools import chain, groupby


class TypeException(CompileException):
    pass

def typecomplete(self):
    if self is None:
        return False
    elif isinstance(self, list):
        return all(typecomplete(t) for t in self)
    elif isinstance(self, IntT):
        return True
    elif isinstance(self, TypeT):
        return True
    elif isinstance(self, FunT):
        return typecomplete(self.args) and typecomplete(self.rets)
    elif isinstance(self, Sym):
        return True
    elif isinstance(self, InterfaceT):
        return True # TODO hmm
    else:
        raise NotImplementedError("typecomplete not implemented for %r" % self)

def typeifacecount(self):
    if self is None:
        return 0
    elif isinstance(self, list):
        return sum(typeifacecount(s) for s in self)
    elif isinstance(self, IntT):
        return 0
    elif isinstance(self, TypeT):
        return 0
    elif isinstance(self, FunT):
        return typeifacecount(self.args) + typeifacecount(self.rets)
    elif isinstance(self, Sym):
        return 0
    elif isinstance(self, InterfaceT):
        return 1

def typeifacecheck(iface, impl):
    for sym, type in iface.funs:
        sym.var.disclude = True
        ntype = type.sub(iface.sym, impl)
        typeselect(sym.scope, sym, ntype)
        sym.var.disclude = False

def typeexpect(self, expected, line=None):
    if expected is None or self is None:
        return self or expected
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
        return FunT(
            typeexpect(self.args, expected.args, line),
            typeexpect(self.rets, expected.rets, line))
    elif isinstance(expected, Sym) and isinstance(self, Sym):
        if self != expected:
            raise TypeException("mismatched types %s and %s" %
                (self, expected), line or self)

        return self
    elif isinstance(expected, InterfaceT) and isinstance(self, InterfaceT):
        iid = InterfaceT.getiid()
        funs = (
            {(sym, type.sub(self.sym,     iid)) for sym, type in self.funs    } |
            {(sym, type.sub(expected.sym, iid)) for sym, type in expected.funs})

        if len(funs) == len(self.funs):
            self.impls.update(expected.impls)
            return self
        elif len(funs) == len(expected.funs):
            expected.impls.update(self.impls)
            return expected
        else:
            type = InterfaceT(iid, funs, self.impls | expected.impls)
            return type

    elif isinstance(expected, InterfaceT) or isinstance(self, InterfaceT):
        if not isinstance(expected, InterfaceT):
            expected, self = self, expected

        if self in expected.impls:
            return self
        else:
            # try to find implementation of expected functions
            expected.impls.add(self)
            try:
                typeifacecheck(expected, self)
            except TypeException:
                expected.impls.remove(self)
                raise

            return self
    else:
        raise TypeException("mismatched types %s and %s" %
            (self, expected), line or self)

def typeinterfy(scope, overloads, line=None):
    assert len(overloads) > 1 and all(
        var.sym == overloads[0][0].sym for var, _ in overloads[1:])

    if not (all(isinstance(type, FunT)
                for _, type in overloads) and
            all(len(type.args) == len(overloads[0][1].args)
                for _, type in overloads[1:]) and
            all(type.rets == overloads[0][1].rets
                for _, type in overloads[1:])):
        raise TypeException(
            '\n'.join(chain(["not interface compatible"],
                ("%r line %d" % (var.type, var.line) for var, _ in overloads))), line)

    ifaces = []
    impls = []
    args = []
    for argset in zip(*[type.args for _, type in overloads]):
        if not all(arg == argset[0] for arg in argset[1:]):
            sym = InterfaceT.getiid()
            ifaces.append(sym)
            impls.append({argset})
            args.append(sym)
        else:
            args.append(argset[0])

    if len(ifaces) != 1:
        raise TypeException(
            '\n'.join(chain(["not interface compatible"],
                ("%r line %d" % (var.type, getattr(var, 'line', 0))
                    for var, _ in overloads))), line)

    funsym = Sym(overloads[0][0].sym)
    funtype = FunT(args, overloads[0][1].rets)
    ifacesym = ifaces[0]
    ifacetype = InterfaceT(ifacesym, {(funsym, funtype)}, impls[0])

    scope.globals().bind(funsym,
        type=FunT([
            arg if arg != ifacesym else ifacetype
            for arg in funtype.args], overloads[0][1].rets),
        impl=RawFunImpl([
            "; TODO implicit interface function",
            "ret i32 0",
        ]),
    )

    return funsym.var, funsym.var.type

def typeexpansions(overloads):
    overloads = [(var, var.type) for var in overloads
        if not getattr(var, 'disclude', False)]

    while overloads:
        for _, layer in groupby(overloads, lambda v: typeifacecount(v[1])):
            yield layer

        expanded = []
        for var, type in overloads:
            type, x = type.expand()
            if x:
                expanded.append((var, type))

        overloads = expanded

def typeselect(scope, sym, expected=None):
    overloads = []
    for var in scope.getoverloads(sym):
        if not hasattr(var, 'type'):
            typevar(var)
            assert var.type

        overloads.append(var)

    # Should have been a scope exception
    assert len(overloads) > 0

    # Find single function if we can
    interfycandidates = None
    for expansions in typeexpansions(overloads):
        options = []
        for var, type in expansions:
            try:
                type = typeexpect(type, expected)
                options.append((var, type))
            except TypeException:
                pass
            
        if len(options) == 1:
            return options[0]
        elif len(options) > 1 and interfycandidates is None:
            interfycandidates = options

    # Interfy if possible
    if interfycandidates is not None:
        try:
            # ambiguous, so try to make an interface
            return typeinterfy(scope, interfycandidates)
        except TypeException:
            raise TypeException(
                '\n'.join(chain(["ambiguous overload for %r, %r" % (sym, expected)],
                    ('%r line %d' % (var.type, getattr(var, 'line', 0)) for var, _ in options))), sym)

    raise TypeException(
        '\n'.join(chain(["no valid overload for %r, %r" % (sym, expected)],
            ('%r line %d' % (var.type, getattr(var, 'line', 0)) for var in overloads))), sym)

def typeframe(self, expected=None):
    if isinstance(self, Call):
        argtypes = [typeexpr(expr) for expr in self.exprs]
        ftype = typeexpr(self.callee, FunT(argtypes, None))

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
        expected = typeexpect([None], expected, self) # Hm
        self.var, self.type = typeselect(self.scope, self, expected[0])

        if self.var.local:
            self.var.type = typeexpect(self.var.type, self.type, self) # Hm
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
        expected = [getattr(sym.var, 'type', None) for sym in self.targets]
        types = typeexprs(self.exprs, expected)

        for sym, type in zip(self.targets, types):
            sym.var.type = type
    elif isinstance(self, Def):
        for sym, expr in zip(self.targets, self.exprs):
            typeexpr(expr, TypeT())
            sym.var.type = expr.eval()
    elif isinstance(self, Impl):
        typeexpr(self.sym, TypeT())
        interface = self.sym.eval()

        # try to find implementation of expected functions
        interface.impls.add(decl.sym)
    elif isinstance(self, Return):
        expected = self.scope['.rets'].types
        self.types = typeexprs(self.exprs, expected)
        self.scope['.rets'].types = self.types
    elif isinstance(self, Expr):
        typeexprs(self.exprs)
    else:
        raise NotImplementedError("typestmt not implemented for %r" % self)

def typedecl(self):
    if isinstance(self, Fun):
        type = typeexpect(getattr(self.sym.var, 'type', None),
            FunT([None for _ in self.args], None))

        self.rets.var.types = type.rets
        for arg, argtype in zip(self.args, type.args):
            arg.var.type = argtype

        for stmt in self.stmts:
            typestmt(stmt, self)

        type.rets = self.rets.var.types
        if type.rets is None or any(ret is None for ret in type.rets):
            raise TypeException("could not infer return type for \"%s\"" %
                self.sym, self)

        type.args = [arg.var.type for arg in self.args]
        self.sym.var.type = type
    elif isinstance(self, Type):
        self.sym.var.type = TypeT()
        for stmt in self.stmts:
            typestmt(stmt, self)

        self.ctor.var.type.args = [sym.var.type
            for sym in stmt.targets
            for stmt in self.stmts if isinstance(stmt, Def)]
    elif isinstance(self, Interface):
        self.sym.var.type = TypeT()
        for stmt in self.stmts:
            typestmt(stmt, self)

        self.sym.var.value = InterfaceT(self.sym,
            {(var.sym, var.type) for var in self.nscope.locals()})

        for var in self.nscope.locals():
            var.impl = RawFunImpl([
                "; TODO interface function",
                "ret i32 0"
            ])
    elif isinstance(self, Extern):
        for sym, expr in zip(self.targets, self.exprs):
            typeexpr(expr, TypeT())
            sym.var.type = expr.eval()
    elif isinstance(self, Export):
        pass
    elif isinstance(self, Def):
        for sym, expr in zip(self.targets, self.exprs):
            typeexpr(expr, TypeT())
            sym.var.type = expr.eval()
    else:
        raise NotImplementedError("typedecl not implemented for %r" % self)

def typevar(self):
    assert isinstance(self, Var)
    if not hasattr(typevar, 'active'):
        typevar.active = set()

    if self in typevar.active:
        raise TypeException("Can't infer recursive type for %r" % self.sym, self)
    typevar.active.add(self)

    for decl in self.decls:
        typedecl(decl)

    typevar.active.remove(self)

def typecheck(scope):
    for var in scope:
        typevar(var)

    for iface in scope.itervalues():
        if isinstance(iface, InterfaceT):
            for impl in iface.impls:
                typeifacecheck(iface, impl)

    for expr in scope.iterexprs():
        assert expr.type
