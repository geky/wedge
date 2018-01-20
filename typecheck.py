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
        return False
    elif isinstance(self, ParamedT):
        assert len(self.syms) > 0
        return False
    elif isinstance(self, Call):
        return typecomplete(self.callee) and typecomplete(self.exprs)
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
    elif isinstance(self, StructT):
        return 0
    elif isinstance(self, Sym):
        return 0
    elif isinstance(self, InterfaceT):
        return 1
    elif isinstance(self, ParamedT):
        return typeifacecount(self.type)
    elif isinstance(self, Call):
        return typeifacecount(self.callee) + typeifacecount(self.exprs)
    else:
        raise NotImplementedError("typeifacecount not implemented for %r" % self)

def typeifacecheck(iface, impl):
    impltype = typeexpect(impl, None)

    for sym, type in iface.funs:
        sym.var.disclude = True
        ntype = type.sub(getattr(iface.sym, 'replacement', iface.sym), impl)
        # TODO using replacement is hacky, see below
        typeselect(sym.scope, sym, ntype)
        sym.var.disclude = False

#TODO separate self and expected params
def typeexpect(self, expected, line=None, params=None):
    if expected is None or self is None:
        return self or expected
    if isinstance(expected, list) and isinstance(self, list):
        if len(expected) != len(self):
            raise TypeException("mismatched types %s and %s" %
                (self, expected), line or self)
        return [typeexpect(s, t, line, params) for s, t in zip(self, expected)]
    if isinstance(expected, TypeT) and isinstance(self, TypeT):
        return self
    if isinstance(expected, IntT) and isinstance(self, IntT):
        return self
    if isinstance(expected, FunT) and isinstance(self, FunT):
        return FunT(
            typeexpect(self.args, expected.args, line, params),
            typeexpect(self.rets, expected.rets, line, params))
    if isinstance(expected, Sym) and isinstance(self, Sym) and self == expected:
        return self
    if isinstance(expected, Call) and isinstance(self, Call):
        try:
            callee = typeexpect(self.callee, expected.callee, line, params)
            c =  Call(callee, typeexpect(self.exprs, expected.exprs, line, params))
            return c
        except TypeException:
            pass
    if isinstance(expected, InterfaceT) and isinstance(self, InterfaceT):
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
    if (params is not None and (
            (isinstance(expected, Sym) and expected in params) or
            (isinstance(self, Sym) and self in params))):
        if not (isinstance(expected, Sym) and expected in params):
            expected, self = self, expected

        type = typeexpect(self, params[expected], line, params)
        params[expected] = type
        return type
    if isinstance(expected, InterfaceT) or isinstance(self, InterfaceT):
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
    if isinstance(expected, Call):
        # TODO use eval for callee (try except becomes a mess
        try:
            if (isinstance(expected, Call) and hasattr(expected.callee, 'var') and
                getattr(expected.callee.var, 'pure', False)):
                res = expected.callee.var.exec(*[expr for expr in expected.exprs])
                return typeexpect(self, res, line, params)
        except TypeException:
            pass
    if isinstance(self, Call):
        try:
            if (isinstance(self, Call) and hasattr(self.callee, 'var') and
                getattr(self.callee.var, 'pure', False)):
                res = self.callee.var.exec(*[expr for expr in self.exprs])
                return typeexpect(res, expected, line, params)
        except TypeException:
            pass
    if isinstance(expected, ParamedT) or isinstance(self, ParamedT):
        if not isinstance(expected, ParamedT):
            expected, self = self, expected

        # clone because python default arg mess
        params = params or {}
        params.update(dict.fromkeys(expected.syms, None))

        ntype = typeexpect(self, expected.type, line, params)
        nsyms = [sym for sym in expected.syms if not typecomplete(params[sym])]

        if len(nsyms) == 0:
            return ntype
        else:
            return ParamedT(nsyms, ntype)

    raise TypeException("mismatched types %r and %r" %
        (self, expected), line or self)

def typeinterfy(scope, overloads, line=None):
    assert len(overloads) > 1 and all(
        var.sym.name == overloads[0][0].sym.name for var, _ in overloads[1:]), (
        "Mismatched functions being interfyied:\n%r" % overloads)


    # TODO handle all ParamedTs
    # Make new ParamedT?
    if not (all(isinstance(type.raw(), FunT)
                for _, type in overloads) and
            all(len(type.raw().args) == len(overloads[0][1].raw().args)
                for _, type in overloads[1:]) and
            all(type.raw().rets == overloads[0][1].raw().rets
                for _, type in overloads[1:])):
        raise TypeException(
            '\n'.join(chain(["not interface compatible"],
                ("%r line %d" % (var.type, var.line) for var, _ in overloads))), line)

    ifaces = []
    impls = []
    args = []
    for argset in zip(*[type.raw().args for _, type in overloads]):
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
    funtype = FunT(args, overloads[0][1].raw().rets)
    ifacesym = ifaces[0]
    ifacetype = InterfaceT(ifacesym, {(funsym, funtype)}, impls[0])

    scope.globals().bind(funsym,
        type=FunT([
            arg if arg != ifacesym else ifacetype
            for arg in funtype.args], overloads[0][1].raw().rets),
        impl=RawFunImpl([
            "; TODO implicit interface function",
            "ret i32 0",
        ]),
    )

    return funsym.var, funsym.var.type

def typeexpansions(overloads):
    overloads = [(var, var.type) for var in overloads
        if not getattr(var, 'disclude', False)]
    iteration = 0

    while overloads:
        iteration += 1
        assert iteration < 1000, (
            "expansions did not halt after 1000 iterations:\n%s" %
            "\n".join("%r %r" % (var.sym, type) for var, type in overloads))

        for _, layer in groupby(overloads, lambda v: typeifacecount(v[1])):
            layer = list(layer)
            yield layer

        expanded = []
        for var, type in overloads:
            try:
                type, x = type.expand()
                if x:
                    expanded.append((var, type))
            except AttributeError:
                # This _usually_ means we tried to expand a None
                # this is a complete hack, but for now we just stick it on
                # the end of the expansions.
                # TODO add None support to expand (typeexpand?)
                expanded.append((var, type))

        overloads = expanded

def typeselect(scope, sym, expected=None):
    overloads = []
    for var in scope.getoverloads(sym):
        if not hasattr(var, 'type'):
            typevar(var)
            assert hasattr(var, 'type'), (
                "Var has no type during overload search:\n%r" % var)

        overloads.append(var)

    # Should have been a scope exception
    assert len(overloads) > 0, "no overloads for %r?" % sym

    # Find single function if we can
    interfycandidates = None
    for expansions in typeexpansions(overloads):
        options = []
        for var, type in expansions:
            try:
                type = typeexpect(type, expected)
                options.append((var, type))
            except TypeException as e:
                try:
                    type = typeexpect(type.eval(False), expected)
                    options.append((var, type))
                except EvalException:
                    pass
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
                    ('%r line %d' % (var.type, getattr(var, 'line', 0)) for var, _ in interfycandidates))), sym)

    raise TypeException(
        '\n'.join(chain(["no valid overload for %r, %r" % (sym, expected)],
            ('%r line %d' % (var.type, getattr(var, 'line', 0)) for var in overloads))), sym)

def typeframe(self, expected=None):
    if isinstance(self, Call):
        argtypes = [typeexpr(expr) for expr in self.exprs]
        ftype = typeexpr(self.callee, FunT(argtypes, None))

        argtypes = [typeexpr(expr, arg)
            for expr, arg in zip(self.exprs, ftype.raw().args)]
        rettypes = typeexpect(ftype.raw().rets, expected)

        self.types = rettypes
        self.type = rettypes[0] # Hmm
        return self.types
    elif isinstance(self, Init):
        argtypes = [typeexpr(expr) for expr in self.exprs]
        typeexpr(self.callee, TypeT())
        self.type = self.callee
        self.struct = self.type.eval(True)

        if not isinstance(self.struct, StructT):
            raise TypeException(
                "initializing not a struct %r" % self.callee, self.callee)

        for expr, (_, type) in zip(self.exprs, self.struct.fields):
            typeexpr(expr, type)

        return [self.type]
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
        for arg in self.args:
            arg.var.type = None

        typeexpr(self.expr, TypeT())
        self.sym.var.type = self.expr

        if len(self.args) > 0:
            self.sym.var.type = ParamedT(self.args, self.sym.var.type)

        # TODO this seems kinda hacky
        if isinstance(decl, Interface) and len(decl.args) > 0:
            self.sym.var.type = ParamedT(decl.args, self.sym.var.type)
    elif isinstance(self, Impl):
        typeexpr(self.expr, TypeT())

        try:
            interface = self.expr.eval(True)
        except EvalException:
            # TODO hm
            if (isinstance(self.expr, Call) and
                hasattr(self.expr.callee, 'var') and
                getattr(self.expr.callee.var, 'pure', False)):
                res = self.expr.callee.var.exec(*[expr for expr in self.expr.exprs])
                interface = res.eval(True)
            else:
                raise

        decl.sym.var.ifaces.append(interface)
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

        self.rets.var.types = type.raw().rets
        for arg, argtype in zip(self.args, type.raw().args):
            arg.var.type = argtype

        for stmt in self.stmts:
            typestmt(stmt, self)

        rets = self.rets.var.types
        if not typecomplete(rets):
            raise TypeException("could not infer return type for \"%s\"" %
                self.sym, self)

        args = [arg.var.type for arg in self.args]
        type = typeexpect(FunT(args, rets), type)
        self.sym.var.type = type
    elif isinstance(self, Struct):
        # TODO is this ok if we ref ourselves?
        # with params?
        self.sym.var.type = TypeT()
        self.sym.var.ifaces = []

        for arg in self.args:
            arg.var.type = None

        for stmt in self.stmts:
            typestmt(stmt, self)

        struct = StructT([(stmt.sym, stmt.sym.var.type)
            for stmt in self.stmts if isinstance(stmt, Def)])

        if len(self.args) == 0:
            self.sym.var.value = struct
            self.sym.var.ifaces = [(iface, self.sym)
                for iface in self.sym.var.ifaces]
        else:
            args = [TypeT() for arg in self.args]
            rets = [TypeT()]

            self.sym.var.type = FunT(args, rets)
            self.sym.var.pure = True
            def executor(*args):
                nstruct = struct
                for sym, arg in zip(self.args, args):
                    nstruct = nstruct.sub(sym, arg)

                sym = Sym('%s(%s)' % (self.sym, ','.join(map(str, args))))
                self.scope.globals().bind(sym, type=TypeT(), value=nstruct)
                return sym
            self.sym.var.exec = executor
            self.sym.var.ifaces = [(iface, Call(self.sym, self.args))
                for iface in self.sym.var.ifaces]
    elif isinstance(self, Interface):
        if len(self.args) == 0:
            self.sym.var.type = TypeT()
        else:
            self.sym.var.type = FunT([TypeT() for arg in self.args], [TypeT()])

        for arg in self.args:
            arg.var.type = None

        for stmt in self.stmts:
            typestmt(stmt, self)

        # TODO SUPER HACKY, move this to Interface?
        # Should all interfaces be assumed to have params?
        if len(self.args) == 0:
            self.sym.replacement = self.sym
        else:
            self.sym.replacement = Call(self.sym, self.args)

        interface = InterfaceT(self.sym,
            {(var.sym, var.type) for var in self.nscope.locals()})

        if len(self.args) == 0:
            self.sym.var.value = interface
        else:
            self.sym.var.pure = True
            def executor(*args):
                ninterface = interface
                for sym, arg in zip(self.args, args):
                    ninterface = ninterface.sub(sym, arg)

                return ninterface

            self.sym.var.exec = executor

        for var in self.nscope.locals():
            var.impl = RawFunImpl([
                "; TODO interface function",
                "ret i32 0"
            ])
    elif isinstance(self, Extern):
        for sym, expr in zip(self.targets, self.exprs):
            typeexpr(expr, TypeT())
            sym.var.type = expr
    elif isinstance(self, Export):
        pass
    elif isinstance(self, Def):
        for arg in self.args:
            arg.var.type = None

        typeexpr(self.expr, TypeT())
        self.sym.var.type = self.expr
        if len(self.args) > 0:
            self.sym.var.type = ParamedT(self.args, self.sym.var.type)
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

    for var in scope:
        if hasattr(var, 'ifaces'):
            for iface, impl in var.ifaces:
                typeifacecheck(iface, impl)

    for expr in scope.iterexprs():
        assert hasattr(expr, 'type'), (
            "%r has no type after typecheck" % expr)
