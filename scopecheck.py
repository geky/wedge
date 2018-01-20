from wsyntax import *
from wtypes import *
from util import CompileException
from itertools import chain

class ScopeException(CompileException):
    pass

class Var:
    def __init__(self, sym, **kwargs):
        if not isinstance(sym, Sym):
            sym = Sym(sym)

        if hasattr(sym, 'line'):
            self.line = sym.line

        self.sym = sym
        self.decls = []

        self.update(kwargs)

    def __repr__(self):
        return 'Var(%s)' % ', '.join(chain([repr(str(self.sym))],
            ('%s=%r' % (k, v)
                for k, v in self.__dict__.items()
                if k != 'sym')))

    def update(self, kwargs):
        if 'decl' in kwargs:
            self.decls.append(kwargs['decl'])
            del kwargs['decl']

        self.__dict__.update(kwargs)

class GlobalScope:
    def __init__(self):
        self.table = []

    def bind(self, sym, **kwargs):
        if isinstance(sym, Var):
            assert isinstance(sym.sym, Sym)
            sym.update(kwargs)
            sym.local = False
            sym.sym.var = sym
            sym.sym.scope = self
            self.table.append(sym)
            return

        if not isinstance(sym, Sym):
            sym = Sym(sym)

        for var in reversed(self.table):
            if sym.name == var.sym.name:
                if hasattr(var, 'impl'):
                    break

                var.update(kwargs)
                sym.var = var
                sym.scope = self
                return self

        sym.var = Var(sym, local=False, **kwargs)
        sym.scope = self
        self.table.append(sym.var)
        return self

    def getoverloads(self, sym):
        if not isinstance(sym, Sym):
            sym = Sym(sym)

        return [var for var in self.table if var.sym.name == sym.name]

    def __getitem__(self, sym):
        overloads = self.getoverloads(sym)

        if len(overloads) == 0:
            raise KeyError(sym)
        elif len(overloads) == 1:
            return overloads[0]
        else:
            return overloads

    def __contains__(self, sym):
        try:
            self[sym]
        except KeyError:
            return False
        else:
            return True

    def __bool__(self):
        return len(self.table) > 0

    def __iter__(self):
        yield from self.table

    def itervalues(self):
        for var in self:
            if hasattr(var, 'value'):
                yield var.value

    def iterdecls(self):
        for var in self:
            yield from var.decls

    def iterexprs(self):
        for decl in self.iterdecls():
            yield from decl.iterexprs()

    def itersyms(self):
        for decl in self.iterdecls():
            yield from decl.itersyms()

    def globals(self):
        return self

    def locals(self):
        return self

    def __repr__(self):
        return 'GlobalScope({%s})' % ', '.join(
            repr(str(var.sym)) for var in self)

class LocalScope:
    def __init__(self, tail=None):
        self.tail = tail
        self.var = None # Indicates bounds of current scope

    def bind(self, sym, **kwargs):
        if isinstance(sym, Var):
            assert isinstance(sym.sym, Sym)
            sym.update(kwargs)
            sym.local = True
            sym.sym.var = sym
            sym.sym.scope = self
            scope = LocalScope(self)
            scope.var = sym
            return scope

        if not isinstance(sym, Sym):
            sym = Sym(sym)

        scope = self
        while scope.var is not None:
            if scope.var.sym.name == sym.name:
                if hasattr(scope.var, 'impl'):
                    break

                scope.var.update(kwargs)
                sym.var = scope.var
                sym.scope = self
                return self

            scope = scope.tail

        scope = LocalScope(self)
        scope.var = Var(sym, local=True, **kwargs)
        sym.var = scope.var
        sym.scope = self
        return scope

    def getoverloads(self, sym):
        if not isinstance(sym, Sym):
            sym = Sym(sym)

        if self.var is not None and self.var.sym.name == sym.name:
            return [self.var]

        return self.tail.getoverloads(sym)

    def __getitem__(self, sym):
        overloads = self.getoverloads(sym)

        if len(overloads) == 0:
            raise KeyError(sym)
        elif len(overloads) == 1:
            return overloads[0]
        else:
            return overloads

    def __contains__(self, sym):
        try:
            self[sym]
        except KeyError:
            return False
        else:
            return True

    def __bool__(self):
        return self.var is not None or bool(self.tail)

    def __iter__(self):
        if self.tail:
            yield from self.tail

        if self.var:
            yield self.var

    def itervalues(self):
        for var in self:
            if hasattr(var, 'value'):
                yield var.value

    def iterdecls(self):
        for var in self:
            yield from var.decls

    def iterexprs(self):
        for decl in self.iterdecls():
            yield from decl.iterexprs()

    def itersyms(self):
        for decl in self.iterdecls():
            yield from decl.itersyms()

    def globals(self):
        return self.tail.globals()

    def locals(self):
        return LocalScopeSlice(self)

    def __repr__(self):
        return 'LocalScope({%s})' % ', '.join(
            repr(str(var.sym)) for var in self)

class LocalScopeSlice:
    def __init__(self, scope=None):
        self.scope = scope

    def getoverloads(self, sym):
        if not isinstance(sym, Sym):
            sym = Sym(sym)

        scope = self.scope
        while scope.var is not None:
            if scope.var.name == sym.name:
                return [scope.var]
            scope = scope.tail
        else:
            return []

    def __iter__(self):
        scope = self.scope
        vars = []
        while scope.var is not None:
            vars.append(scope.var)
            scope = scope.tail
        return reversed(vars)

    def __getitem__(self, sym):
        overloads = self.getoverloads(sym)

        if len(overloads) == 0:
            raise KeyError(sym)
        elif len(overloads) == 1:
            return overloads[0]
        else:
            return overloads

    def __contains__(self, sym):
        try:
            self[sym]
        except KeyError:
            return False
        else:
            return True

    def __bool__(self):
        return bool(self.scope)

    def iterdecls(self):
        for var in self:
            yield from var.decls

    def iterexprs(self):
        for decl in self.iterdecls():
            yield from decl.iterexprs()

    def itersyms(self):
        for decl in self.iterdecls():
            yield from decl.itersyms()

    def __repr__(self):
        return 'LocalScopeSlice({%s})' % ', '.join(
            repr(str(var.sym)) for var in self)


# scoping rules
def scopeexpr(self, scope):
    if isinstance(self, Call):
        self.scope = scope
        scopeexpr(self.callee, scope)
        for e in self.exprs:
            scopeexpr(e, scope)
    elif isinstance(self, Init):
        self.scope = scope
        scopeexpr(self.callee, scope)
        for e in self.exprs:
            scopeexpr(e, scope)
    elif isinstance(self, Num):
        self.scope = scope
    elif isinstance(self, Str):
        self.scope = scope
    elif isinstance(self, IntT):
        self.scope = scope
    elif isinstance(self, FunT):
        self.scope = scope
        for arg in self.args:
            scopeexpr(arg, scope)
        for ret in self.rets:
            scopeexpr(ret, scope)
    elif isinstance(self, Sym):
        self.scope = scope
        if self not in scope:
            raise ScopeException("Symbol not in scope %r\n%r" % (self, self.scope), self)
    else:
        raise NotImplementedError("scopeexpr not implemented for %r" % self)

def scopeexprs(selfs, scope):
    for self in selfs:
        scopeexpr(self, scope)

def scopestmt(self, scope):
    if isinstance(self, Let):
        scopeexprs(self.exprs, scope)
        for sym in self.targets:
            scope = scope.bind(sym, decl=self, impl=self)
        return scope
    elif isinstance(self, Def):
        scopeexpr(self.expr, scope)
        return scope.bind(self.sym, decl=self)
    elif isinstance(self, Impl):
        scopeexpr(self.expr, scope)
        return scope
    elif isinstance(self, Return):
        self.scope = scope
        scopeexprs(self.exprs, scope)
        return scope
    elif isinstance(self, Expr):
        scopeexprs(self.exprs, scope)
        return scope
    else:
        raise NotImplementedError("scopestmt not implemented for %r" % self)

def scanpseudostmt(self, scope):
    if isinstance(self, Def):
        return scope.bind(self.sym, decl=self)
    else:
        raise NotImplementedError("scopepseudostmt not implemented for %r" % self)

def scopepseudostmt(self, scope):
    if isinstance(self, Def):
        nscope = LocalScope(scope)
        for arg in self.args:
            nscope = nscope.bind(arg)

        scopeexpr(self.expr, nscope)
    else:
        raise NotImplementedError("scopepseudostmt not implemented for %r" % self)


def scandecl(self, scope):
    if isinstance(self, Fun):
        self.scope = scope
        scope.bind(self.sym, decl=self, impl=self)
    elif isinstance(self, Struct):
        self.scope = scope
        scope.bind(self.sym, decl=self, impl=self)
    elif isinstance(self, Interface):
        self.scope = scope
        scope.bind(self.sym, decl=self)

        nscope = LocalScope(scope)
        for arg in self.args:
            nscope = nscope.bind(arg)

        nscope = LocalScope(nscope)
        for stmt in self.stmts:
            nscope = scanpseudostmt(stmt, nscope)

        self.nscope = nscope
        for var in self.nscope.locals():
            scope.bind(var, decl=self, impl=self)
    elif isinstance(self, Extern):
        self.scope = scope
        for sym in self.targets:   
            scope.bind(sym, decl=self, impl=self, extern=True)
    elif isinstance(self, Export):
        self.scope = scope
        scope.bind(self.sym, decl=self, export=True)
    elif isinstance(self, Def):
        self.scope = scope
        scope.bind(self.sym, decl=self)
    else:
        raise NotImplementedError("scandecl not implemented for %r" % self)

def scopedecl(self, scope):
    if isinstance(self, Fun):
        nscope = LocalScope(scope)

        self.rets = Sym('.rets')
        nscope = nscope.bind(self.rets)

        for arg in self.args:
            nscope = nscope.bind(arg)

        for stmt in self.stmts:
            nscope = scopestmt(stmt, nscope)
    elif isinstance(self, Struct):
        nscope = LocalScope(scope)

        for arg in self.args:
            nscope = nscope.bind(arg)

        for stmt in self.stmts:
            nscope = scopestmt(stmt, nscope)
    elif isinstance(self, Interface):
        for stmt in self.stmts:
            scopepseudostmt(stmt, self.nscope)
    elif isinstance(self, Extern):
        scopeexprs(self.exprs, scope)
    elif isinstance(self, Export):
        pass
    elif isinstance(self, Def):
        nscope = LocalScope(scope)
        for arg in self.args:
            nscope = nscope.bind(arg)

        scopeexpr(self.expr, nscope)
    else:
        raise NotImplementedError("scopedecl not implemented for %r" % self)

def scopecheck(ptree):
    scope = GlobalScope()

    for decl in ptree:
        scandecl(decl, scope)

    for decl in ptree:
        scopedecl(decl, scope)

    for decl in ptree:
        for expr in decl.iterexprs():
            assert hasattr(expr, 'scope'), (
                "%r has no scope after scopecheck" % expr)

    return scope
