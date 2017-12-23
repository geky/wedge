from wsyntax import *
from wtypes import *
from util import CompileException

class ScopeException(CompileException):
    pass

class Scope:
    def __init__(self, sym=None, tail=None):
        assert sym is None or isinstance(sym, Sym)
        assert tail is None or isinstance(tail, Scope)
        self.tail = tail
        self.sym = sym

    def bind(self, sym, **kwargs):
        if isinstance(sym, str):
            sym = Sym(sym)
        assert isinstance(sym, Sym)

        sym.scope = self
        for k, v in kwargs.items():
            setattr(sym, k, v)
        return Scope(sym, self)

    def bindall(self, syms):
        for sym in syms:
            if isinstance(sym, tuple):
                sym, attrs = sym
            else:
                sym, attrs = sym, {}

            self = self.bind(sym, **attrs)

        return self

    def __getitem__(self, sym):
        if isinstance(sym, tuple):
            sym, attr = sym
        else:
            sym, attr = sym, None

        if isinstance(sym, str):
            sym = Sym(sym)

        while self:
            if attr:
                if sym == self.sym and hasattr(self.sym, attr):
                    return getattr(self.sym, attr)
            else:
                if sym == self.sym:
                    return self.sym
            self = self.tail
        else:
            raise KeyError(sym)

    def __contains__(self, sym):
        try:
            self[sym]
        except KeyError:
            return False
        else:
            return True

    def __bool__(self):
        return not (self.sym is None and self.tail is None)

    def __iter__(self):
        all = []
        while self:
            all.append(self.sym)
            self = self.tail
        return reversed(all)

    def __repr__(self):
        return 'Scope([%s])' % ', '.join(map(repr, self))

    def until(self, scope):
        all = []
        while self and self is not scope:
            all.append(self.sym)
            self = self.tail
        return reversed(all)

def scopeexpr(self, scope):
    if isinstance(self, Call):
        self.scope = scope
        scopeexpr(self.sym, scope)
        for e in self.exprs:
            scopeexpr(e, scope)
        return scope
    elif isinstance(self, Num):
        pass
    elif isinstance(self, Str):
        pass
    elif isinstance(self, IntT):
        pass
    elif isinstance(self, FunT):
        pass
    elif isinstance(self, Sym):
        if self not in scope or not hasattr(scope[self], 'decl'):
            raise ScopeException("Symbol not in scope %s" % self, self)
        self.scope = scope
    else:
        raise NotImplementedError("scopeexpr not implemented for %r" % self)

def scopeexprs(selfs, scope):
    for self in selfs:
        scopeexpr(self, scope)

def scopestmt(self, scope):
    if isinstance(self, Let):
        self.scope = scope
        scopeexprs(self.exprs, scope)
        for sym in self.syms:
            scope = scope.bind(sym, decl=self)
        return scope
    elif isinstance(self, Def):
        self.scope = scope
        scopeexprs(self.exprs, scope)
        for sym in self.syms:
            scope = scope.bind(sym, decl=self)
        return scope
    elif isinstance(self, Return):
        self.scope = scope
        scopeexprs(self.exprs, scope)
        return scope
    elif isinstance(self, Expr):
        self.scope = scope
        scopeexprs(self.exprs, scope)
        return scope
    else:
        raise NotImplementedError("scopestmt not implemented for %r" % self)

def scopedecl(self, scope):
    if isinstance(self, Fun):
        self.scope = scope
        scope = scope.bind(self.sym, local=False, decl=self, impl=self)

        ns = scope
        self.ret = Sym('return')
        ns = ns.bind(self.ret)

        for arg in self.args:
            ns = ns.bind(arg, decl=self, impl=self)

        for stmt in self.stmts:
            ns = scopestmt(stmt, ns)

        return scope
    elif isinstance(self, Type):
        self.scope = scope
        scope = scope.bind(self.sym, local=False, decl=self, impl=self)

        ns = scope
        for stmt in self.stmts:
            ns = scopestmt(stmt, ns)

        self.ctor = Fun(Sym('%s.ctor' % self.sym.name),
            [Sym(sym.name)
                for sym in stmt.syms
                for stmt in self.stmts], [])
        return scopedecl(self.ctor, scope)
    elif isinstance(self, Extern):
        self.scope = scope
        scopeexprs(self.exprs, scope)
        for sym in self.syms:   
            scope = scope.bind(sym, local=False, decl=self, impl=self)
        return scope
    elif isinstance(self, Export):
        self.scope = scope
        return scope.bind(self.sym, local=False, decl=self, export=True)
    elif isinstance(self, Def):
        self.scope = scope
        scopeexprs(self.exprs, scope)
        for sym in self.syms:
            scope = scope.bind(sym, local=False, decl=self)
        return scope
    else:
        raise NotImplementedError("scopedecl not implemented for %r" % self)

def scopecheck(ptree):
    scope = Scope()

    for decl in ptree:
        scope = scopedecl(decl, scope)

    return scope
