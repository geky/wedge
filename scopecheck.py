from syntax import *
from type import *

class ScopeException(Exception):
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

def scopetype(self, s):
    if isinstance(self, IntT):
        pass
    elif isinstance(self, FunT):
        for arg in self.args:
            scopetype(arg, s)
        for ret in self.rets:
            scopetype(ret, s)
    elif isinstance(self, Sym):
        if self not in s or not hasattr(s[self], 'decl'):
            raise ScopeException("Symbol not in scope %s line %d" % (self, self.line))
        self.scope = s
    else:
        raise NotImplementedError("scopetype not implemented for %r" % self)

def scopeexpr(self, s):
    if isinstance(self, Call):
        self.scope = s
        scopeexpr(self.sym, s)
        for e in self.exprs:
            scopeexpr(e, s)
        return s
    elif isinstance(self, Num):
        pass
    elif isinstance(self, Str):
        pass
    elif isinstance(self, Sym):
        if self not in s or not hasattr(s[self], 'decl'):
            raise ScopeException("Symbol not in scope %s line %d" % (self, self.line))
        self.scope = s
    else:
        raise NotImplementedError("scopeexpr not implemented for %r" % self)

def scopeexprs(selfs, s):
    for self in selfs:
        scopeexpr(self, s)

def scopestmt(self, s):
    if isinstance(self, Let):
        self.scope = s
        scopeexprs(self.exprs, s)
        for sym in self.syms:
            s = s.bind(sym, decl=self)
        return s
    if isinstance(self, Def):
        self.scope = s
        scopetype(self.type, s)
        return s.bind(self.sym, def_=self)
    elif isinstance(self, Return):
        self.scope = s
        scopeexprs(self.exprs, s)
        return s
    elif isinstance(self, Expr):
        self.scope = s
        scopeexprs(self.exprs, s)
        return s
    else:
        raise NotImplementedError("scopestmt not implemented for %r" % self)

def scopedecl(self, s):
    if isinstance(self, Fun):
        self.scope = s
        s = s.bind(self.sym, local=False, decl=self, impl=self)

        ns = s
        self.ret = Sym('return')
        ns = ns.bind(self.ret)

        for arg in self.args:
            ns = ns.bind(arg)

        for stmt in self.stmts:
            ns = scopestmt(stmt, ns)

        return s
    elif isinstance(self, Type):
        self.scope = s
        s = s.bind(self.sym, local=False, decl=self, impl=self)

        ns = s
        for stmt in self.stmts:
            ns = scopestmt(stmt, ns)

        self.ctor = Fun(Sym('%s.ctor' % self.sym.name),
            [Sym(stmt.sym.name) for stmt in self.stmts], [])
        return scopedecl(self.ctor, s)
    elif isinstance(self, Extern):
        self.scope = s
        scopetype(self.type, s)
        return s.bind(self.sym, local=False, decl=self, impl=self)
    elif isinstance(self, Export):
        self.scope = s
        return s.bind(self.sym, local=False, decl=self, export=True)
    elif isinstance(self, Def):
        self.scope = s
        scopetype(self.type, s)
        return s.bind(self.sym, local=False, decl=self)
    else:
        raise NotImplementedError("scopedecl not implemented for %r" % self)

def scopecheck(ptree):
    s = Scope()

    for d in ptree:
        s = scopedecl(d, s)

    return s
