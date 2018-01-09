from wtokens import *

# Declarations
class Def:
    def __init__(self, targets=None, exprs=None):
        self.targets = targets
        self.exprs = exprs

    def __repr__(self):
        return 'Def(%r, %r)' % (self.targets, self.exprs)

    def itersyms(self):
        for expr in self.exprs:
            yield from expr.itersyms()

    def iterexprs(self):
        for expr in self.exprs:
            yield from expr.iterexprs()

class Let:
    def __init__(self, targets=None, exprs=None):
        self.targets = targets
        self.exprs = exprs

    def __repr__(self):
        return 'Let(%r, %r)' % (self.targets, self.exprs)

    def itersyms(self):
        for expr in self.exprs:
            yield from expr.itersyms()

    def iterexprs(self):
        for expr in self.exprs:
            yield from expr.iterexprs()

class Fun:
    def __init__(self, sym=None, args=[], stmts=[]):
        assert isinstance(sym, Sym)
        assert all(isinstance(a, Sym) for a in args)
        self.sym = sym
        self.args = args
        self.stmts = stmts

    def __repr__(self):
        return 'Fun(%r, %r, %r)' % (self.sym, self.args, self.stmts)

    def itersyms(self):
        for stmt in self.stmts:
            yield from stmt.itersyms()

    def iterexprs(self):
        for stmt in self.stmts:
            yield from stmt.iterexprs()

class RawFunImpl:
    def __init__(self, ir):
        self.ir = ir

    def __repr__(self):
        return 'RawFun(...)'

    def itersyms(self):
        yield from []

    def iterexprs(self):
        yield self

class Type:
    def __init__(self, sym=None, stmts=[]):
        assert isinstance(sym, Sym)
        self.sym = sym
        self.stmts = stmts

    def __repr__(self):
        return 'Type(%r, %r)' % (self.sym, self.stmts)

    def itersyms(self):
        for stmt in self.stmts:
            yield from stmt.itersyms()

    def iterexprs(self):
        for stmt in self.stmts:
            yield from stmt.iterexprs()

class Interface:
    def __init__(self, sym=None, stmts=[]):
        assert isinstance(sym, Sym)
        self.sym = sym
        self.stmts = stmts

    def __repr__(self):
        return 'Interface(%r, %r)' % (self.sym, self.stmts)

    def itersyms(self):
        for stmt in self.stmts:
            yield from stmt.itersyms()

    def iterexprs(self):
        for stmt in self.stmts:
            yield from stmt.iterexprs()

class Impl:
    def __init__(self, sym=None):
        assert isinstance(sym, Sym)
        self.sym = sym

    def __repr__(self):
        return 'Impl(%r)' % self.sym

    def itersyms(self):
        yield from self.sym.itersyms()

    def iterexprs(self):
        yield from self.sym.itersyms()

class Export:
    def __init__(self, sym=None):
        assert isinstance(sym, Sym)
        self.sym = sym

    def __repr__(self):
        return 'Export(%r)' % self.sym

    def itersyms(self):
        yield from []

    def iterexprs(self):
        yield from []

class Extern:
    def __init__(self, sym=None, exprs=None):
        self.targets = sym
        self.exprs = exprs

    def __repr__(self):
        return 'Extern(%r, %r)' % (self.targets, self.exprs)

    def itersyms(self):
        for expr in self.exprs:
            yield from expr.itersyms()

    def iterexprs(self):
        for expr in self.exprs:
            yield from expr.iterexprs()

# Statements
class Return:
    def __init__(self, exprs=[]):
        self.exprs = exprs

    def __repr__(self):
        return 'Return(%r)' % self.exprs

    def itersyms(self):
        for expr in self.exprs:
            yield from expr.itersyms()

    def iterexprs(self):
        for expr in self.exprs:
            yield from expr.iterexprs()

class Assign:
    def __init__(self, targets=None, exprs=None):
        self.targets = targets
        self.exprs = exprs

    def __repr__(self):
        return 'Assign(%r, %r)' % (self.targets, self.exprs)

    def itersyms(self):
        for expr in self.exprs:
            yield from expr.itersyms()

    def iterexprs(self):
        for expr in self.exprs:
            yield from expr.iterexprs()

class Expr:
    def __init__(self, exprs=[]):
        self.exprs = exprs

    def __repr__(self):
        return 'Expr(%r)' % self.exprs

    def itersyms(self):
        for expr in self.exprs:
            yield from expr.itersyms()

    def iterexprs(self):
        for expr in self.exprs:
            yield from expr.iterexprs()

# Expressions
class Call:
    def __init__(self, callee=None, exprs=[]):
        self.callee = callee
        self.exprs = exprs

    def __repr__(self):
        return 'Call(%r, %r)' % (self.callee, self.exprs)

    def itersyms(self):
        yield from self.callee.itersyms()

        for expr in self.exprs:
            yield from expr.itersyms()

    def iterexprs(self):
        yield from self.callee.iterexprs()

        for expr in self.exprs:
            yield from expr.iterexprs()

        yield self
