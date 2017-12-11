from tokens import *

# Declarations
class Def:
    def __init__(self, sym=None, type=None):
        assert isinstance(sym, Sym)
        self.sym = sym
        self.type = type

    def __repr__(self):
        return 'Def(%r, %r)' % (self.sym, self.type)

    def emit(self):
        return

class Let:
    def __init__(self, sym=None, expr=None):
        assert isinstance(sym, Sym)
        self.sym = sym
        self.expr = expr

    def __repr__(self):
        return 'Let(%r, %r)' % (self.sym, self.expr)

    def emit(self):
        return

class Fun:
    def __init__(self, sym=None, args=[], stmts=[]):
        assert isinstance(sym, Sym)
        assert all(isinstance(a, Sym) for a in args)
        self.sym = sym
        self.args = args
        self.stmts = stmts

    def __repr__(self):
        return 'Fun(%r, %r, %r)' % (self.sym, self.args, self.stmts)

    def emit(self):
        return [
            'define i32 %s() {' % self.sym.emit(),
                [s.emit() for s in self.stmts], '}', '']

class Export:
    def __init__(self, sym=None):
        assert isinstance(sym, Sym)
        self.sym = sym

    def __repr__(self):
        return 'Export(%r)' % self.sym

class Extern:
    def __init__(self, sym=None):
        assert isinstance(sym, Sym)
        self.sym = sym

    def __repr__(self):
        return 'Extern(%r)' % self.sym

# Statements
class Return:
    def __init__(self, exprs=[]):
        self.exprs = exprs

    def __repr__(self):
        return 'Return(%r)' % self.exprs

    def emit(self):
        return 'ret i32 0'

class Assign:
    def __init__(self, lh=[], rh=[]):
        self.lh = lh
        self.rh = rh

    def __repr__(self):
        return 'Assign(%r, %r)' % (self.lh, self.rh)

# Expressions
class Call:
    def __init__(self, sym=None, exprs=[]):
        self.sym = sym
        self.exprs = exprs

    def __repr__(self):
        return 'Call(%r, %r)' % (self.sym, self.exprs)
