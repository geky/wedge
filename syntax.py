from tokens import *

# Declarations
class Def:
    def __init__(self, syms=None, exprs=None):
        self.syms = syms
        self.exprs = exprs

    def __repr__(self):
        return 'Def(%r, %r)' % (self.syms, self.exprs)

class Let:
    def __init__(self, syms=None, exprs=None):
        self.syms = syms
        self.exprs = exprs

    def __repr__(self):
        return 'Let(%r, %r)' % (self.syms, self.exprs)

class Fun:
    def __init__(self, sym=None, args=[], stmts=[]):
        assert isinstance(sym, Sym)
        assert all(isinstance(a, Sym) for a in args)
        self.sym = sym
        self.args = args
        self.stmts = stmts

    def __repr__(self):
        return 'Fun(%r, %r, %r)' % (self.sym, self.args, self.stmts)

class Type:
    def __init__(self, sym=None, stmts=[]):
        assert isinstance(sym, Sym)
        self.sym = sym
        self.stmts = stmts

    def __repr__(self):
        return 'Type(%r, %r)' % (self.sym, self.stmts)

class Export:
    def __init__(self, sym=None):
        assert isinstance(sym, Sym)
        self.sym = sym

    def __repr__(self):
        return 'Export(%r)' % self.sym

class Extern:
    def __init__(self, sym=None, exprs=None):
        self.syms = sym
        self.exprs = exprs

    def __repr__(self):
        return 'Extern(%r, %r)' % (self.syms, self.exprs)

# Statements
class Return:
    def __init__(self, exprs=[]):
        self.exprs = exprs

    def __repr__(self):
        return 'Return(%r)' % self.exprs

class Assign:
    def __init__(self, syms=None, exprs=None):
        self.syms = syms
        self.exprs = exprs

    def __repr__(self):
        return 'Assign(%r, %r)' % (self.syms, self.exprs)

class Expr:
    def __init__(self, exprs=[]):
        self.exprs = exprs

    def __repr__(self):
        return 'Expr(%r)' % self.exprs

# Expressions
class Call:
    def __init__(self, sym=None, exprs=[]):
        self.sym = sym
        self.exprs = exprs

    def __repr__(self):
        return 'Call(%r, %r)' % (self.sym, self.exprs)
