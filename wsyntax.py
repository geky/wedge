from wtokens import *

# Declarations
class Def:
    def __init__(self, sym=None, args=[], expr=None):
        self.sym = sym
        self.args = args
        self.expr = expr

    def __repr__(self):
        return 'Def(%r, %r, %r)' % (self.sym, self.args, self.expr)

    def itersyms(self):
        yield from self.expr.itersyms()

    def iterexprs(self):
        yield from self.expr.iterexprs()

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

class Struct:
    def __init__(self, sym=None, args=[], stmts=[]):
        assert isinstance(sym, Sym)
        self.sym = sym
        self.args = args
        self.stmts = stmts

    def __repr__(self):
        return 'Struct(%r, %r, %r)' % (self.sym, self.args, self.stmts)

    def itersyms(self):
        for stmt in self.stmts:
            yield from stmt.itersyms()

    def iterexprs(self):
        for stmt in self.stmts:
            yield from stmt.iterexprs()

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
    def __init__(self, sym=None, args=[], stmts=[]):
        assert isinstance(sym, Sym)
        self.sym = sym
        self.args = args
        self.stmts = stmts

    def __repr__(self):
        return 'Interface(%r, %r, %r)' % (self.sym, self.args, self.stmts)

    def itersyms(self):
        for stmt in self.stmts:
            yield from stmt.itersyms()

    def iterexprs(self):
        for stmt in self.stmts:
            yield from stmt.iterexprs()

class Impl:
    def __init__(self, expr=None):
        self.expr = expr

    def __repr__(self):
        return 'Impl(%r)' % self.expr

    def itersyms(self):
        yield from self.expr.itersyms()

    def iterexprs(self):
        yield from self.expr.iterexprs()

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

    def eval(self, expand, exclude=set()):
        callee = self.callee.eval(False, exclude)
        if hasattr(callee, 'var') and getattr(callee.var, 'pure', False):
            args = [expr.eval(expand, exclude) for expr in self.exprs]
            sym = callee.var.exec(*args)
            return sym.eval(expand, exclude)

        raise EvalException("not able to eval %r" % self, self)

    def sub(self, sym, rep, exclude=set()):
        # TODO hm, hack? maybe adopt this pattern everywhere?
        if isinstance(sym, Call):
            if self.callee == sym.callee and self.exprs == sym.exprs:
                return rep

        return Call(self.callee.sub(sym, rep, exclude),
            [expr.sub(sym, rep, exclude) if expr else expr for expr in self.exprs])

    def expand(self, exclude=set()):
        callee, exprs, expanded = None, [], False
        callee, expanded = self.callee.expand(exclude)
        for expr in self.exprs:
            e, x = expr.expand(exclude)
            exprs.append(e)
            expanded = expanded or x

        return Call(callee, exprs), expanded

# Expressions
class Init:
    def __init__(self, callee=None, exprs=[]):
        self.callee = callee
        self.exprs = exprs

    def __repr__(self):
        return 'Init(%r, %r)' % (self.callee, self.exprs)

    def itersyms(self):
        yield from self.callee.itersyms()

        for expr in self.exprs:
            yield from expr.itersyms()

    def iterexprs(self):
        yield from self.callee.iterexprs()

        for expr in self.exprs:
            yield from expr.iterexprs()

        yield self

