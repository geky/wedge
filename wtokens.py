from util import CompileException

class EvalException(CompileException):
    pass

# Literal numbers and strings
class Num:
    def __init__(self, v):
        assert isinstance(v, int)
        self.v = v

    def __repr__(self):
        return 'Num(%r)' % self.v

    def __eq__(self, other):
        return isinstance(other, Num) and self.v == other.v

    def __ne__(self, other):
        return not self.__eq__(other)

    def itersyms(self):
        yield from []

    def iterexprs(self):
        yield self

class Str:
    def __init__(self, v):
        assert isinstance(v, str)
        self.v = v

    def __repr__(self):
        return 'Str(%r)' % self.v

    def __eq__(self, other):
        return isinstance(other, Str) and self.v == other.v

    def __ne__(self, other):
        return not self.__eq__(other)

    def itersyms(self):
        yield from []

    def iterexprs(self):
        yield self

# General symbols (not keywords!)
class Sym:
    def __init__(self, name):
        if isinstance(name, Sym):
            name = name.name

        assert isinstance(name, str)
        self.name = name

    def __repr__(self):
        return 'Sym(%r)' % self.name

    def __str__(self):
        return self.name

    def __eq__(self, other):
        return (isinstance(other, Sym) and self.name == other.name)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(self.name)

    def itersyms(self):
        yield self

    def iterexprs(self):
        yield self

    def sub(self, sym, rep):
        if sym == self:
            return rep
        else:
            return self

    def expand(self):
        if hasattr(self, 'var') and hasattr(self.var, 'value'):
            return self.var.value, True
        else:
            return self, False

    def eval(self):
        if hasattr(self, 'var') and hasattr(self.var, 'value'):
            return self.var.value
        else:
            raise EvalException("not able to eval %r" % self, self)

