
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

# General symbols (not keywords!)
class Sym:
    def __init__(self, v):
        assert isinstance(v, str)
        self.v = v

    def __repr__(self):
        return 'Sym(%r)' % self.v

    def __eq__(self, other):
        return isinstance(other, Sym) and self.v == other.v

    def __ne__(self, other):
        return not self.__eq__(other)

    def emit(self):
        return '@%s' % self.v
