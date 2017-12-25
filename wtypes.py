
# Types
class IntT:
    def __init__(self):
        self.name = 'int' # hm

    def __repr__(self):
        return 'IntT()'

    def __eq__(self, other):
        return isinstance(other, IntT)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(IntT)

class FunT:
    def __init__(self, args, rets):
        self.args = args
        self.rets = rets

    def __repr__(self):
        return 'FunT(%r, %r)' % (self.args, self.rets)

    def __eq__(self, other):
        return (
            isinstance(other, FunT) and
            self.args == other.args and
            self.rets == other.rets)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return (hash(FunT) +
            sum(hash(arg) for arg in self.args) +
            sum(hash(ret) for ret in self.rets))

#class StructT:
#    def __init__(self, syms, types):
#        self.syms = syms
#        self.types = types
#
#    def __repr__(self):
#        return 'StructT(%r, %r)' % (self.syms, self.types)
#
#    def __eq__(self, other):
#        return (
#            isinstance(other, StructT) and
#            self.types == other.types)

class TypeT:
    def __repr__(self):
        return 'TypeT'

    def __eq__(self, other):
        return isinstance(other, TypeT)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(TypeT)
