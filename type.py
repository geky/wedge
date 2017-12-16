
# Types
class IntT:
    def __repr__(self):
        return 'IntT()'

    def __eq__(self, other):
        return isinstance(other, IntT)

    def __ne__(self, other):
        return not self.__eq__(other)

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
