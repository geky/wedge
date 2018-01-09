from wtokens import Sym


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

    def sub(self, sym, rep):
        return self

    def expand(self):
        return self, False

    def eval(self):
        return self

    def itersyms(self):
        yield from []

    def iterexprs(self):
        yield self

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

    def sub(self, sym, rep):
        return FunT(
            [arg.sub(sym, rep) for arg in self.args],
            [ret.sub(sym, rep) for ret in self.rets])

    def expand(self):
        args, rets, expanded = [], [], False
        for arg in self.args:
            a, x = arg.expand()
            args.append(a)
            expanded = expanded or x
        for ret in self.rets:
            r, x = ret.expand()
            rets.append(r)
            expanded = expanded or x
            
        return FunT(args, rets), expanded

    def eval(self):
        return self

    def itersyms(self):
        for arg in self.args:
            yield from arg.itersyms()
        for ret in self.rets:
            yield from ret.itersyms()

    def iterexprs(self):
        for arg in self.args:
            yield from arg.iterexprs()
        for ret in self.rets:
            yield from ret.iterexprs()
        yield self

class InterfaceT:
    def __init__(self, sym, funs=set(), impls=set()):
        if isinstance(sym, list):
            impls = funs
            funs = sym
            sym = InterfaceT.getiid()

        self.sym = sym
        self.funs = funs
        self.impls = impls or set()

    def __repr__(self):
        return 'InterfaceT(%r, %r)' % (str(self.sym), self.funs)

    def __eq__(self, other):
        return (
            isinstance(other, InterfaceT) and
            self.sym == other.sym)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(InterfaceT) + hash(self.sym)

    def sub(self, sym, rep):
        return InterfaceT(self.sym,
            {(sym, type.sub(sym, rep)) for sym, type in self.funs},
            self.impls)

    def expand(self):
        return self, False

    @classmethod
    def getiid(cls):
        id = getattr(cls, 'id', 0)
        cls.id = id + 1
        return Sym('.i%d' % id)

    def eval(self):
        return self

    def itersyms(self):
        yield from [] # TODO?

    def iterexprs(self):
        yield self

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

    def sub(self, sym, rep):
        return self

    def expand(self):
        return self, False

    def eval(self):
        return self

    def itersyms(self):
        yield from []

    def iterexprs(self):
        yield self


