from wtokens import Sym


# Types
class IntT:
    def __init__(self):
        self.name = 'int' # hm

    def __repr__(self):
        return 'IntT()'

    def __str__(self):
        return 'int'

    def __eq__(self, other):
        return isinstance(other, IntT)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(IntT)

    def sub(self, sym, rep, exclude=set()):
        return self

    def eval(self, expand, exclude=set()):
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

    def sub(self, sym, rep, exclude=set()):
        return FunT(
            [arg.sub(sym, rep, exclude) for arg in self.args],
            [ret.sub(sym, rep, exclude) for ret in self.rets])

    def eval(self, expand, exclude=set()):
        return FunT(
            [arg.eval(expand, exclude) for arg in self.args],
            [ret.eval(expand, exclude) for ret in self.rets])

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

    def raw(self):
        return self

class StructT:
    def __init__(self, fields):
        self.fields = fields

    def __repr__(self):
        return 'StructT(%r)' % self.fields

    def __eq__(self, other):
        return (
            isinstance(other, StructT) and
            self.fields == other.fields)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return (hash(FunT) +
            sum(hash(field) for field in self.fields))

    def sub(self, sym, rep, exclude=set()):
        return StructT(
            [(key, type.sub(sym, rep, exclude)) for key, type in self.fields])

    def eval(self, expand, exclude=set()):
        return self

    def itersyms(self):
        for _, type in self.fields:
            yield from field.itersyms()

    def iterexprs(self):
        for _, type in self.field:
            yield from field.iterexprs()
        yield self

class InterfaceT:
    def __init__(self, target, funs, impls=None, rebindings=None):
        self.target = target
        self.funs = funs
        self.impls = impls if impls is not None else set()
        self.rebindings = rebindings if rebindings is not None else {} #TODO rm me

    def __repr__(self):
        return 'InterfaceT(%r, %r)' % (str(self.target), self.funs)

    @classmethod
    def getiid(cls):
        id = getattr(cls, 'id', 0)
        cls.id = id + 1
        return Sym('.i%d' % id)

    def __eq__(self, other):
        return (
            isinstance(other, InterfaceT) and
            self.target == other.target)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(InterfaceT) + hash(self.target)

    def sub(self, sym, rep, exclude=set()):
        return InterfaceT(self.target,
            {(sym, type.sub(sym, rep, exclude)) for sym, type in self.funs},
            self.impls)

    def eval(self, expand, exclude=set()):
        return self

    def itersyms(self):
        yield from [] # TODO?

    def iterexprs(self):
        yield self

class ParamedT:
    def __init__(self, syms, type):
        self.syms = syms
        self.type = type

    def __repr__(self):
        return 'ParamedT(%r, %r)' % (self.syms, self.type)

    def __eq__(self, other):
        # TODO sub params?
        return isinstance(other, ParamedT) and self.type == other.type

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(ParamedT) + hash(self.type) 

    def sub(self, sym, rep, exclude=set()):
        if isinstance(sym, Sym) and any(params.name == sym.name for params in self.syms):
            return self

        return ParamedT(self.syms,
            self.type.sub(sym, rep, exclude | set(self.syms)))

    def eval(self, expand, exclude=set()):
        return ParamedT(self.syms,
            self.type.eval(expand, exclude | set(self.syms)))

    def itersyms(self):
        yield from self.type.itersyms()

    def iterexprs(self):
        yield from self.type.iterexprs()
        yield self

    def raw(self):
        ntype = self.type
        for sym in self.syms:
            ntype = ntype.sub(sym, None)
        return ntype

class TypeT:
    def __repr__(self):
        return 'TypeT()'

    def __eq__(self, other):
        return isinstance(other, TypeT)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(TypeT)

    def sub(self, sym, rep, exclude=set()):
        return self

    def eval(self, expand, exclude=set()):
        return self

    def itersyms(self):
        yield from []

    def iterexprs(self):
        yield self

#class ParameterT:
#    def __init__(self, sym):
#        self.sym = sym
#
#    def __repr__(self, sym):
#        return 'ParameterT(%r)' % self.sym
#
#    def __eq__(self, other):
#        return isinstance(other, TypeT)
#
#    def __ne__(self, other):
#        return not self.__eq__(other)
#
#    def __hash__(self):
#        return hash(TypeT)
#
#    def sub(self, sym, rep):
#        return self
#
#    def expand(self):
#        return self, False
#
#    def eval(self):
#        return self
#
#    def itersyms(self):
#        yield from []
#
#    def iterexprs(self):
#        yield self
#
#
