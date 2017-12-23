from wtokens import *
from wsyntax import *
from wtypes import *


def eval(self):
    if isinstance(self, Num):
        self.value = self
        return self
    elif isinstance(self, Str):
        self.value = self
        return self
    elif isinstance(self, IntT):
        self.value = self
        return self
    elif isinstance(self, FunT):
        self.value = self
        return self
    elif isinstance(self, StructT):
        self.value = self
        return self
    elif isinstance(self, TypeT):
        self.value = self
        return self
    elif isinstance(self, Sym):
        return getattr(self, 'value', None)
