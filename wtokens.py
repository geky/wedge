

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
    def __init__(self, name):
        assert isinstance(name, str)
        self.name = name

    def __repr__(self):
        return 'Sym(%r)' % self.name

    def __eq__(self, other):
        return isinstance(other, Sym) and self.name == other.name

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(self.name)

    def __getattr__(self, attr):
        if attr != 'scope':
            if hasattr(self, 'scope'):
                if 'def_' not in self.__dict__:
                    try:
                        return self.scope.getattr(self, attr)
                    except AttributeError:
                        pass

                if attr.endswith('s'):
                    if getattr(self, 'local', True):
                        try:
                            return [getattr(self, attr[:-1])]
                        except AttributeError:
                            pass
                    else:
                        attrs = list(self.scope.getattrs(self, attr[:-1]))
                        if attr[:-1] in self.__dict__:
                            attrs.append(self.__dict__[attr[:-1]])
                        if attrs:
                            return attrs
                        
        raise AttributeError("%r has no attribute %r" % (self, attr))

    def sub(self, sym, rep):
        if sym == self:
            return rep
        else:
            return self
