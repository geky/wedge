

def getline(line):
    while line and not isinstance(line, int):
        if isinstance(line, list):
            line = getline(line[0])
        else:
            try:
                line = line.line
            except AttributeError:
                line = None

    return line

class CompileException(Exception):
    def __init__(self, message, line=None):
        self.message = message
        self.line = getline(line)

    def __str__(self):
        if self.line:
            lines = self.message.split('\n')
            lines[0] = "%s line %d" % (lines[0], self.line)
            return "\n".join(lines)
        else:
            return self.message

def method(cls):
    def decorator(f):
        setattr(cls, f.__name__, f)
        return f
    return decorator

def methods(cls):
    for name, f in cls.__dict__.items():
        if callable(f):
            f.__name__ = cls.__name__
            method(f.__globals__[name])(f)
