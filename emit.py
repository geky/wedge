from syntax import *

class Emitter:
    def __init__(self):
        self.gid = 0
        self.lid = 0
        self.globals = []
        self.locals = []

    def getgid(self):
        gid = self.gid
        self.gid += 1
        return '@g%d' % gid

    def getlid(self):
        lid = self.lid
        self.lid += 1
        return '%%l%d' % lid

def emitsym(self):
    if self.local:
        return '%%%s' % self.v
    else:
        return '@%s' % self.v

def emittype(self):
    if isinstance(self, IntT):
        return "i32"
    else:
        raise NotImplementedError("emittype not implemented for %r" % self)

def emitexpr(self, e):
    if isinstance(self, Call):
        aid = emitexpr(self.exprs[0], e)
        id = e.getlid()
        e.locals.append(['%s = call i32 %s(i32 %s)' % (id, emitsym(self.sym), aid)])
        return id
    elif isinstance(self, Num):
        return '%d' % self.v
    elif isinstance(self, Sym):
        if self.local:
            return '%%%s' % self.v
        else:
            return '@%s' % self.v
    else:
        raise NotImplementedError("emitexpr not implemented for %r" % self)

def emitstmt(self, e):
    if isinstance(self, Let):
        lid = e.getlid()
        e.locals.append([
            '%s = alloca i32, align 4' % lid,
            'store i32 %s, i32* %s, align 4' % (emitexpr(self.expr, e), lid),
            '%s = load i32, i32* %s, align 4' % (emitsym(self.sym), lid)])
    elif isinstance(self, Def):
        pass
    elif isinstance(self, Return):
        v = emitexpr(self.exprs[0], e)
        e.locals.append(['ret i32 %s' % v])
    else:
        emitexpr(self, e)

def emitdecl(self, e):
    if isinstance(self, Fun):
        args = []
        for arg in self.args:
            args.append('i32 %s' % emitsym(arg))

        for s in self.stmts:
            emitstmt(s, e)

        locals = ['    '+l for l in sum(e.locals, [])]
        e.locals = []

        e.globals.append([
            'define i32 %s(%s) {' % (emitsym(self.sym), ', '.join(args)),
            ] + locals + [
            '    ret i32 0',
            '}'
        ])
    elif isinstance(self, Def):
        pass
    elif isinstance(self, Export):
        pass
    elif isinstance(self, Extern):
        e.globals.append(['declare i32 %s(i32)' % emitsym(self.sym)])
    else:
        raise NotImplementedError("emitdecl not implemented for %r" % self)

def emit(ptree):
    e = Emitter()

    for d in ptree:
        emitdecl(d, e)

    assert len(e.locals) == 0

    output = []
    for g in e.globals:
        output.extend(g)
        output.append('')

    return '\n'.join(output)
 
