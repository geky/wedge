from wsyntax import *
from wtypes import *
from itertools import chain

class Emitter:
    def __init__(self):
        self.gid = 0
        self.lid = 0
        self.globals = []
        self.locals = []
        self.allocs = []

    def getgid(self):
        gid = self.gid
        self.gid += 1
        return '@g%d' % gid

    def getlid(self):
        lid = self.lid
        self.lid += 1
        return '%%l%d' % lid

def mangletype(self):
    if isinstance(self, IntT):
        return 'i'
    elif isinstance(self, FunT):
        return 'f%sr%se' % (
            ''.join(mangletype(arg) for arg in self.args),
            ''.join(mangletype(ret) for ret in self.rets))
    elif isinstance(self, TypeT):
        return 't'
    elif isinstance(self, InterfaceT):
        return mangletype(self.sym) # TODO hm
    elif isinstance(self, Sym):
        return '%d%s' % (len(self.name), self.name)
    else:
        raise NotImplementedError("mangletype not implemented for %r" % self)

def mangle(self):
    var = getattr(self, 'var', None) or self.scope[self]
    if getattr(var, 'local', True):
        i = 0
        for sym in chain(self.scope, [self]):
            if sym == self and 'impl' in sym.__dict__:
                i += 1
        return '%%v%s.%d' % (self.name, i)
    else:
        if hasattr(var, 'export') or hasattr(var, 'extern'):
            return '@%s' % var.sym
        else:
            return '@%s.%s' % (var.sym, mangletype(var.type))

def emittype(self):
    if isinstance(self, IntT):
        return "i32"
    else:
        raise NotImplementedError("emittype not implemented for %r" % self)

def emitexpr(self, e):
    if isinstance(self, Call):
        args = []
        callee = mangle(self.callee)

        for expr in self.exprs:
            args.append('i32 %s' % emitexpr(expr, e))

        if len(self.types) > 1:
            rid = e.getlid() + '.r'
            rtype = '{%s}' % ', '.join('i32' for _ in self.types)
            e.allocs.append([
                '%s = alloca %s, align 4' % (rid, rtype)])
            args.append('%s* %s' % (rtype, rid))

        id = e.getlid()
        e.locals.append([
            '%s = call i32 %s(%s)' % (id, callee, ', '.join(args))])
        return rid if len(self.types) > 1 else id
    elif isinstance(self, Num):
        return '%d' % self.v
    elif isinstance(self, Sym):
        id = e.getlid()
        e.locals.append([
            '%s = load i32, i32* %s, align 4' % (id, mangle(self))])
        return id
    else:
        raise NotImplementedError("emitexpr not implemented for %r" % self)

def emitexprs(self, e):
    if len(self) == 1 and isinstance(self[0], Call) and len(self[0].types) > 1:
        rid = emitexpr(self[0], e)
        rtype = '{%s}' % ', '.join('i32' for _ in self[0].types)
        ids = []
        for i, _ in enumerate(self[0].types):
            lid1, lid2 = e.getlid(), e.getlid()
            e.locals.append([
                '%s = getelementptr inbounds %s, %s* %s, i32 0, i32 %d' % (
                    lid1, rtype, rtype, rid, i),
                '%s = load i32, i32* %s, align 4' % (
                    lid2, lid1)])
            ids.append(lid2)
        return ids
    else:
        return [emitexpr(expr, e) for expr in self]

def emitstmt(self, e):
    if isinstance(self, Let):
        ids = emitexprs(self.exprs, e)
        for sym, id in zip(self.targets, ids):
            e.allocs.append([
                '%s = alloca i32, align 4' % mangle(sym)])
            e.locals.append([
                'store i32 %s, i32* %s, align 4' % (id, mangle(sym))])
    elif isinstance(self, Def):
        pass
    elif isinstance(self, Return):
        rets = emitexprs(self.exprs, e)

        if len(rets) == 0:
            e.locals.append(['ret i32 0'])
        elif len(rets) == 1:
            e.locals.append(['ret i32 %s' % rets[0]])
        else:
            rtype = '{%s}' % ', '.join('i32' for _ in rets)
            for i, ret in enumerate(rets):
                lid = e.getlid()
                e.locals.append([
                    '%s = getelementptr inbounds %s, %s* %%r, i32 0, i32 %d' % (
                        lid, rtype, rtype, i),
                    'store i32 %s, i32* %s, align 4' % (
                        ret, lid)])
    elif isinstance(self, Expr):
        emitexprs(self.exprs, e)
    else:
        raise NotImplementedError("emitstmt not implemented for %r" % self)

def emitvar(self, e):
    if isinstance(self.impl, Fun):
        args = []
        for i, arg in enumerate(self.impl.args):
            args.append('i32 %%a%d' % i)
            e.allocs.append([
                '%s = alloca i32, align 4' % mangle(arg)])
            e.locals.append([
                'store i32 %%a%d, i32* %s, align 4' % (i, mangle(arg))])

        if len(self.type.rets) > 1:
            rtype = '{%s}' % ', '.join('i32' for _ in self.sym.type.rets)
            args.append('%s* %%r' % rtype)

        for s in self.impl.stmts:
            emitstmt(s, e)

        allocs = ['    '+a for a in sum(e.allocs, [])]
        locals = ['    '+l for l in sum(e.locals, [])]
        e.allocs = []
        e.locals = []

        e.globals.append([
            'define i32 %s(%s) {' % (mangle(self.sym), ', '.join(args)),
            ] + allocs + locals + [
            '    ret i32 0',
            '}'
        ])
    elif isinstance(self.impl, Extern):
        for sym in self.impl.targets:
            e.globals.append(['declare i32 %s(i32)' % mangle(self.sym)])
    elif isinstance(self.impl, RawFunImpl):
        args = ['i32 %%a%d' % i for i, _ in enumerate(self.type.args)]
        e.globals.append([
            'define i32 %s(%s) {' % (mangle(self.sym), ','.join(args)),
            ] + ['    '+i for i in self.impl.ir] + [
            '}'
        ])
    else:
        raise NotImplementedError("emitvar not implemented for %r" % self.impl)

def emit(deps):
    e = Emitter()

    for var in deps:
        emitvar(var, e)

    assert len(e.locals) == 0

    output = []
    for g in e.globals:
        output.extend(g)
        output.append('')

    return '\n'.join(output)
 
