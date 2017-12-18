from syntax import *

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

def emitsym(self):
    if getattr(self, 'local', True):
        return '%%%s' % self.name
    else:
        return '@%s' % self.name

def emittype(self):
    if isinstance(self, IntT):
        return "i32"
    else:
        raise NotImplementedError("emittype not implemented for %r" % self)

def emitexpr(self, e):
    if isinstance(self, Call):
        args = []
        callee = emitsym(self.sym)

        for expr in self.exprs:
            args.append('i32 %s' % emitexpr(expr, e))

        if len(self.types) > 1:
            rid = e.getlid() + '-r'
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
            '%s = load i32, i32* %s, align 4' % (id, emitsym(self))])
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
        for sym, id in zip(self.syms, ids):
            e.allocs.append([
                '%s = alloca i32, align 4' % emitsym(sym)])
            e.locals.append([
                'store i32 %s, i32* %s, align 4' % (id, emitsym(sym))])
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
                    '%s = getelementptr inbounds %s, %s* %%-r, i32 0, i32 %d' % (
                        lid, rtype, rtype, i),
                    'store i32 %s, i32* %s, align 4' % (
                        ret, lid)])
    elif isinstance(self, Expr):
        emitexprs(self.exprs, e)
    else:
        raise NotImplementedError("emitstmt not implemented for %r" % self)

def emitdecl(self, e):
    if isinstance(self, Fun):
        args = []
        for arg in self.args:
            args.append('i32 %s-a' % emitsym(arg))
            e.allocs.append([
                '%s = alloca i32, align 4' % emitsym(arg)])
            e.locals.append([
                'store i32 %s-a, i32* %s, align 4' % (emitsym(arg), emitsym(arg))])

        if len(self.sym.type.rets) > 1:
            rtype = '{%s}' % ', '.join('i32' for _ in self.sym.type.rets)
            args.append('%s* %%-r' % rtype)

        for s in self.stmts:
            emitstmt(s, e)

        allocs = ['    '+a for a in sum(e.allocs, [])]
        locals = ['    '+l for l in sum(e.locals, [])]
        e.allocs = []
        e.locals = []

        e.globals.append([
            'define i32 %s(%s) {' % (emitsym(self.sym), ', '.join(args)),
            ] + allocs + locals + [
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

    for sym in ptree:
        if hasattr(sym, 'impl'):
            emitdecl(sym.impl, e)

    assert len(e.locals) == 0

    output = []
    for g in e.globals:
        output.extend(g)
        output.append('')

    return '\n'.join(output)
 
