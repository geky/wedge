
from wtokens import *
from wsyntax import *
from wtypes import *
import rules
import lex

def parsesingletype(p):
    if p.accept('int'):
        return IntT()
    elif p.accept(Sym):
        return p.match
    else:
        raise p.unexpected()

def parsetypes(p):
    if p.accept('void'):
        types = []
    else:
        types = [parsesingletype(p)]

    while True:
        if p.accept(','):
            types.append(parsesingletype(p))
        elif p.accept('->'):
            rh = p.expect(parsetypes)
            types = [FunT(types, rh)]
        else:
            return types

def parsetype(p):
    ts = parsetypes(p)
    assert len(ts) == 1 # TODO
    return ts[0]

def parseframe(p):
    if p.accept(Sym):
        expr = p.match
    elif p.accept(Num):
        expr = p.match
    elif p.accept('int'):
        expr = IntT()
    else:
        raise p.unexpected()

    while True:
        if p.accept('('):
            args = p.expect(rules.sepby(parseframe, ','))
            p.expect(')')
            expr = Call(expr, args)
        elif p.accept('{'):
            args = p.expect(rules.sepby(parseframe, ','))
            p.expect('}')
            expr = Call(Sym('.ctor.%s' % expr.name), args)
        else:
            return expr

def parseexprs(p):
    if p.accept('void'):
        exprs = []
    else:
        exprs = p.expect(rules.sepby(parseframe, ','))

    if p.accept('->'):
        rets = p.expect(parseexprs)
        exprs = [FunT(exprs, rets)]

    return exprs

def parseexpr(p):
    exprs = parseexprs(p)
    if len(exprs) != 1:
        raise p.unexpected(',')
    return exprs[0]

def parsefnstmt(p):
    if p.accept('{'):
        s = parsefnstmt(p)
        p.expect('}')
        return s
    elif p.accept('return'):
        return Return(p.expect(parseexprs))
    elif p.accept('let'):
        names = p.expect(rules.sepby(Sym, ','))
        p.expect('=')
        exprs = p.expect(parseexprs)
        return Let(names, exprs)
    elif p.accept('def'):
        name = p.expect(rules.sepby(Sym, ','))
        p.expect('=')
        exprs = p.expect(parseexprs)
        return Def(name, exprs)
    else:
        exprs = p.expect(parseexprs)
        return Expr(exprs) if len(exprs) > 0 else None

def parsetypestmt(p):
    if p.accept('{'):
        s = parsetypestmt(p)
        p.expect('}')
        return s
    elif p.accept('def'):
        name = p.expect(rules.sepby(Sym, ','))
        p.expect('=')
        exprs = p.expect(parseexprs)
        return Def(name, exprs)
    elif p.accept('impl'):
        name = p.expect(Sym)
        return Impl(name)
    else:
        return None

def parseinterfacestmt(p):
    if p.accept('{'):
        s = parsetypestmt(p)
        p.expect('}')
        return s
    elif p.accept('def'):
        name = p.expect(rules.sepby(Sym, ','))
        p.expect('=')
        exprs = p.expect(parseexprs)
        return Def(name, exprs)
    else:
        return None

def parsedecl(p):
    if p.accept('fun'):
        name = p.expect(Sym)

        p.expect('(')
        args = p.expect(rules.sepby(Sym, ','))
        p.expect(')')

        p.expect('{')
        stmts = p.expect(rules.sepby(parsefnstmt, ';'))
        stmts = [s for s in stmts if s]
        p.expect('}')

        return Fun(name, args, stmts)
    elif p.accept('type') or p.accept('struct'):
        name = p.expect(Sym)
        args = []

        if p.accept('('):
            args = p.expect(rules.sepby(Sym, ','))
            p.expect(')')

        p.expect('{')
        stmts = p.expect(rules.sepby(parsetypestmt, ';'))
        stmts = [s for s in stmts if s]
        p.expect('}')

        return Struct(name, args, stmts)
    elif p.accept('interface'):
        name = p.expect(Sym)

        p.expect('{')
        stmts = p.expect(rules.sepby(parseinterfacestmt, ';'))
        stmts = [s for s in stmts if s]
        p.expect('}')

        return Interface(name, stmts)
    elif p.accept('def'):
        name = p.expect(rules.sepby(Sym, ','))
        p.expect('=')
        exprs = p.expect(parseexprs)
        return Def(name, exprs)
    elif p.accept('export'):
        name = p.expect(Sym)
        return Export(name)
    elif p.accept('extern'):
        name = p.expect(rules.sepby(Sym, ','))
        p.expect('=')
        exprs = p.expect(parseexprs)
        return Extern(name, exprs)
    else:
        raise p.unexpected()

def parse(tokens):
    tokens = iter(tokens)
    lookahead = []

    def next_():
        nonlocal tokens, lookahead
        if lookahead:
            return True

        try:
            lookahead.append(next(tokens))
            return True
        except StopIteration:
            return False

    def match(p, rule):
        nonlocal tokens, lookahead
        if not next_():
            raise p.unexpected([], rule)

        if rule != lookahead[0][0] and (
                not isinstance(rule, type) or
                not isinstance(lookahead[0][0], rule)):
            raise p.unexpected(lookahead[0][0], rule, lookahead[0][1])

        m, lookahead = lookahead[0], lookahead[1:]

        p.line = m[1] # assign line number
        try:
            m[0].line = p.line
        except AttributeError:
            pass
        return m[0]

    p = rules.Matcher(match)
    ptree = []
    while next_():
        while p.accept(';'):
            pass
        ptree.append(p.expect(parsedecl))
        if next_():
            p.expect(';')
            while p.accept(';'):
                pass

    return ptree

    
