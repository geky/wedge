
from tokens import *
from syntax import *
from type import *
import rules
import lex


def parsetype(p):
    if p.accept('int'):
        type = IntT()
    elif p.accept('void'):
        type = None
    else:
        raise p.unexpected()

    while True:
        if p.accept('->'):
            rh = p.expect(parsetype)
            type = FunT([type] if type else [], [rh] if rh else [])
        else:
            return type

def parseexpr(p):
    if p.accept(Sym):
        sym = p.match
        if p.accept('('):
            args = p.expect(rules.sepby(parseexpr, ','))
            p.expect(')')
            return Call(sym, args)
        else:
            return sym
    elif p.accept(Num):
        return p.match
    else:
        raise p.unexpected()

def parsestmt(p):
    if p.accept('{'):
        s = parsestmt(p)
        p.expect('}')
        return s
    elif p.accept('return'):
        return Return(p.expect(rules.sepby(parseexpr, ',')))
    elif p.accept('let'):
        name = p.expect(Sym)
        p.expect('=')
        value = p.expect(parseexpr)
        return Let(name, value)
    elif p.accept('def'):
        name = p.expect(Sym)
        p.expect('=')
        value = p.expect(parsetype)
        return Def(name, value)
    elif p.accept(parseexpr):
        value = p.match
        if isinstance(value, Sym) and p.accept('='):
            name = value.name
            value = p.expect(parseexpr)
            return Assign(name, value)
        else:
            return value
    else:
        p.unexpected()

def parsedecl(p):
    if p.accept('fun'):
        name = p.expect(Sym)

        p.expect('(')
        args = p.expect(rules.sepby(Sym, ','))
        p.expect(')')

        p.expect('{')
        stmts = p.expect(rules.sepby(parsestmt, ';'))
        stmts = [s for s in stmts if s]
        p.expect('}')

        return Fun(name, args, stmts)
    elif p.accept('let'):
        name = p.expect(Sym)
        p.expect('=')
        value = p.expect(parseexpr)
        return Let(name, value)
    elif p.accept('def'):
        name = p.expect(Sym)
        p.expect('=')
        value = p.expect(parsetype)
        return Def(name, value)
    elif p.accept('export'):
        name = p.expect(Sym)
        return Export(name)
    elif p.accept('extern'):
        name = p.expect(Sym)
        p.expect('=')
        value = p.expect(parsetype)
        return Extern(name, value)
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

        if rule != lookahead[0] and (
                not isinstance(rule, type) or
                not isinstance(lookahead[0], rule)):
            raise p.unexpected(lookahead[0], rule)

        m, lookahead = lookahead[0], lookahead[1:]
        return m

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

    
