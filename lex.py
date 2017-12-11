
import rules
import re
from tokens import *


KEYWORDS = {
    "let",
    "def",
    "is",
    "fun",
    "type",
    "return",
    "if",
    "else",
    "for",
    "while",
    "break",
    "continue",
    "blarg",
    "int",
    "void",
    "extern",
}


def lexsym(p):
    sym = p.expect('[a-zA-Z_][a-zA-Z0-9_]*')
    if sym in KEYWORDS:
        return sym
    else:
        return Sym(sym)

def lexnum(p):
    num = p.expect('[0-9]+')
    return Num(int(num))

def lexstr(p):
    _, str = p.expect('\"([^"]*)\"')
    return Str(str)

def lexws(p):
    p.expect('[ \t\v\f\r]+')

def lexnl(p):
    ws, = p.expect('\n([ \t\v\f\r]*)')

    if not hasattr(p, 'indents'):
        p.indents = [0]

    if len(ws) > p.indents[-1]:
        p.indents.append(len(ws))
        return ['{', ';']
    elif len(ws) < p.indents[-1]:
        p.indents.pop()
        return [';', '}']
    else:
        return ';'

def lex(string):
    def match(p, rule):
        nonlocal string
        m = re.match(rule, string)
        if not m:
            raise p.unexpected(string[:1], rule)

        string = string[m.end():]
        return m.groups() if m.re.groups != 0 else m.group(0)

    p = rules.Matcher(match)
    p.indent = [0]

    while string:
        p.expect(rules.choice([
            lexsym,
            lexnum,
            lexstr,
            lexnl,
            lexws,
            '=',
            ',',
            ';',
            '\{',
            '\}',
            '\(',
            '\)',
            '->',
        ]))

        # TODO maybe handle this more gracefully?
        # What did mu do?
        if p.match:
            if isinstance(p.match, list):
                for m in p.match:
                    yield m
            else:
                yield p.match

