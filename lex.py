
import rules
import re
from wtokens import *


KEYWORDS = {
    "let",
    "def",
    "fun",
    "type",
    "struct",
    "interface",
    "impl",
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
    "export"
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
    str = p.expect('\"([^"]*)\"')
    return Str(str)

def lexws(p):
    p.expect('[ \t\v\f\r]+')

def lexnl(p):
    ws, = p.expect('\n([ \t\v\f\r]*)')
    p.line += 1

    if not hasattr(p, 'indents'):
        p.indents = [0]

    if len(ws) > p.indents[-1]:
        p.indents.append(len(ws))
        return '{'
    elif len(ws) < p.indents[-1]:
        p.indents.pop()
        return [';', '}']
    else:
        return ';'

def lexcomment(p):
    p.expect('//[^\n]*')

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
    p.line = 1

    while string:
        line = p.line

        p.expect(rules.choice([
            lexcomment,
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
                    yield (m, line)
            else:
                yield (p.match, line)

