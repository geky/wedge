#!/usr/bin/env python3

from lex import lex
from parse import parse
from scopecheck import scopecheck
from escapecheck import escapecheck
from typecheck import typecheck
from depcheck import depcheck
from emit import emit
import llvm
from itertools import groupby


def prettify(things):
    return '\n'.join(['[']+['    %r,' % (t,) for t in things]+[']'])

def prettifytokens(things):
    return '\n'.join(['[']+['    %s,' % ', '.join(['%r' % t for t, _ in g]) for _, g in groupby(things, key=lambda v: v[1])]+[']'])

def prettifyscope(things):
    return '\n'.join(['[']+['    %s: %r,' % (s, s.__dict__) for s in things]+[']'])

def prettifytypes(things):
    return '\n'.join(['[']+['    %s: %r,' % (s, s.type) for s in things if hasattr(s, 'type')]+[']'])

def prettifydeps(things):
    return '\n'.join(['[']+['    %s: %r,' % (s, s.decl) for s in things if hasattr(s, 'decl')]+[']'])

def main(name, input, level='emit'):
    # Lexical analysis
    tokens = lex(input)

    tokens = list(tokens)
    with open('%s.l' % name, 'w') as f:
        f.write(prettifytokens(tokens))

    # Parse out the language structure
    ptree = parse(tokens)

    ptree = list(ptree)
    with open('%s.p' % name, 'w') as f:
        f.write(prettify(ptree))

    # Find dependencies, and compile into LLVM IR
    scope = scopecheck(ptree)
    with open('%s.s.p' % name, 'w') as f:
        f.write(prettifyscope(scope))
    
    typecheck(scope)
    escapecheck(scope)
    with open('%s.t.p' % name, 'w') as f:
        f.write(prettifytypes(scope))

    deps = depcheck(scope)
    with open('%s.d.p' % name, 'w') as f:
        f.write(prettifydeps(deps))

    code = emit(deps)
    with open('%s.ll' % name, 'w') as f:
        f.write(code)

    # LLVM compile time
    code = llvm.optimize(code)
    with open('%s.O3.ll' % name, 'w') as f:
        f.write(code)

    code = llvm.compile(code)
    with open('%s.s' % name, 'w') as f:
        f.write(code)

    llvm.link(code, '%s.out' % name)

if __name__ == "__main__":
    import sys
    with open(sys.argv[1]) as f:
        main(sys.argv[1], f.read(), *sys.argv[2:])
