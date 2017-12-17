# We don't actuall assemble ourselves, we leave that up to llvm
import subprocess
import sys


def optimize(code):
    proc = subprocess.Popen(['opt', '-O3', '-S'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=sys.stderr)

    output, _ = proc.communicate(code.encode('utf-8'))
    return output.decode()

def compile(code):
    proc = subprocess.Popen(['llc', '-o', '-'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=sys.stderr)

    output, _ = proc.communicate(code.encode('utf-8'))
    return output.decode()

def link(code, filename):
    # TODO hmm
    proc = subprocess.Popen(['clang', '-xassembler', '-', '-o', filename],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=sys.stderr)

    output, _ = proc.communicate(code.encode('utf-8'))
    return output.decode()

