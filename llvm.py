# We don't actuall assemble ourselves, we leave that up to llvm
import subprocess
import sys

def llvm(code):
    proc = subprocess.Popen(['llc', '-o', '-'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=sys.stderr)

    asm, _ = proc.communicate(code.encode('utf-8'))
    return asm.decode()
