#!/usr/bin/env python3

import sys
import yaml, yaml.composer, yaml.constructor
import os
import contextlib
import subprocess
import time
import re

TIMEOUT = 3 # seconds

@contextlib.contextmanager
def cd(newdir):
    prev = os.getcwd()
    os.chdir(newdir)
    try:
        yield
    finally:
        os.chdir(prev)

def color(color, string):
    COLORS = {
        'red': 31,
        'green': 32,
    }

    return '\x1b[%dm%s\x1b[0m' % (COLORS[color], string)

def yamlloadlines(file):
    loader = yaml.Loader(file)
    def compose_node(parent, index):
        line = loader.line
        node = yaml.composer.Composer.compose_node(loader, parent, index)
        node.line = line + 1
        return node
    def construct_mapping(node, deep=False):
        mapping = yaml.constructor.Constructor.construct_mapping(loader, node, deep=deep)
        mapping['line'] = node.line
        return mapping
    loader.compose_node = compose_node
    loader.construct_mapping = construct_mapping
    return loader.get_single_data()

def test(path, dir, case):
    path = os.path.relpath(path, dir)

    with cd(dir):
        with open('test.w', 'w') as f:
            f.write(case['code'])

        try:
            res = subprocess.run([path, 'test.w'], timeout=TIMEOUT,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        except subprocess.TimeoutExpired:
            return (False, 'T', 'timeout after %ds' % TIMEOUT, None)

        err = res.stderr.decode('utf-8')
        err = re.split('(\n [^\n]*|^)\n(?=[^ ])', err)[-1].strip('\n')

        if 'error' in case:
            if not (res.returncode != 0 and case['error'] in err):
                return (False, '✗', 'expected %r:' % case['error'], err)
        else:
            if res.returncode != 0:
                return (False, '✗', 'failed:', err)

        return (True, '✓', 'success', None)
        
def main(path, tests):
    print('--- testing ---')

    if os.path.isdir(tests):
        test_dir = tests
        test_files = [
            os.path.join(tests, file) for file in os.listdir(tests)
            if os.path.isfile(os.path.join(tests, file))
            if file.endswith('.yml')]
    else:
        test_dir = os.path.dirname(tests)
        test_files = [tests]

    first_failure = None
    runtime = 0

    for test_file in test_files:
        sys.stdout.write('testing %s ' % test_file)
        sys.stdout.flush()
        start = time.time()

        with open(test_file) as f:
            cases = yamlloadlines(f)
            for case in cases:
                success, mark, note, info = test(path, test_dir, case)

                sys.stdout.write(color('green' if success else 'red', mark))
                sys.stdout.flush()

                if not success and first_failure is None:
                    first_failure = (
                        test_file, case['line'], note, info,
                        path, test_dir, case)

        end = time.time()
        sys.stdout.write('\n')
        sys.stdout.flush()
        runtime += end-start

    if first_failure:
        print(color('red', "failure in %.2fs :(" % runtime))
        print()
        print("--- first failure ---")
        print(color('red', "%s:%d %s" % first_failure[:3]))
        if first_failure[3]:
            print(first_failure[3])
        test(*first_failure[4:])
        return 1
    else:
        print(color('green', "success in %.2fs :)" % runtime))
        return 0

if __name__ == "__main__":
    sys.exit(main(*sys.argv[1:]))
