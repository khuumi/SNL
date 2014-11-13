#!/usr/bin/env python

import subprocess
import sys


AST_BIN = "./snl_ast"


def process_code(filename):
    """
    Builds AST from a single code file
    Input files end with '.snl' and will produce an output of the same name
    except with extension '.snlast'
    """
    print 'Start processing code...'
    with open(filename, 'r') as code:
        output = subprocess.check_output([AST_BIN, '-e'], stdin=code)
    print 'Outputting AST'
    with open(filename.replace('.snl', '.snlb'), 'w') as out:
        out.write(output)


def check_argc(argc):
    if argc != 2:
        print 'Incorrect format: should be python get_snl_ast.py <file>'
        sys.exit()


def main():
    check_argc(len(sys.argv))
    f = str(sys.argv[1])
    process_code(f)


if __name__ == '__main__':
    main()
