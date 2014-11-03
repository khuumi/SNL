#!/usr/bin/env python

import glob
import subprocess


AST_BIN = "./snl_ast"
TOTAL_PASS = 0
TOTAL_FAIL = 0


def run_expr_tests():
    global TOTAL_PASS
    global TOTAL_FAIL
    print 'Running expr tests...'
    expr_tests = glob.glob('tests/expr/*.snl')
    for test in expr_tests:
        with open(test.replace('.snl', '.out'), 'r') as f:
            expected_output = f.read()
        with open(test, 'r') as f:
            output = subprocess.check_output([AST_BIN, '-e'], stdin=f)
        if expected_output != output:
            TOTAL_FAIL += 1
            print 'FAIL: %s' % test
            print '\texpected: %s' % expected_output
            print '\tactual: %s' % output
        else:
            TOTAL_PASS += 1
            print 'PASS: %s' % test


def main():
    run_expr_tests()
    print '%d out of %d passing' % (TOTAL_PASS, TOTAL_PASS + TOTAL_FAIL)


if __name__ == '__main__':
    main()

