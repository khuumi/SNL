#!/usr/bin/env python

import glob
import subprocess


AST_BIN = "./snl_ast"
TOTAL_PASS = 0
TOTAL_FAIL = 0


def run_ast_tests(files):
    """
    Runs tests to build ASTs from code files.
    The input files must end with the extension '.snl', which may not appear
    anywhere else in the file name.
    The expected output files must be named exactly as the input files except
    that they end with the extension '.out' instead of '.snl'.
    """
    global TOTAL_PASS
    global TOTAL_FAIL
    for test in files:
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


def run_expr_tests():
    """
    Runs all the tests in the tests/expr directory.
    """
    print 'Running expr tests...'
    expr_tests = glob.glob('tests/expr/*.snl')
    run_ast_tests(expr_tests)


def main():
    run_expr_tests()
    print '%d out of %d passing' % (TOTAL_PASS, TOTAL_PASS + TOTAL_FAIL)


if __name__ == '__main__':
    main()
