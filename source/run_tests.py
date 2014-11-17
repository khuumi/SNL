#!/usr/bin/env python

import argparse
import glob
import subprocess


AST_BIN = "./snl_ast"
TOTAL_PASS = 0
TOTAL_FAIL = 0


parser = argparse.ArgumentParser(description='Run SNL tests.')
parser.add_argument('-v', action='store_true',
                    help='Print all passing tests.')
args = parser.parse_args()


def run_ast_tests(files, cmd_arg):
    """
    Runs tests to build ASTs from code files.
    The input files must end with the extension '.snl', which may not appear
    anywhere else in the file name.
    The expected output files must be named exactly as the input files except
    that they end with the extension '.out' instead of '.snl'.

    Args:
      files: a list of the names of input files, all of which end in '.snl'.
      cmd_arg: the corresponding argument to pass into the AST-printing binary,
        e.g. '-e' to test expr and '-s' for stmt.
    """
    global TOTAL_PASS
    global TOTAL_FAIL
    for test in files:
        with open(test.replace('.snl', '.out'), 'r') as f:
            expected_output = f.read()
        with open(test, 'r') as f:
            output = subprocess.check_output([AST_BIN, cmd_arg], stdin=f)
        if expected_output != output:
            TOTAL_FAIL += 1
            print '\nFAIL: %s' % test
            print 'EXPECTED:\n%s' % expected_output
            print 'ACTUAL:\n%s' % output
        else:
            TOTAL_PASS += 1
            if args.v:
                print 'PASS: %s' % test


def run_expr_tests():
    """
    Runs all the tests in the tests/expr directory.
    """
    print 'Running expr tests...'
    expr_tests = glob.glob('tests/expr/*.snl')
    run_ast_tests(expr_tests, '-e')
    print 'Finished running expr tests.\n'


def run_stmt_tests():
    """
    Runs all the tests in the tests/stmt directory.
    """
    print 'Running stmt tests...'
    stmt_tests = glob.glob('tests/stmt/*.snl')
    run_ast_tests(stmt_tests, '-s')
    print 'Finished running stmt tests.\n'


def run_program_tests():
    """
    Runs all the tests in the tests/program directory.
    """
    print 'Running program tests...'
    program_tests = glob.glob('tests/program/*.snl')
    run_ast_tests(program_tests, '-p')
    print 'Finished running program tests.\n'


def main():
    run_expr_tests()
    run_stmt_tests()
    run_program_tests()
    print '%d out of %d tests passing.' % (TOTAL_PASS, TOTAL_PASS + TOTAL_FAIL)


if __name__ == '__main__':
    main()
