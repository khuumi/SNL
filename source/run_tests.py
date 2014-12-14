#!/usr/bin/env python

import argparse
import glob
import os
import shutil
import subprocess
import tempfile


AST_BIN = "./snl"
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
        try:
            output = subprocess.check_output([AST_BIN, cmd_arg, test])
        except subprocess.CalledProcessError as e:
            print 'Error processing %s\n' % test, e
            TOTAL_FAIL += 1
            continue
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


def run_failing_tests():
    """
    Runs all the tests in the tests/failing directory.
    """
    global TOTAL_PASS
    global TOTAL_FAIL
    print 'Running failing tests...'
    failing_tests = glob.glob('tests/failing/*.snl')
    with open(os.devnull, 'wb') as DEVNULL:
        for test in failing_tests:
            try:
                output = subprocess.check_output([AST_BIN, '-p', test],
                                                 stderr=DEVNULL)
                print '\nFAIL: %s' % test
                TOTAL_FAIL += 1
            except subprocess.CalledProcessError:
                TOTAL_PASS += 1
                if args.v:
                    print 'PASS: %s' % test
    print 'Finished running failing tests.\n'


def run_java_tests():
    """
    Runs all the tests in the tests/java directory.
    """
    global TOTAL_PASS
    global TOTAL_FAIL
    print 'Running compiler tests...'
    compiler_tests = glob.glob('tests/java/*.snl')
    temp_dir = tempfile.mkdtemp()
    for test in compiler_tests:
        with open(test.replace('.snl', '.out'), 'r') as f:
            expected_output = f.read()
        try:
            name = test[len('tests/java/'):-len('.snl')]
            output = ''
            subprocess.call([AST_BIN,
                             '-j', test,
                             '--output_path', temp_dir])
            subprocess.call(['javac', '-d', temp_dir,
                              'SNLObject.java',
                              os.path.join(temp_dir, name + '.java')])
            output = subprocess.check_output(['java', '-classpath', temp_dir, name])

        except subprocess.CalledProcessError as e:
            print 'Error processing %s\n' % test, e
            TOTAL_FAIL += 1
            continue
        if expected_output != output:
            TOTAL_FAIL += 1
            print '\nFAIL: %s' % test
            print 'EXPECTED:\n%s' % expected_output
            print 'ACTUAL:\n%s' % output
        else:
            TOTAL_PASS += 1
            if args.v:
                print 'PASS: %s' % test
    shutil.rmtree(temp_dir)
    print 'Finished running compiler tests.\n'


def main():
    run_expr_tests()
    run_stmt_tests()
    run_program_tests()
    run_failing_tests()
    run_java_tests()
    print '%d out of %d tests passing.' % (TOTAL_PASS, TOTAL_PASS + TOTAL_FAIL)


if __name__ == '__main__':
    main()
