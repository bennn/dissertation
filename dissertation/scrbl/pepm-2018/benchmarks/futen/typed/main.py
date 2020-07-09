from os import path
#import argparse
#from collections import namedtuple
from futen import get_netlocs, execute
from Timer import Timer
from retic import Int

#bg: all test files should be in current directory when tests run


def main(n:Int)->Void:
    testfile = path.join(path.dirname(__file__), 'ssh.config.dat')
    expect = {'web': '2200', 'app': '2201', 'db': '2202'}
    with open(testfile) as fd:
        lines = fd.readlines()
        for i in range(n):
            actual = get_netlocs(lines)
        if expect != actual:
            raise AssertionError("'%s' is not equal to '%s'" % (expect, actual))

    testfile = path.join(path.dirname(__file__), 'ssh.config.dat')
    template = path.join(path.dirname(__file__), 'inventory_template.dat')
    expectfile = path.join(path.dirname(__file__), 'inventory_expect.dat')
    with open(expectfile) as fd:
        expect = ''.join(fd.readlines()).strip()
    with open(testfile) as fd:
        lines = fd.readlines()
        for i in range(n):
            result = execute(lines, template)
        if result != expect:
            raise ValueError("'%s' is not equal to '%s'" % (expect, result))
    return


t = Timer()
with t:
    main(1900)
