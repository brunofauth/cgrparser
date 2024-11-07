#-----------------------------------------------------------------
# cgrparser: func_calls.py
#
# Using cgrparser for printing out all the calls of some function
# in a C file.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#-----------------------------------------------------------------
import sys

from cgrparser.api import parse_file
from cgrparser import c_ast


# A visitor with some state information (the funcname it's looking for)
class FuncCallVisitor(c_ast.NodeVisitor):
    def __init__(self, funcname):
        self.funcname = funcname

    def visit_FuncCall(self, node):
        if node.name.name == self.funcname:
            print('%s called at %s' % (self.funcname, node.name.coord))
        # Visit args in case they contain more func calls.
        if node.args:
            self.visit(node.args)


def show_func_calls(filename, funcname):
    ast = parse_file(filename, preprocessor_cmd=['cpp'])
    v = FuncCallVisitor(funcname)
    v.visit(ast)


def main():
    if len(sys.argv) > 2:
        filename = sys.argv[1]
        func = sys.argv[2]
    else:
        filename = 'examples/c_files/basic.c'
        func = 'foo'

    show_func_calls(filename, func)


if __name__ == "__main__":
    main()
