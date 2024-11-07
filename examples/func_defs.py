#-----------------------------------------------------------------
# cgrparser: func_defs.py
#
# Using cgrparser for printing out all the functions defined in a
# C file.
#
# This is a simple example of traversing the AST generated by
# cgrparser. Call it from the root directory of cgrparser.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#-----------------------------------------------------------------
import sys

from cgrparser.api import parse_file
from cgrparser import c_ast


# A simple visitor for FuncDef nodes that prints the names and
# locations of function definitions.
class FuncDefVisitor(c_ast.NodeVisitor):
    def visit_FuncDef(self, node):
        print('%s at %s' % (node.decl.name, node.decl.coord))


def show_func_defs(filename):
    # Note that cpp is used. Provide a path to your own cpp or
    # make sure one exists in PATH.
    ast = parse_file(filename, preprocessor_cmd=['cpp', r'-Iutils/fake_libc_include', '-Itests/c_files'])

    v = FuncDefVisitor()
    v.visit(ast)


def main():
    if len(sys.argv) > 1:
        filename = sys.argv[1]
    else:
        filename = 'examples/c_files/memmgr.c'

    show_func_defs(filename)


if __name__ == "__main__":
    main()
