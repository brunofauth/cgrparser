#-----------------------------------------------------------------
# cgrparser: dump_ast.py
#
# Basic example of parsing a file and dumping its parsed AST.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#-----------------------------------------------------------------
import argparse
import sys

from cgrparser.api import parse_file


def main():
    argparser = argparse.ArgumentParser('Dump AST')
    argparser.add_argument('filename',
                            default='examples/c_files/basic.c',
                            nargs='?',
                            help='name of file to parse')
    argparser.add_argument('--coord', help='show coordinates in the dump', action='store_true')
    args = argparser.parse_args()

    ast = parse_file(args.filename, preprocessor_cmd=None)
    ast.show(showcoord=args.coord)


if __name__ == "__main__":
    main()
