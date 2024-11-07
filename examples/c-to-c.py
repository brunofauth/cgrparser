#------------------------------------------------------------------------------
# cgrparser: c-to-c.py
#
# Example of using cgrparser.c_generator, serving as a simplistic translator
# from C to AST and back to C.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#------------------------------------------------------------------------------
import sys

from cgrparser.api import parse_file
from cgrparser import c_generator


def translate_to_c(filename):
    """ Simply use the c_generator module to emit a parsed AST.
    """
    ast = parse_file(filename, preprocessor_cmd=['cpp'])
    generator = c_generator.CGenerator()
    print(generator.visit(ast))


def main():
    if len(sys.argv) > 1:
        translate_to_c(sys.argv[1])
    else:
        print("Please provide a filename as argument")


if __name__ == "__main__":
    main()
