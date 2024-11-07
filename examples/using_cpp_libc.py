#-----------------------------------------------------------------
# cgrparser: using_cpp_libc.py
#
# Shows how to use the provided 'cpp' (on Windows, substitute for
# the 'real' cpp if you're on Linux/Unix) and "fake" libc includes
# to parse a file that includes standard C headers.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#-----------------------------------------------------------------
import sys

from cgrparser.api import parse_file


def main():
    if len(sys.argv) > 1:
        filename = sys.argv[1]
    else:
        filename = 'examples/c_files/year.c'

    ast = parse_file(filename, preprocessor_cmd=['cpp', r'-Iutils/fake_libc_include'])
    ast.show()


if __name__ == "__main__":
    main()
