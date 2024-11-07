#-------------------------------------------------------------------------------
# cgrparser: using_gcc_E_libc.py
#
# Similar to the using_cpp_libc.py example, but uses 'gcc -E' instead
# of 'cpp'. The same can be achieved with Clang instead of gcc. If you have
# Clang installed, simply replace 'gcc' with 'clang' here.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#-------------------------------------------------------------------------------
import sys

from cgrparser.api import parse_file


def main():
    if len(sys.argv) > 1:
        filename = sys.argv[1]
    else:
        filename = 'examples/c_files/year.c'

    ast = parse_file(filename, preprocessor_cmd=['cpp', '-E', r'-Iutils/fake_libc_include'])
    ast.show()


if __name__ == "__main__":
    main()
