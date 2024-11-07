#-----------------------------------------------------------------
# cgrparser: rewrite_ast.py
#
# Tiny example of rewriting a AST node
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#-----------------------------------------------------------------
import sys

from cgrparser import c_parser

text = r"""
void func(void)
{
  x = 1;
}
"""


def main():
    parser = c_parser.CParser()
    ast = parser.parse(text)
    print("Before:")
    ast.show(offset=2)

    assign = ast.ext[0].body.block_items[0]
    assign.lvalue.name = "y"
    assign.rvalue.value = 2

    print("After:")
    ast.show(offset=2)


if __name__ == '__main__':
    main()
