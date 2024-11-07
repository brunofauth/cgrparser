from __future__ import annotations

import subprocess as sp
from collections.abc import Callable

from .c_parser import CParser
from .ast_base import Node


type Factory[T] = Callable[[], T]


CPP_CMD = (
    r"cpp",
    r"-D__extension__=",
    r"-D__attribute__(x)=",
    r"-D__asm__(...)=",
    r"-D__inline=inline",
    r"-D__restrict=restrict",
    r'-Ilibs/cgrparser/utils/fake_libc_include',
)


def parse_file(
    c_src_file: str,
    preprocessor_cmd: Sequence[str] | None = None,
    parser_factory: Factory[CParser] | None = None,
) -> Node:
    """ Parse a C file using cgrparser.

        filename:
            Name of the file you want to parse.

        preprocessor_cmd:
            Whether or not to call 'cpp' and how to do so.

        parser_factory:
            A callable that returns an instance of CParser or one of its subclasses.

        When successful, an AST is returned. ParseError can be
        thrown if the file doesn't parse successfully.

        Errors from cpp will be printed out.
    """
    with open(c_src_file) as file:
        if preprocessor_cmd is not None:
            try:
                code = sp.run(preprocessor_cmd, stdin=file, stdout=sp.PIPE, text=True, check=True).stdout
            except sp.CalledProcessError as error:
                print(error.cmd)
                print(error.returncode)
                print(error.stderr)
                print(error.stdout)
                raise
        else:
            code = file.read()

    parser_factory = CParser if parser_factory is None else parser_factory
    return parser_factory().parse(code, c_src_file)

