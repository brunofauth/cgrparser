#-----------------------------------------------------------------
# cgrparser: _build_tables.py
#
# A dummy for generating the lexing/parsing tables and and
# compiling them into .pyc for faster execution in optimized mode.
# Also generates AST code from the configuration file.
# Should be called from the cgrparser directory.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#-----------------------------------------------------------------

from __future__ import annotations
import importlib
import importlib.util
from pathlib import Path
import typing
import sys

from c_ast_gen import generate_ast_code
from clean_cached_files import cleanup

if typing.TYPE_CHECKING:
    from types import ModuleType

REPO_ROOT: Path = Path(__file__).parent.parent
CGRPARSER_ROOT: Path = REPO_ROOT / 'src/cgrparser'


def import_from_fp(module_name: str, module_fp: Path) -> ModuleType:
    module_spec = importlib.util.spec_from_file_location(module_name, module_fp)
    module = importlib.util.module_from_spec(module_spec)
    sys.modules[module_name] = module
    module_spec.loader.exec_module(module)
    return module


if __name__ == "__main__":
    generate_ast_code(
        cfg_fp=REPO_ROOT / 'codegen/c_ast.cfg',
        out_fp=CGRPARSER_ROOT / 'c_ast.py',
    )

    cgrparser = import_from_fp("cgrparser", CGRPARSER_ROOT / "__init__.py")
    # Generates the tables
    cgrparser.c_parser.CParser(
        lex_optimize=True,
        lextab='cgrparser.lextab',
        yacc_optimize=True,
        yacctab='cgrparser.yacctab',
        yacc_debug=False,
        taboutputdir=CGRPARSER_ROOT,
    )

    # (re)build cache for generated optimized files
    cleanup("..", targetfiles=[])
    import_from_fp("cgrparser.lextab", CGRPARSER_ROOT / "lextab.py")
    import_from_fp("cgrparser.yacctab", CGRPARSER_ROOT / "yacctab.py")
    import_from_fp("cgrparser.c_ast", CGRPARSER_ROOT / "c_ast.py")
