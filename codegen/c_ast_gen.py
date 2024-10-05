#-----------------------------------------------------------------
# _ast_gen.py
#
# Generates the AST Node classes from a specification given in
# a configuration file
#
# The design of this module was inspired by astgen.py from the
# Python 2.5 code-base.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#-----------------------------------------------------------------
from __future__ import annotations
from itertools import chain, starmap
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from typing import TextIO
    from collections.abc import Iterable
    from pathlib import Path
    type FilePath = str | Path


def generate_ast_code(cfg_fp: FilePath, out_fp: FilePath) -> None:
    """Generates the code into file, an open file buffer."""
    node_cfgs: list[NodeCfg] = list(starmap(NodeCfg, parse_astcfg_file(cfg_fp)))

    with open(out_fp, "w") as out_file:
        out_file.write(_PROLOGUE_COMMENT.format(cfg_fp=cfg_fp))
        out_file.write(_PROLOGUE_CODE)

        for node_cfg in node_cfgs:
            out_file.write(node_cfg.generate_source())
            out_file.write('\n\n')


def _parse_astcfg_file(cfg_fp: FilePath, lines: Iterable[str]) -> Iterable[tuple[str, list[str]]]:
    for i, line in enumerate(lines):
        line = line.strip()
        if line == "" or line.startswith('#'):
            continue
        colon_idx = line.find(':')
        lbracket_idx = line.find('[')
        rbracket_idx = line.find(']')
        if colon_idx < 1 or lbracket_idx <= colon_idx or rbracket_idx <= lbracket_idx:
            raise SyntaxError(f"{cfg_fp}:{i+1}: Invalid syntax on {line!r}")

        node_name = line[:colon_idx].strip()
        val = line[lbracket_idx + 1:rbracket_idx]
        node_children = [v.strip() for v in val.split(',')] if val else []
        yield node_name, node_children


def parse_astcfg_file(cfg_fp: FilePath) -> Iterable[tuple[str, list[str]]]:
    """Yield pairs of (name, contents) for each node."""
    with open(cfg_fp) as cfg_contents:
        yield from _parse_astcfg_file(cfg_fp, cfg_contents)


class NodeCfg(object):
    """ Node configuration.

        name: node name
        contents: a list of contents - attributes and child nodes
        See comment at the top of the configuration file for details.
    """

    def __init__(self, name, contents):
        self.name = name
        self.all_entries = []
        self.attr = []
        self.child = []
        self.seq_child = []

        for entry in contents:
            clean_entry = entry.rstrip('*')
            self.all_entries.append(clean_entry)

            if entry.endswith('**'):
                self.seq_child.append(clean_entry)
            elif entry.endswith('*'):
                self.child.append(clean_entry)
            else:
                self.attr.append(entry)

    def generate_source(self):
        return '\n\n'.join([
            f"class {self.name}(Node):",
            self._gen_init(),
            self._gen_children(),
            self._gen_iter(),
            self._gen_attr_names(),
        ])

    def _gen_init(self):
        src_lines = []

        if len(self.all_entries) != 0:
            slots = ", ".join(repr(slot) for slot in chain(self.all_entries, ["coord", "__weakref__"]))
            arglist = f'(self, {", ".join(self.all_entries)}, coord: Coord | None = None)'
        else:
            slots = "'coord', '__weakref__'"
            arglist = '(self, coord: Coord | None = None)'

        src_lines.append("    __slots__ = (%s)" % slots)
        src_lines.append("    def __init__%s:" % arglist)

        for name in chain(self.all_entries, ['coord']):
            src_lines.append("        self.%s = %s" % (name, name))

        return "\n".join(src_lines)

    def _gen_children(self):
        src_lines = ['    def children(self):']

        if len(self.all_entries) == 0:
            src_lines.append('        return ()')
            return '\n'.join(src_lines)

        src_lines.append('        nodes = []')

        for child in self.child:
            src_lines.append('        '
                                f'if self.{child} is not None: nodes.append(("{child}", self.{child}))')

        for child in self.seq_child:
            src_lines.append('        '
                                f'for i, child in enumerate(self.{child} or []):'
                                f' nodes.append(("{child}[%d]" % i, child))')

        src_lines.append('        return tuple(nodes)')
        return '\n'.join(src_lines)

    def _gen_iter(self):
        src_lines = ['    def __iter__(self):']

        if len(self.all_entries) == 0:
            # Empty generator
            src_lines.append('        return\n        yield')
            return '\n'.join(src_lines)

        for child in self.child:
            src_lines.append(f'        if self.{child} is not None: yield self.{child}')

        for child in self.seq_child:
            src_lines.append(f'        yield from (self.{child} or [])')

        if not (self.child or self.seq_child):
            # Empty generator
            src_lines.append('        return\n        yield')

        return '\n'.join(src_lines)

    def _gen_attr_names(self):
        if len(self.attr) == 0:
            return "    attr_names = ()\n"
        return f"    attr_names = ({', '.join(repr(name) for name in self.attr)},)\n"


_PROLOGUE_COMMENT = \
r'''#-----------------------------------------------------------------
# ** ATTENTION **
# This code was automatically generated from the file:
# {cfg_fp}
#
# Do not modify it directly. Modify the configuration file and
# run the generator again.
# ** ** *** ** **
#
# pycparser: c_ast.py
#
# AST Node classes.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#-----------------------------------------------------------------
'''

_PROLOGUE_CODE = r'''
from __future__ import annotations
from .ast_base import Node, NodeVisitor
import typing

if typing.TYPE_CHECKING:
    from .plyparser import Coord


'''
