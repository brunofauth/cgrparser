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
from itertools import chain


class ASTCodeGenerator(object):

    def __init__(self, cfg_filename='_c_ast.cfg'):
        """ Initialize the code generator from a configuration
            file.
        """
        self.cfg_filename = cfg_filename
        self.node_cfg = [NodeCfg(name, contents) for (name, contents) in self.parse_cfgfile(cfg_filename)]

    def generate(self, file=None):
        """ Generates the code into file, an open file buffer.
        """
        file.write(_PROLOGUE_COMMENT.format(cfg_filename=self.cfg_filename))
        file.write(_PROLOGUE_CODE)

        for node_cfg in self.node_cfg:
            file.write(node_cfg.generate_source())
            file.write('\n\n')

    def parse_cfgfile(self, filename):
        """ Parse the configuration file and yield pairs of
            (name, contents) for each node.
        """
        with open(filename, "r") as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'):
                    continue
                colon_i = line.find(':')
                lbracket_i = line.find('[')
                rbracket_i = line.find(']')
                if colon_i < 1 or lbracket_i <= colon_i or rbracket_i <= lbracket_i:
                    raise RuntimeError("Invalid line in %s:\n%s\n" % (filename, line))

                name = line[:colon_i]
                val = line[lbracket_i + 1:rbracket_i]
                vallist = [v.strip() for v in val.split(',')] if val else []
                yield name, vallist


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
            arglist = f'(self, {", ".join(self.all_entries)}, coord=None)'
        else:
            slots = "'coord', '__weakref__'"
            arglist = '(self, coord=None)'

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
            return "    attr_names = ()"
        return f"    attr_names = ({', '.join(repr(name) for name in self.attr)},)"


_PROLOGUE_COMMENT = \
r'''#-----------------------------------------------------------------
# ** ATTENTION **
# This code was automatically generated from the file:
# {cfg_filename}
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
try: 
    from .ast_base import Node, NodeVisitor
except ImportError:
    from ast_base import Node, NodeVisitor


'''
