from __future__ import annotations

from collections.abc import Sequence
import typing
import sys

if typing.TYPE_CHECKING:
    from collections.abc import Iterable
    from typing import Any


def _repr(obj):
    """
    Get the representation of an object, with dedicated pprint-like format for lists.
    """
    if isinstance(obj, list):
        return '[' + (',\n '.join((_repr(e).replace('\n', '\n ') for e in obj))) + '\n]'
    else:
        return repr(obj)


_types: dict[str, set[str]] = {}


class Node(object):
    __slots__ = ()
    _eq_ignore = ()
    attr_names: tuple[str, ...] = ()
    children_names: tuple[str, ...] = ()

    def __setattr__(self, name: str, value: Any) -> None:
        if name[0] != '_':
            _types.setdefault(f"{self.__class__.__name__}.{name}", set()).add(type(value))
        super().__setattr__(name, value)

    def __iter__(self) -> Iterable[Node]:
        for field in self.children_names:
            if isinstance(value := getattr(self, field), Sequence):
                yield from value
            elif value is not None:
                yield value

    def _children(self, skip_none: bool = True) -> Iterable[tuple[str, Any]]:
        for field in self.children_names:
            if isinstance(value := getattr(self, field), Sequence):
                yield from map(lambda x: (f"{field}[{x[0]}]", x[1]), enumerate(value))
            elif not skip_none or value is not None:
                yield (field, value)

    def children(self, skip_none: bool = True) -> tuple[tuple[str, Any], ...]:
        return tuple(self._children(skip_none=skip_none))

    def __eq__(self, other: object) -> bool:
        if type(self) != type(other):
            return False
        slot: str
        for slot in self.__slots__[:-2]:
            if slot in self._eq_ignore:
                continue
            mine = getattr(self, slot)
            others = getattr(other, slot)
            if mine == ... or others == ...:
                continue
            if mine != others:
                return False
        return True

    def __repr__(self):
        """ Generates a python representation of the current node
        """
        result = self.__class__.__name__ + '('

        indent = ''
        separator = ''
        name: str
        for name in self.__slots__[:-2]:
            result += separator
            result += indent
            result += name + '=' + (_repr(getattr(self, name)).replace(
                '\n', '\n  ' + (' ' * (len(name) + len(self.__class__.__name__)))))

            separator = ','
            indent = '\n ' + (' ' * len(self.__class__.__name__))

        result += indent + ')'

        return result

    def show(self,
                buf=sys.stdout,
                offset=0,
                attrnames=False,
                nodenames=False,
                showcoord=False,
                _my_node_name=None):
        """ Pretty print the Node and all its attributes and
            children (recursively) to a buffer.

            buf:
                Open IO buffer into which the Node is printed.

            offset:
                Initial offset (amount of leading spaces)

            attrnames:
                True if you want to see the attribute names in
                name=value pairs. False to only see the values.

            nodenames:
                True if you want to see the actual node names
                within their parents.

            showcoord:
                Do you want the coordinates of each Node to be
                displayed.
        """
        lead = ' ' * offset
        if nodenames and _my_node_name is not None:
            buf.write(lead + self.__class__.__name__ + ' <' + _my_node_name + '>: ')
        else:
            buf.write(lead + self.__class__.__name__ + ': ')

        if len(self.attr_names) != 0:
            if attrnames:
                nvlist = [(n, getattr(self, n)) for n in self.attr_names]
                attrstr = ', '.join('%s=%s' % nv for nv in nvlist)
            else:
                vlist = [getattr(self, n) for n in self.attr_names]
                attrstr = ', '.join('%s' % v for v in vlist)
            buf.write(attrstr)

        if showcoord:
            buf.write(' (at %s)' % self.coord)
        buf.write('\n')

        for (child_name, child) in self.children():
            child.show(buf,
                        offset=offset + 2,
                        attrnames=attrnames,
                        nodenames=nodenames,
                        showcoord=showcoord,
                        _my_node_name=child_name)


class NodeVisitor(object):
    """ A base NodeVisitor class for visiting c_ast nodes.
        Subclass it and define your own visit_XXX methods, where
        XXX is the class name you want to visit with these
        methods.

        For example:

        class ConstantVisitor(NodeVisitor):
            def __init__(self):
                self.values = []

            def visit_Constant(self, node):
                self.values.append(node.value)

        Creates a list of values of all the constant nodes
        encountered below the given node. To use it:

        cv = ConstantVisitor()
        cv.visit(node)

        Notes:

        *   generic_visit() will be called for AST nodes for which
            no visit_XXX method was defined.
        *   The children of nodes for which a visit_XXX was
            defined will not be visited - if you need this, call
            generic_visit() on the node.
            You can use:
                NodeVisitor.generic_visit(self, node)
        *   Modeled after Python's own AST visiting facilities
            (the ast module of Python 3.0)
    """

    _method_cache = None

    def visit(self, node):
        """ Visit a node.
        """

        if self._method_cache is None:
            self._method_cache = {}

        visitor = self._method_cache.get(node.__class__.__name__, None)
        if visitor is None:
            method = 'visit_' + node.__class__.__name__
            visitor = getattr(self, method, self.generic_visit)
            self._method_cache[node.__class__.__name__] = visitor

        return visitor(node)

    def generic_visit(self, node):
        """ Called if no explicit visitor function exists for a
            node. Implements preorder visiting of the node.
        """
        for c in node:
            self.visit(c)
