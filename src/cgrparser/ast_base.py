from __future__ import annotations

from collections.abc import Sequence
import contextlib
import dataclasses as dc
import functools
import io
import typing
import sys

if typing.TYPE_CHECKING:
    from collections.abc import Iterable, Callable
    from typing import Any, Self, IO
    type Visit[T] = Callable[[Node], Iterable[T]]


_g_repr_level: int = 0
REPR_INDENT: str = '    '


@functools.cache
def dummy_node[**P, T: type[Node]](node_type: type[Node]) -> Callable[P, T]:
    """Returns an alternative constructor for a Node type, in which all
    parameters default to '...', to produce 'dummy' types, useful when
    comparing/debugging AST structures."""

    default_args: dict[str, ellipsis] = {slot: ... for slot in node_type.__slots__}
    del default_args["coord"]
    del default_args["__weakref__"]

    @functools.wraps(node_type)
    def _ctor(**kwargs) -> T:
        return node_type(**{**default_args, **kwargs})
    return _ctor


def _repr(obj: Any, *, inline: bool = False) -> str:
    """
    Get the representation of an object, with dedicated pprint-like format for lists.
    """
    global _g_repr_level

    if not isinstance(obj, list):
        return repr(obj)

    if len(obj) == 0:
        return '[]'
    if len(obj) == 1:
        return f'[{_repr(obj[0])}]'

    with io.StringIO() as result:
        write = functools.partial(print, sep='', file=result)
        write('[')
        _g_repr_level += 1
        for item in obj:
            write(REPR_INDENT * _g_repr_level, _repr(item), ',')
        _g_repr_level -= 1
        write(_g_repr_level * REPR_INDENT, ']' if inline else '],', end='' if inline else '\n')
        return result.getvalue()


class Node(object):
    __slots__ = ()
    _eq_ignore = ()
    attr_names: tuple[str, ...] = ()
    children_names: tuple[str, ...] = ()

    def __iter__(self) -> Iterable[Self]:
        for field in self.children_names:
            if isinstance(value := getattr(self, field), Sequence):
                yield from value
            elif value is not None:
                yield value


    @classmethod
    @contextlib.contextmanager
    def ctx_eq_ignore(cls, *attrs):
        try:
            old_value = cls._eq_ignore
            cls._eq_ignore = attrs
            yield
        finally:
            cls._eq_ignore = old_value


    def _children(self, skip_none: bool = True) -> Iterable[tuple[str, Any]]:
        for field in self.children_names:
            if isinstance(value := getattr(self, field), Sequence):
                yield from map(lambda x: (f"{field}[{x[0]}]", x[1]), enumerate(value))
            elif not skip_none or value is not None:
                yield (field, value)


    def children(self, skip_none: bool = True) -> tuple[tuple[str, Any], ...]:
        return tuple(self._children(skip_none=skip_none))


    def children_proper(self) -> Iterable[tuple[str, Node | list[Node]]]:
        for field in self.children_names:
            yield (field, getattr(self, field))


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
        global _g_repr_level

        slots = self.__slots__[:-2]
        if len(slots) == 0:
            return f'{self.__class__.__name__}()'

        if len(slots) == 1:
            val = _repr(getattr(self, slots[0]), inline=True)
            return f'{self.__class__.__name__}({slots[0]}={val})'

        with io.StringIO() as result:
            write = functools.partial(print, sep='', file=result)
            write(self.__class__.__name__, '(')

            _g_repr_level += 1
            for name in slots:
                write(REPR_INDENT * _g_repr_level, name, '=', _repr(getattr(self, name)), ',')
            _g_repr_level -= 1

            write(REPR_INDENT * _g_repr_level, ')', end='')
            return result.getvalue()


    def show(
        self,
        buf: IO[str] = sys.stdout,
        offset: int = 0,
        attrnames: bool = False,
        nodenames: bool = False,
        showcoord: bool = False,
        _my_node_name=None,
    ) -> None:
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
            child.show(
                buf,
                offset=offset + 2,
                attrnames=attrnames,
                nodenames=nodenames,
                showcoord=showcoord,
                _my_node_name=child_name,
            )


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
        """ Visit a node. """

        if self._method_cache is None:
            self._method_cache = {}

        visitor = self._method_cache.get(node.__class__.__name__)
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


@dc.dataclass(slots=True, kw_only=True)
class NodeVisitorIter[T]:
    """ A base NodeVisitor class for visiting c_ast nodes.
        Subclass it and define your own visit_XXX methods, where
        XXX is the class name you want to visit with these
        methods.

        For example:

        class ConstantVisitor(NodeVisitorIter):
            def __init__(self):
                self.values = []

            def visit_Constant(self, node):
                self.values.append(node.value)
                yield blablabla

        Creates a list of values of all the constant nodes
        encountered below the given node. To use it:

        cv = ConstantVisitor()
        cv.visit(node)

        Notes:

        * generic_visit() will be called for nodes for which
          no visit_XXX method was defined.
        * The children of nodes for which a visit_XXX was
          defined will not be visited - if you need this, call
          generic_visit() on the node. You can use:
              NodeVisitor.generic_visit(self, node)
        * Modeled after Python's own 'ast' module
    """

    _method_cache: dict[str, Visit[T]] = dc.field(default_factory=dict)

    def visit(self, node: Node) -> Iterable[T]:
        """ Visit a node. """

        visitor: Visit[T] | None = self._method_cache.get(node.__class__.__name__)
        if visitor is None:
            method = 'visit_' + node.__class__.__name__
            visitor = getattr(self, method, self.generic_visit)
            self._method_cache[node.__class__.__name__] = visitor

        yield from visitor(node)

    def generic_visit(self, node: Node) -> Iterable[T]:
        """ Called if no explicit visitor function exists for a
            node. Implements preorder visiting of the node.
        """
        for c in node:
            yield from self.visit(c)
