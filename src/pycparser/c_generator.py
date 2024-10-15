#------------------------------------------------------------------------------
# pycparser: c_generator.py
#
# C code generator from pycparser AST nodes.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#------------------------------------------------------------------------------
from __future__ import annotations

from . import c_ast
import dataclasses as dc
import io
import itertools
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from collections.abc import Iterable, Callable
    from typing import Literal
    type Predicate[T] = Callable[[T], bool]

# Precedence map of binary operators:
_OPERATOR_PRECEDENCE = {
   # Should be in sync with c_parser.CParser.precedence
   # Higher numbers are stronger binding
    '||': 0,   # weakest binding
    '&&': 1,
    '|': 2,
    '^': 3,
    '&': 4,
    '==': 5,
    '!=': 5,
    '>': 6,
    '>=': 6,
    '<': 6,
    '<=': 6,
    '>>': 7,
    '<<': 7,
    '+': 8,
    '-': 8,
    '*': 9,
    '/': 9,
    '%': 9   # strongest binding
}


@dc.dataclass(slots=True)
class CGenerator(object):
    """ Uses the same visitor pattern as c_ast.NodeVisitor, but modified to
        return a value from each visit method, using string accumulation in
        generic_visit.
    """

    output_stream: io.TextIOBase
    reduce_parentheses: bool = False
    sizeof_indent: int = 4
    indent_level: int = 0

    def _node_as_str(self, node: c_ast.Node) -> str:
        with io.StringIO() as result:
            generator = CGenerator(
                output_stream=result,
                reduce_parentheses=self.reduce_parentheses,
                sizeof_indent=self.sizeof_indent,
                indent_level=self.indent_level,
            )
            generator.visit(node)
            return result.getvalue()

    def _make_indent(self) -> None:
        self.output_stream.write(' ' * self.indent_level * self.sizeof_indent)

    def visit(self, node: c_ast.Node) -> None:
        method = 'visit_' + node.__class__.__name__
        getattr(self, method, self.generic_visit)(node)

    def generic_visit(self, node: c_ast.Node | None) -> None:
        if node is None:
            return
        for _, child in node.children():
            self.visit(child)

    def visit_Constant(self, n: c_ast.Constant) -> None:
        self.output_stream.write(n.value)

    def visit_ID(self, n: c_ast.ID) -> None:
        self.output_stream.write(n.name)

    def visit_Pragma(self, n: c_ast.Pragma) -> None:
        self.output_stream.write(f'#pragma {n.string or ""}')

    def visit_ArrayRef(self, n: c_ast.ArrayRef) -> None:
        self._parenthesize_unless_simple(n.name)
        self.output_stream.write('[')
        self.visit(n.subscript)
        self.output_stream.write(']')

    def visit_StructRef(self, n: c_ast.StructRef) -> None:
        self._parenthesize_unless_simple(n.name)
        self.output_stream.write(n.type)
        self.visit(n.field)

    def visit_FuncCall(self, n: c_ast.FuncCall) -> None:
        self._parenthesize_unless_simple(n.name)
        self.output_stream.write('(')
        self.visit(n.args)
        self.output_stream.write(')')

    def visit_UnaryOp(self, n: c_ast.UnaryOp) -> None:
        if n.op == 'sizeof':
            self.output_stream.write('sizeof(')
            self.visit(n.expr)
            self.output_stream.write(')')
            return

        if n.op == 'p++':
            self._parenthesize_unless_simple(n.expr)
            self.output_stream.write('++')
            return
        if n.op == 'p--':
            self._parenthesize_unless_simple(n.expr)
            self.output_stream.write('--')
            return
        self.output_stream.write(n.op)
        self._parenthesize_unless_simple(n.expr)

    def visit_BinaryOp(self, n: c_ast.BinaryOp) -> None:
        # Note: all binary operators are left-to-right associative

        # If `n.left.op` has a stronger or equally binding precedence in
        # comparison to `n.op`, no parenthesis are needed for the left:
        # e.g., `(a*b) + c` is equivalent to `a*b + c`, as well as
        #       `(a+b) - c` is equivalent to `a+b - c` (same precedence).
        # If the left operator is weaker binding than the current, then
        # parentheses are necessary:
        # e.g., `(a+b) * c` is NOT equivalent to `a+b * c`.
        self._parenthesize_if(
            n.left, lambda d: not (self._is_simple_node(d) or self.reduce_parentheses and isinstance(
                d, c_ast.BinaryOp) and _OPERATOR_PRECEDENCE[d.op] >= _OPERATOR_PRECEDENCE[n.op]))

        self.output_stream.write(' ')
        self.output_stream.write(n.op)
        self.output_stream.write(' ')

        # If `n.right.op` has a stronger -but not equal- binding precedence,
        # parenthesis can be omitted on the right:
        # e.g., `a + (b*c)` is equivalent to `a + b*c`.
        # If the right operator is weaker or equally binding, then parentheses
        # are necessary:
        # e.g., `a * (b+c)` is NOT equivalent to `a * b+c` and
        #       `a - (b+c)` is NOT equivalent to `a - b+c` (same precedence).
        self._parenthesize_if(
            n.right, lambda d: not (self._is_simple_node(d) or self.reduce_parentheses and isinstance(
                d, c_ast.BinaryOp) and _OPERATOR_PRECEDENCE[d.op] > _OPERATOR_PRECEDENCE[n.op]))

    def visit_Assignment(self, n: c_ast.Assignment) -> None:
        self.visit(n.lvalue)
        self.output_stream.write(' ')
        self.output_stream.write(n.op)
        self.output_stream.write(' ')
        self._parenthesize_if(n.rvalue, lambda n: isinstance(n, c_ast.Assignment))

    def visit_IdentifierType(self, n: c_ast.IdentifierType) -> None:
        for name in n.names[:-1]:
            self.output_stream.write(str(name))
            self.output_stream.write(' ')
        if len(n.names) != 0:
            self.output_stream.write(str(n.names[-1]))

    def _visit_expr(self, n) -> None:
        if isinstance(n, c_ast.InitList):
            self.output_stream.write('{')
            self.visit(n)
            self.output_stream.write('}')
            return
        if isinstance(n, c_ast.ExprList):
            self.output_stream.write('(')
            self.visit(n)
            self.output_stream.write(')')
            return
        self.visit(n)

    def visit_Decl(self, n: c_ast.Decl, no_type: bool = False) -> None:
        # no_type is used when a Decl is part of a DeclList, where the type is
        # explicitly only for the first declaration in a list.
        if no_type:
            self.output_stream.write(n.name)
        else:
            self._generate_decl(n)
        if n.bitsize:
            self.output_stream.write(' : ')
            self.visit(n.bitsize)
        if n.init:
            self.output_stream.write(' = ')
            self._visit_expr(n.init)

    def visit_DeclList(self, n: c_ast.DeclList) -> None:
        self.visit(n.decls[0])
        if len(n.decls) <= 1:
            return
        self.output_stream.write(', ')
        for decl in n.decls[1:]:
            self.visit_Decl(decl, no_type=True)

    def visit_Typedef(self, n: c_ast.Typedef) -> None:
        self.output_stream.write(' '.join(map(str, n.storage)))
        self.output_stream.write(' ')
        self._generate_type(n.type)

    def visit_Cast(self, n: c_ast.Cast) -> None:
        self.output_stream.write('(')
        self._generate_type(n.to_type, emit_declname=False)
        self.output_stream.write(') ')
        self._parenthesize_unless_simple(n.expr)

    def visit_ExprList(self, n: c_ast.ExprList) -> None:
        for expr in n.exprs[:-1]:
            self._visit_expr(expr)
            self.output_stream.write(', ')
        if len(n.exprs) != 0:
            self._visit_expr(n.exprs[-1])

    def visit_InitList(self, n: c_ast.InitList) -> None:
        for expr in n.exprs[:-1]:
            self._visit_expr(expr)
            self.output_stream.write(', ')
        if len(n.exprs) != 0:
            self._visit_expr(n.exprs[-1])

    def visit_Enum(self, n: c_ast.Enum) -> None:
        self._generate_struct_union_enum(n, name='enum')

    def visit_Alignas(self, n: c_ast.Alignas) -> None:
        self.output_stream.write('_Alignas(')
        self.visit(n.alignment)
        self.output_stream.write(')')

    def visit_Enumerator(self, n: c_ast.Enumerator) -> None:
        self._make_indent()
        self.output_stream.write(n.name)
        if n.value:
            self.output_stream.write(' = ')
            self.output_stream.write(n.value.value)
        self.output_stream.write(',\n')

    def visit_FuncDef(self, n: c_ast.FuncDef) -> None:
        self.visit(n.decl)
        self.output_stream.write('\n')

        for param in n.param_decls or []:
            self.visit(param)
            self.output_stream.write(';\n')

        self.indent_level = 0
        self.visit(n.body)
        self.output_stream.write('\n')

    def visit_FileAST(self, n: c_ast.FileAST) -> None:
        for ext in n.ext:
            if isinstance(ext, c_ast.FuncDef):
                self.visit(ext)
            elif isinstance(ext, c_ast.Pragma):
                self.visit(ext)
                self.output_stream.write('\n')
            else:
                self.visit(ext)
                self.output_stream.write(';\n')

    def visit_Compound(self, n: c_ast.Compound) -> None:
        self._make_indent()
        self.output_stream.write('{\n')
        self.indent_level += 1
        for stmt in n.block_items or []:
            self._generate_stmt(stmt)
        self.indent_level -= 1
        self._make_indent()
        self.output_stream.write('}\n')

    def visit_CompoundLiteral(self, n: c_ast.CompoundLiteral) -> None:
        self.output_stream.write('(')
        self.visit(n.type)
        self.output_stream.write('){')
        self.visit(n.init)
        self.output_stream.write('}')

    def visit_EmptyStatement(self, n: c_ast.EmptyStatement) -> None:
        self.output_stream.write(';')

    def visit_ParamList(self, n: c_ast.ParamList) -> None:
        for param in n.params[:-1]:
            self.visit(param)
            self.output_stream.write(', ')
        if len(n.params) != 0:
            self.visit(n.params[-1])

    def visit_Return(self, n: c_ast.Return) -> None:
        self.output_stream.write('return')
        if n.expr:
            self.output_stream.write(' ')
            self.visit(n.expr)
        self.output_stream.write(';')

    def visit_Break(self, n: c_ast.Break) -> None:
        self.output_stream.write('break;')

    def visit_Continue(self, n: c_ast.Continue) -> None:
        self.output_stream.write('continue;')

    def visit_TernaryOp(self, n: c_ast.TernaryOp) -> None:
        self.output_stream.write('(')
        self._visit_expr(n.cond)
        self.output_stream.write(') ? (')
        self._visit_expr(n.iftrue)
        self.output_stream.write(') : (')
        self._visit_expr(n.iffalse)
        self.output_stream.write(')')

    def visit_If(self, n: c_ast.If) -> None:
        self.output_stream.write('if (')
        if n.cond:
            self.visit(n.cond)
        self.output_stream.write(')\n')
        self._generate_stmt(n.iftrue, add_indent=True)
        if n.iffalse:
            self._make_indent()
            self.output_stream.write('else\n')
            self._generate_stmt(n.iffalse, add_indent=True)

    def visit_For(self, n: c_ast.For) -> None:
        self.output_stream.write('for (')
        if n.init:
            self.visit(n.init)
        self.output_stream.write(';')
        if n.cond:
            self.output_stream.write(' ')
            self.visit(n.cond)
        self.output_stream.write(';')
        if n.next:
            self.output_stream.write(' ')
            self.visit(n.next)
        self.output_stream.write(')\n')
        self._generate_stmt(n.stmt, add_indent=True)

    def visit_While(self, n: c_ast.While) -> None:
        self.output_stream.write('while (')
        if n.cond:
            self.visit(n.cond)
        self.output_stream.write(')\n')
        self._generate_stmt(n.stmt, add_indent=True)

    def visit_DoWhile(self, n: c_ast.DoWhile) -> None:
        self.output_stream.write('do\n')
        self._generate_stmt(n.stmt, add_indent=True)
        self._make_indent()
        self.output_stream.write(' while (')
        if n.cond:
            self.visit(n.cond)
        self.output_stream.write(');')

    def visit_StaticAssert(self, n: c_ast.StaticAssert) -> None:
        self.output_stream.write('_Static_assert(')
        self.visit(n.cond)
        if n.message:
            self.output_stream.write(',')
            self.visit(n.message)
        self.output_stream.write(')')

    def visit_Switch(self, n: c_ast.Switch) -> None:
        self.output_stream.write('switch (')
        self.visit(n.cond) 
        self.output_stream.write(')\n')
        self._generate_stmt(n.stmt, add_indent=True) 

    def visit_Case(self, n: c_ast.Case) -> None:
        self.output_stream.write('case ')
        self.visit(n.expr)
        self.output_stream.write(':\n')
        for stmt in n.stmts:
            self._generate_stmt(stmt, add_indent=True)

    def visit_Default(self, n: c_ast.Default) -> None:
        self.output_stream.write ('default:\n')
        for stmt in n.stmts:
            self._generate_stmt(stmt, add_indent=True)

    def visit_Label(self, n: c_ast.Label) -> None:
        self.output_stream.write(n.name)
        self.output_stream.write(':\n')
        self._generate_stmt(n.stmt)

    def visit_Goto(self, n: c_ast.Goto) -> None:
        self.output_stream.write('goto ')
        self.output_stream.write(n.name)
        self.output_stream.write(';')

    def visit_EllipsisParam(self, n: c_ast.EllipsisParam) -> None:
        self.output_stream.write('...')

    def visit_Struct(self, n: c_ast.Struct) -> None:
        self._generate_struct_union_enum(n, 'struct')

    def visit_Typename(self, n: c_ast.Typename) -> None:
        return self._generate_type(n.type)

    def visit_Union(self, n: c_ast.Union) -> None:
        return self._generate_struct_union_enum(n, 'union')

    def visit_NamedInitializer(self, n: c_ast.NamedInitializer) -> None:
        for name in n.name:
            if isinstance(name, c_ast.ID):
                self.output_stream.write('.')
                self.output_stream.write(name.name)
            else:
                self.output_stream.write('[')
                self.visit(name)
                self.output_stream.write(']')
        self.output_stream.write(' = ')
        self._visit_expr(n.expr)

    def visit_FuncDecl(self, n: c_ast.FuncDecl) -> None:
        return self._generate_type(n)

    def visit_ArrayDecl(self, n: c_ast.ArrayDecl) -> None:
        return self._generate_type(n, emit_declname=False)

    def visit_TypeDecl(self, n: c_ast.TypeDecl) -> None:
        return self._generate_type(n, emit_declname=False)

    def visit_PtrDecl(self, n: c_ast.PtrDecl) -> None:
        return self._generate_type(n, emit_declname=False)

    def _generate_struct_union_enum(self, n, name: Literal["struct"] | Literal["union"] | Literal["enum"]) -> None:
        """ Generates code for structs, unions, and enums. name should be
            'struct', 'union', or 'enum'.
        """

        # None means no members. Empty sequence means an empty list of members
        members: list | None

        if name == 'enum':
            members = None if n.values is None else n.values.enumerators
            _generate_body = self._generate_enum_body
        else:
            members = n.decls
            _generate_body = self._generate_struct_union_body

        self.output_stream.write(name)
        self.output_stream.write(' ')
        if n.name:
            self.output_stream.write(n.name)

        if members is not None:
            self.output_stream.write('\n')
            self._make_indent()
            self.indent_level += 1
            self.output_stream.write('{\n')
            _generate_body(members)
            self.indent_level -= 1
            self._make_indent()
            self.output_stream.write('}')

    def _generate_struct_union_body(self, members: Iterable) -> None:
        for decl in members:
            self._generate_stmt(decl)

    def _generate_enum_body(self, members: Iterable) -> None:
        # `[:-2] + '\n'` removes the final `,` from the enumerator list
        for value in members:
            self.visit(value) 
        self.output_stream.write('\n')

    def _generate_stmt(self, n: c_ast.Node, add_indent: bool = False) -> None:
        """ Generation from a statement node. This method exists as a wrapper
            for individual visit_* methods to handle different treatment of
            some statements in this context.
        """
        typ = type(n)
        if isinstance(n, c_ast.Compound):
            # Here we return before indenting (Compound makes its own indents)
            self.visit(n)
            return

        if add_indent:
            self.indent_level += 1
        self._make_indent()
        if add_indent:
            self.indent_level -= 1
        self.visit(n)

        if isinstance(n, c_ast.If):
            return

        _expr_stmt_types = (c_ast.Decl, c_ast.Assignment, c_ast.Cast,
                            c_ast.UnaryOp, c_ast.BinaryOp, c_ast.TernaryOp,
                            c_ast.FuncCall, c_ast.ArrayRef, c_ast.StructRef,
                            c_ast.Constant, c_ast.ID, c_ast.Typedef,
                            c_ast.ExprList)
        if isinstance(n, _expr_stmt_types):
            self.output_stream.write(';\n')
            return

        self.output_stream.write('\n')

    def _generate_decl(self, n: c_ast.Decl) -> None:
        for spec in n.funcspec:
            self.output_stream.write(f'{spec} ')
        for storage in n.storage:
            self.output_stream.write(f'{storage} ')
        if n.align:
            self.visit(n.align[0])
            self.output_stream.write(' ')
        self._generate_type(n.type)
    
    def _generate_typedecl(self, n: c_ast.TypeDecl, modifiers: list | None, emit_declname: bool) -> None:
        for type_qualifier in n.quals:
            self.output_stream.write(f'{type_qualifier} ')
        self.visit(n.type)

        nstr = n.declname if n.declname and emit_declname else ''
        # Here, we resolve the modifiers, wrapping in parens to distinguish
        # pointer to array and pointer to function syntax.
        modifiers = modifiers or []
        for i, modifier in enumerate(modifiers):

            if isinstance(modifier, c_ast.ArrayDecl):
                if (i != 0 and isinstance(modifiers[i - 1], c_ast.PtrDecl)):
                    nstr = '(' + nstr + ')'
                nstr += '['
                if modifier.dim_quals:
                    nstr += ' '.join(map(str, modifier.dim_quals)) + ' '
                nstr += self._node_as_str(modifier.dim) + ']'
                continue

            if isinstance(modifier, c_ast.FuncDecl):
                if (i != 0 and isinstance(modifiers[i - 1], c_ast.PtrDecl)):
                    nstr = '(' + nstr + ')'
                nstr += '(' + self._node_as_str(modifier.args) + ')'
                continue

            if isinstance(modifier, c_ast.PtrDecl):
                if modifier.quals:
                    nstr = '* %s%s' % (' '.join(map(str, modifier.quals)), ' ' + nstr if nstr else '')
                else:
                    nstr = '*' + nstr
                continue

        if nstr:
            self.output_stream.write(' ')
            self.output_stream.write(nstr)

    def _generate_type(self, n, modifiers: list | None=None, emit_declname:bool=True) -> None:
        """ Recursive generation from a type node. n is the type node.
            modifiers collects the PtrDecl, ArrayDecl and FuncDecl modifiers
            encountered on the way down to a TypeDecl, to allow proper
            generation from it.
        """
        typ = type(n)

        if typ == c_ast.TypeDecl:
            self._generate_typedecl(n, modifiers=modifiers, emit_declname=emit_declname)
        elif typ == c_ast.Decl:
            self._generate_decl(n.type)
        elif typ == c_ast.Typename:
            self._generate_type(n.type, emit_declname=emit_declname)
        elif typ == c_ast.IdentifierType:
            for name in n.names:
                self.output_stream.write(f'{name} ')
        elif typ in (c_ast.ArrayDecl, c_ast.PtrDecl, c_ast.FuncDecl):
            modifiers = [n] if modifiers is None else modifiers + [n]
            self._generate_type(n.type, modifiers, emit_declname=emit_declname)
        else:
            self.visit(n)

    def _parenthesize_if[N: c_ast.Node](self, n: N, condition: Predicate[N]) -> None:
        if not condition(n):
            self._visit_expr(n)
            return
        self.output_stream.write('(')
        self._visit_expr(n)
        self.output_stream.write(')')

    def _parenthesize_unless_simple(self, n: c_ast.Node) -> None:
        """ Common use case for _parenthesize_if
        """
        self._parenthesize_if(n, lambda d: not self._is_simple_node(d))

    def _is_simple_node(self, n: c_ast.Node) -> bool:
        """ Returns True for nodes that are "simple" - i.e. nodes that always
            have higher precedence than operators.
        """
        return isinstance(n, (c_ast.Constant, c_ast.ID, c_ast.ArrayRef, c_ast.StructRef, c_ast.FuncCall))
