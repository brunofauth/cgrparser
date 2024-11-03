#------------------------------------------------------------------------------
# cgrparser: c_parser.py
#
# CParser class: Parser and AST builder for the C language
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#------------------------------------------------------------------------------
# yapf: disable

from __future__ import annotations

from .ply import yacc
from . import c_ast
from .c_lexer import CLexer
from .model import DeclSpecifiers, FunctionSpecifierKind, StorageSpecifierKind, TypeQualifierSpecifierKind, InitDeclarator, StructDeclarator, PtrNullness, PtrIntent, TypeSpecifier
from .plyparser import PLYParser, ParseError, parameterized, template
from .ast_transforms import fix_switch_cases, fix_atomic_specifiers

import typing

if typing.TYPE_CHECKING:
    from typing import Never, TypedDict
    from .plyparser import Coord
    type TypeDeclModifier = c_ast.ArrayDecl | c_ast.FuncDecl | c_ast.PtrDecl 


_PARSE_ERROR_HELP_NOTES: dict[str, str] = {
    "cgr_fallthru": "this keyword may only appear inside 'case' labels. Perhaps you misplaced it?",
    "cgr_nullable": "this keyword may only apper right after the star '*' of a pointer declaration. Perhaps you misplaced it?",
    "cgr_not_null": "this keyword may only apper right after the star '*' of a pointer declaration. Perhaps you misplaced it?",
    "cgr_in": "this keyword may only apper inside a function signature, right before the star '*' of a pointer declaration. Perhaps you misplaced it?",
    "cgr_out": "this keyword may only apper inside a function signature, right before the star '*' of a pointer declaration. Perhaps you misplaced it?",
    "cgr_inout": "this keyword may only apper inside a function signature, right before the star '*' of a pointer declaration. Perhaps you misplaced it?",
}


def _dummy_typedecl() -> c_ast.TypeDecl:
    return c_ast.TypeDecl(None, None, None, None)


@template
class CParser(PLYParser):

    def __init__(
        self,
        lex_optimize: bool = True,
        lexer: type[CLexer] = CLexer,
        lextab: str = 'cgrparser.lextab',
        yacc_optimize: bool = True,
        yacctab: str = 'cgrparser.yacctab',
        yacc_debug: bool = False,
        taboutputdir: str = '',
    ) -> None:
        """ Some arguments for controlling the debug/optimization
            level of the parser are provided. The defaults are
            tuned for release/performance mode.
            The simple rules for using them are:
            *) When tweaking CParser/CLexer, set these to False
            *) When releasing a stable parser, set to True

            lex_optimize:
                Set to False when you're modifying the lexer.
                Otherwise, changes in the lexer won't be used, if
                some lextab.py file exists.
                When releasing with a stable lexer, set to True
                to save the re-generation of the lexer table on
                each run.

            lexer:
                Set this parameter to define the lexer to use if
                you're not using the default CLexer.

            lextab:
                Points to the lex table that's used for optimized
                mode. Only if you're modifying the lexer and want
                some tests to avoid re-generating the table, make
                this point to a local lex table file (that's been
                earlier generated with lex_optimize=True)

            yacc_optimize:
                Set to False when you're modifying the parser.
                Otherwise, changes in the parser won't be used, if
                some parsetab.py file exists.
                When releasing with a stable parser, set to True
                to save the re-generation of the parser table on
                each run.

            yacctab:
                Points to the yacc table that's used for optimized
                mode. Only if you're modifying the parser, make
                this point to a local yacc table file

            yacc_debug:
                Generate a parser.out file that explains how yacc
                built the parsing table from the grammar.

            taboutputdir:
                Set this parameter to control the location of generated
                lextab and yacctab files.
        """
        self.clex = lexer(
            error_func=self._lex_error_func,
            on_lbrace_func=self._lex_on_lbrace_func,
            on_rbrace_func=self._lex_on_rbrace_func,
            type_lookup_func=self._lex_type_lookup_func,
        )

        self.clex.build(optimize=lex_optimize, lextab=lextab, outputdir=taboutputdir)
        self.tokens = self.clex.tokens

        rules_with_opt = [
            'arg_abstract_declarator',
            'abstract_declarator',
            'assignment_expression',
            'declaration_list',
            'declaration_specifiers_no_type',
            'designation',
            'expression',
            'identifier_list',
            'init_declarator_list',
            'id_init_declarator_list',
            'initializer_list',
            'parameter_type_list',
            'block_item_list',
            'type_qualifier_list',
            'struct_declarator_list',
            'nullness_qualifier',
        ]

        for rule in rules_with_opt:
            self._create_opt_rule(rule)

        self.cparser = yacc.yacc(
            module=self,
            start='translation_unit_or_empty',
            debug=yacc_debug,
            optimize=yacc_optimize,
            tabmodule=yacctab,
            outputdir=taboutputdir,
        )

        # Stack of scopes for keeping track of symbols. _scope_stack[-1] is
        # the current (topmost) scope. Each scope is a dictionary that
        # specifies whether a name is a type. If _scope_stack[n][name] is
        # True, 'name' is currently a type in the scope. If it's False,
        # 'name' is used in the scope but not as a type (for instance, if we
        # saw: int name;
        # If 'name' is not a key in _scope_stack[n] then 'name' was not defined
        # in this scope at all.
        self._scope_stack: list[dict[str, bool]] = [{}]

        # Keeps track of the last token given to yacc (the lookahead token)
        self._last_yielded_token = None


    def parse(self, text: str, filename: str = '', debug: bool = False):
        """ Parses C code and returns an AST.

            text:
                A string containing the C source code

            filename:
                Name of the file being parsed (for meaningful
                error messages)

            debug:
                Debug flag to YACC
        """
        self.clex.filename = filename
        self.clex.reset_lineno()
        self._scope_stack = [{}]
        self._last_yielded_token = None
        return self.cparser.parse(input=text, lexer=self.clex, debug=debug)


    ######################--   PRIVATE   --######################

    def _push_scope(self) -> None:
        self._scope_stack.append({})

    def _pop_scope(self) -> None:
        assert len(self._scope_stack) > 1
        self._scope_stack.pop()

    def _add_typedef_name(self, name: str, coord: Coord) -> None:
        """ Add a new typedef name (ie a TYPEID) to the current scope
        """
        if not self._scope_stack[-1].get(name, True):
            self._parse_error("Typedef %r previously declared as non-typedef "
                                "in this scope" % name, coord)
        self._scope_stack[-1][name] = True

    def _add_identifier(self, name: str, coord: Coord) -> None:
        """ Add a new object, function, or enum member name (ie an ID) to the
            current scope
        """
        if self._scope_stack[-1].get(name, False):
            self._parse_error("Non-typedef %r previously declared as typedef "
                                "in this scope" % name, coord)
        self._scope_stack[-1][name] = False

    def _is_type_in_scope(self, name: str) -> bool:
        """ Is *name* a typedef-name in the current scope?
        """
        for scope in reversed(self._scope_stack):
            # If name is an identifier in this scope it shadows typedefs in
            # higher scopes.
            in_scope = scope.get(name)
            if in_scope is not None:
                return in_scope
        return False

    def _lex_error_func(self, msg: str, line, column) -> Never:
        self._parse_error(msg, self._coord(line, column))

    def _lex_on_lbrace_func(self) -> None:
        self._push_scope()

    def _lex_on_rbrace_func(self) -> None:
        self._pop_scope()

    def _lex_type_lookup_func(self, name: str) -> bool:
        """ Looks up types that were previously defined with
            typedef.
            Passed to the lexer for recognizing identifiers that
            are types.
        """
        return self._is_type_in_scope(name)

    def _get_yacc_lookahead_token(self):
        """ We need access to yacc's lookahead token in certain cases.
            This is the last token yacc requested from the lexer, so we
            ask the lexer.
        """
        return self.clex.last_token

    # To understand what's going on here, read sections A.8.5 and
    # A.8.6 of K&R2 very carefully.
    #
    # A C type consists of a basic type declaration, with a list
    # of modifiers. For example:
    #
    # int *c[5];
    #
    # The basic declaration here is 'int c', and the pointer and
    # the array are the modifiers.
    #
    # Basic declarations are represented by TypeDecl (from module c_ast) and the
    # modifiers are FuncDecl, PtrDecl and ArrayDecl.
    #
    # The standard states that whenever a new modifier is parsed, it should be
    # added to the end of the list of modifiers. For example:
    #
    # K&R2 A.8.6.2: Array Declarators
    #
    # In a declaration T D where D has the form
    #   D1 [constant-expression-opt]
    # and the type of the identifier in the declaration T D1 is
    # "type-modifier T", the type of the
    # identifier of D is "type-modifier array of T"
    #
    # This is what this method does. The declarator it receives
    # can be a list of declarators ending with TypeDecl. It
    # tacks the modifier to the end of this list, just before
    # the TypeDecl.
    #
    # Additionally, the modifier may be a list itself. This is
    # useful for pointers, that can come as a chain from the rule
    # p_pointer. In this case, the whole modifier list is spliced
    # into the new location.
    #
    # Note to Self: I guess, when the comment above talks about lists, it does
    # not mean actual python 'list's, but linked lists, which use a node's
    # 'type' field as the pointer to the next node.
    def _type_modify_decl(
        self,
        decl,
        modifier,
    ):
        """ Tacks a type modifier on a declarator, and returns
            the modified declarator.

            Note: the declarator and modifier may be modified
        """
        modifier_head = modifier
        modifier_tail = modifier

        # The modifier may be a nested list. Reach its tail.
        while modifier_tail.type:
            modifier_tail = modifier_tail.type

        # If the decl is a basic type, just tack the modifier onto it.
        if isinstance(decl, c_ast.TypeDecl):
            modifier_tail.type = decl
            return modifier
        else:
            # Otherwise, the decl is a list of modifiers. Reach
            # its tail and splice the modifier onto the tail,
            # pointing to the underlying basic type.
            decl_tail = decl

            while not isinstance(decl_tail.type, c_ast.TypeDecl):
                decl_tail = decl_tail.type

            modifier_tail.type = decl_tail.type
            decl_tail.type = modifier_head
            return decl

    # Due to the order in which declarators are constructed,
    # they have to be fixed in order to look like a normal AST.
    #
    # When a declaration arrives from syntax construction, it has
    # these problems:
    # * The innermost TypeDecl has no type (because the basic
    #   type is only known at the uppermost declaration level)
    # * The declaration has no variable name, since that is saved
    #   in the innermost TypeDecl
    # * The typename of the declaration is a list of type
    #   specifiers, and not a node. Here, basic identifier types
    #   should be separated from more complex types like enums
    #   and structs.
    #
    # This method fixes these problems.
    def _fix_decl_name_type(self, decl, typename):
        """ Fixes a declaration. Modifies decl.
        """
        # Reach the underlying basic type
        #
        _type = decl
        while not isinstance(_type, c_ast.TypeDecl):
            _type = _type.type
        bottom_type: c_ast.TypeDecl = _type # type: ignore

        decl.name = bottom_type.declname
        bottom_type.quals = decl.quals

        # The typename is a list of types. If any type in this
        # list isn't an IdentifierType, it must be the only
        # type in the list (it's illegal to declare "int enum ..")
        # If all the types are basic, they're collected in the
        # IdentifierType holder.
        for tn in typename:
            if not isinstance(tn, c_ast.IdentifierType):
                if len(typename) > 1:
                    self._parse_error(f"Invalid multiple types specified\n{typename}", tn.coord)
                else:
                    bottom_type.type = tn
                    return decl

        if not typename:
            # Functions default to returning int
            #
            if not isinstance(decl.type, c_ast.FuncDecl):
                self._parse_error("Missing type in declaration", decl.coord)
            bottom_type.type = c_ast.IdentifierType([TypeSpecifier.INT], coord=decl.coord)
        else:
            # At this point, we know that typename is a list of IdentifierType
            # nodes. Concatenate all the names into a single list.
            #
            bottom_type.type = c_ast.IdentifierType(
                [name for id in typename for name in id.names],
                coord=typename[0].coord,
            )
        return decl

    def _build_declarations(
        self,
        decl_specifiers: DeclSpecifiers,
        decls: list[StructDeclarator | InitDeclarator],
        typedef_namespace: bool = False,
    ):
        """ Builds a list of declarations all sharing the given specifiers.
            If typedef_namespace is true, each declared name is added
            to the "typedef namespace", which also includes objects,
            functions, and enum constants.
        """
        is_typedef = StorageSpecifierKind.TYPEDEF in decl_specifiers.storage
        declarations = []

        # Bit-fields are allowed to be unnamed.
        if isinstance(decls[0], StructDeclarator) and decls[0].bitsize is not None:
            pass

        # When redeclaring typedef names as identifiers in inner scopes, a
        # problem can occur where the identifier gets grouped into
        # spec['type'], leaving decl as None.  This can only occur for the
        # first declarator.
        elif decls[0].decl is None:
            # Given the check above the if chain below, I guess an invariant
            # grants that decl_specifiers['type'][-1] is an IdentifierType when
            # decls[0]['decl'] is None
            _id = typing.cast(c_ast.IdentifierType, decl_specifiers.type[-1])
            if (   len(decl_specifiers.type) < 2
                or len(_id.names) != 1
                or not self._is_type_in_scope(_id.names[0])
            ):
                coord = None # Why is this used here? At what times will we not have a proper Coord?
                for t in decl_specifiers.type:
                    if hasattr(t, 'coord'): # can a type specifier ever have no 'coord'?
                        coord = t.coord
                        break # Can this loop exit without ever breaking?
                self._parse_error('Invalid declaration', coord) # type: ignore

            # Make this look as if it came from "direct_declarator:ID"
            decls[0].decl = c_ast.TypeDecl(
                declname=_id.names[0],
                type=None,
                quals=TypeQualifierSpecifierKind.EMPTY,
                align=decl_specifiers.alignment,
                coord=_id.coord,
            )
            # Remove the "new" type's name from the end of spec['type']
            del decl_specifiers.type[-1]

        # A similar problem can occur where the declaration ends up looking
        # like an abstract declarator. Give it a name if this is the case.
        elif not isinstance(
            decls[0].decl,
            (c_ast.Enum, c_ast.Struct, c_ast.Union, c_ast.IdentifierType)
        ):
            _decls_0_tail: Declarator = decls[0].decl
            while not isinstance(_decls_0_tail, c_ast.TypeDecl):
                _decls_0_tail = _decls_0_tail.type
            decls_0_tail: c_ast.TypeDecl = _decls_0_tail

            if decls_0_tail.declname is None:
                # The line below makes me assume decl_specifiers.type[-1] must be IdentifierType
                _id = typing.cast(c_ast.IdentifierType, decl_specifiers.type[-1])
                decls_0_tail.declname = _id.names[0]
                del decl_specifiers.type[-1]

        for decl in decls:
            assert decl.decl is not None

            declaration: c_ast.Typedef | c_ast.Decl
            if is_typedef:
                declaration = c_ast.Typedef(
                    name=None,
                    quals=decl_specifiers.qualifiers,
                    storage=decl_specifiers.storage,
                    type=decl.decl,
                    coord=decl.decl.coord,
                )
            elif isinstance(decl, StructDeclarator):
                declaration = c_ast.Decl(
                    name=None,
                    quals=decl_specifiers.qualifiers,
                    align=decl_specifiers.alignment,
                    storage=decl_specifiers.storage,
                    funcspec=decl_specifiers.function,
                    type=decl.decl,
                    init=None,
                    bitsize=decl.bitsize,
                    coord=decl.decl.coord,
                )
            elif isinstance(decl, InitDeclarator):
                declaration = c_ast.Decl(
                    name=None,
                    quals=decl_specifiers.qualifiers,
                    align=decl_specifiers.alignment,
                    storage=decl_specifiers.storage,
                    funcspec=decl_specifiers.function,
                    type=decl.decl,
                    init=decl.init,
                    bitsize=None,
                    coord=decl.decl.coord,
                )
            else:
                raise TypeError(f'"decl" is of type {type(decl)!r}')

            if isinstance(
                declaration.type,
                (c_ast.Enum, c_ast.Struct, c_ast.Union, c_ast.IdentifierType),
            ):
                fixed_decl = declaration
            else:
                fixed_decl = self._fix_decl_name_type(decl=declaration, typename=decl_specifiers.type)

            # Add the type name defined by typedef to a
            # symbol table (for usage in the lexer)
            if typedef_namespace:
                if is_typedef:
                    self._add_typedef_name(fixed_decl.name, fixed_decl.coord)
                else:
                    self._add_identifier(fixed_decl.name, fixed_decl.coord)

            fixed_decl = fix_atomic_specifiers(fixed_decl)
            declarations.append(fixed_decl)

        return declarations

    def _build_function_definition(
        self,
        spec: DeclSpecifiers,
        decl: c_ast.TypeDecl | TypeDeclModifier,
        param_decls: list[c_ast.Decl | c_ast.Typedef],
        body: c_ast.Compound,
    ) -> c_ast.FuncDef:
        if StorageSpecifierKind.TYPEDEF in spec.storage:
            self._parse_error("Invalid typedef", decl.coord)

        declaration: list[c_ast.Decl] = self._build_declarations(
            decl_specifiers=spec,
            decls=[InitDeclarator(decl=decl)],
            typedef_namespace=True,
        )[0]

        return c_ast.FuncDef(decl=declaration, param_decls=param_decls, body=body, coord=decl.coord)

    def _select_struct_union_class(self, token):
        return c_ast.Struct if token == 'struct' else c_ast.Union

    ##
    ## Precedence and associativity of operators
    ##
    # If this changes, c_generator.CGenerator.precedence_map needs to change as
    # well
    precedence = (
        ('left', 'LOR'),
        ('left', 'LAND'),
        ('left', 'OR'),
        ('left', 'XOR'),
        ('left', 'AND'),
        ('left', 'EQ', 'NE'),
        ('left', 'GT', 'GE', 'LT', 'LE'),
        ('left', 'RSHIFT', 'LSHIFT'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE', 'MOD'),
    )

    ##
    ## Grammar productions
    ## Implementation of the BNF defined in K&R2 A.13
    ##

    # Wrapper around a translation unit, to allow for empty input.
    # Not strictly part of the C99 Grammar, but useful in practice.
    def p_translation_unit_or_empty(self, p):
        """ translation_unit_or_empty   : translation_unit
                                        | empty
        """
        if p[1] is None:
            p[0] = c_ast.FileAST([])
        else:
            p[0] = c_ast.FileAST(p[1])

    def p_translation_unit_1(self, p):
        """ translation_unit    : external_declaration
        """
        # Note: external_declaration is already a list
        p[0] = p[1]

    def p_translation_unit_2(self, p):
        """ translation_unit    : translation_unit external_declaration
        """
        p[1].extend(p[2])
        p[0] = p[1]

    # Declarations always come as lists (because they can be
    # several in one line), so we wrap the function definition
    # into a list as well, to make the return value of
    # external_declaration homogeneous.
    def p_external_declaration_1(self, p):
        """ external_declaration    : function_definition
        """
        p[0] = [p[1]]

    def p_external_declaration_2(self, p):
        """ external_declaration    : declaration
        """
        p[0] = p[1]

    def p_external_declaration_3(self, p):
        """ external_declaration    : pp_directive
                                    | pppragma_directive
        """
        p[0] = [p[1]]

    def p_external_declaration_4(self, p):
        """ external_declaration    : SEMI
        """
        p[0] = []

    def p_external_declaration_5(self, p):
        """ external_declaration    : static_assert
        """
        p[0] = p[1]

    def p_static_assert_declaration(self, p):
        """ static_assert           : _STATIC_ASSERT LPAREN constant_expression COMMA unified_string_literal RPAREN
                                    | _STATIC_ASSERT LPAREN constant_expression RPAREN
        """
        if len(p) == 5:
            p[0] = [c_ast.StaticAssert(p[3], None, self._token_coord(p, 1))]
        else:
            p[0] = [c_ast.StaticAssert(p[3], p[5], self._token_coord(p, 1))]

    def p_pp_directive(self, p):
        """ pp_directive  : PPHASH
        """
        self._parse_error('Directives not supported yet', self._token_coord(p, 1))

    # This encompasses two types of C99-compatible pragmas:
    # - The #pragma directive:
    #       # pragma character_sequence
    # - The _Pragma unary operator:
    #       _Pragma ( " string_literal " )
    def p_pppragma_directive(self, p):
        """ pppragma_directive      : PPPRAGMA
                                    | PPPRAGMA PPPRAGMASTR
                                    | _PRAGMA LPAREN unified_string_literal RPAREN
        """
        if len(p) == 5:
            p[0] = c_ast.Pragma(p[3], self._token_coord(p, 2))
        elif len(p) == 3:
            p[0] = c_ast.Pragma(p[2], self._token_coord(p, 2))
        else:
            p[0] = c_ast.Pragma("", self._token_coord(p, 1))

    def p_pppragma_directive_list(self, p):
        """ pppragma_directive_list : pppragma_directive
                                    | pppragma_directive_list pppragma_directive
        """
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]

    # In function definitions, the declarator can be followed by
    # a declaration list, for old "K&R style" function definitios.
    def p_function_definition_1(self, p):
        """ function_definition : id_declarator declaration_list_opt compound_statement
        """
        # no declaration specifiers - 'int' becomes the default type
        spec = DeclSpecifiers(type=[
            c_ast.IdentifierType([TypeSpecifier.INT], coord=self._token_coord(p, 1))
        ])

        p[0] = self._build_function_definition(spec=spec, decl=p[1], param_decls=p[2], body=p[3])

    def p_function_definition_2(self, p):
        """ function_definition : declaration_specifiers id_declarator declaration_list_opt compound_statement
        """
        spec = p[1]

        p[0] = self._build_function_definition(spec=spec, decl=p[2], param_decls=p[3], body=p[4])

    # Note, according to C18 A.2.2 6.7.10 static_assert-declaration _Static_assert
    # is a declaration, not a statement. We additionally recognise it as a statement
    # to fix parsing of _Static_assert inside the functions.
    #
    def p_statement(self, p):
        """ statement   : labeled_statement
                        | expression_statement
                        | compound_statement
                        | selection_statement
                        | iteration_statement
                        | jump_statement
                        | pppragma_directive
                        | static_assert
        """
        p[0] = p[1]

    # A pragma is generally considered a decorator rather than an actual
    # statement. Still, for the purposes of analyzing an abstract syntax tree of
    # C code, pragma's should not be ignored and were previously treated as a
    # statement. This presents a problem for constructs that take a statement
    # such as labeled_statements, selection_statements, and
    # iteration_statements, causing a misleading structure in the AST. For
    # example, consider the following C code.
    #
    #   for (int i = 0; i < 3; i++)
    #       #pragma omp critical
    #       sum += 1;
    #
    # This code will compile and execute "sum += 1;" as the body of the for
    # loop. Previous implementations of cgrparser would render the AST for this
    # block of code as follows:
    #
    #   For:
    #     DeclList:
    #       Decl: i, [], [], []
    #         TypeDecl: i, []
    #           IdentifierType: ['int']
    #         Constant: int, 0
    #     BinaryOp: <
    #       ID: i
    #       Constant: int, 3
    #     UnaryOp: p++
    #       ID: i
    #     Pragma: omp critical
    #   Assignment: +=
    #     ID: sum
    #     Constant: int, 1
    #
    # This AST misleadingly takes the Pragma as the body of the loop and the
    # assignment then becomes a sibling of the loop.
    #
    # To solve edge cases like these, the pragmacomp_or_statement rule groups
    # a pragma and its following statement (which would otherwise be orphaned)
    # using a compound block, effectively turning the above code into:
    #
    #   for (int i = 0; i < 3; i++) {
    #       #pragma omp critical
    #       sum += 1;
    #   }
    def p_pragmacomp_or_statement_1(self, p):
        """ pragmacomp_or_statement : pppragma_directive_list statement """
        p[0] = c_ast.Compound(block_items=p[1] + [p[2]], coord=self._token_coord(p, 1))
    def p_pragmacomp_or_statement_2(self, p):
        """ pragmacomp_or_statement : statement """
        p[0] = p[1]

    def p_fallthru_statement(self, p):
        """ fallthru_statement : CGR_FALLTHRU SEMI """
        p[0] = c_ast.CgrFallthrough(self._token_coord(p, 1))

    def p_statement_case_1(self, p):
        """ statement_case : statement fallthru_statement """
        p[0] = p[1] + [p2[2]]

    def p_statement_case_2(self, p):
        """ statement_case : fallthru_statement """
        p[0] = p[1]

    def p_statement_case_3(self, p):
        """ statement_case : statement """
        p[0] = p[1]

    def p_pragmacomp_or_statement_case_1(self, p):
        """ pragmacomp_or_statement_case     : pppragma_directive_list statement_case """
        p[0] = c_ast.Compound(block_items=p[1] + [p[2]], coord=self._token_coord(p, 1))

    def p_pragmacomp_or_statement_case_2(self, p):
        """ pragmacomp_or_statement_case     : statement_case """
        p[0] = p[1]

    # In C, declarations can come several in a line:
    #   int x, *px, romulo = 5;
    #
    # However, for the AST, we will split them to separate Decl
    # nodes.
    #
    # This rule splits its declarations and always returns a list
    # of Decl nodes, even if it's one element long.
    #
    def p_decl_body(self, p):
        """ decl_body : declaration_specifiers init_declarator_list_opt
                      | declaration_specifiers_no_type id_init_declarator_list_opt
        """
        declspecs = p[1]

        # p[2] (init_declarator_list_opt) is either a list or None
        if p[2] is not None:
            decls = self._build_declarations(
                decl_specifiers=declspecs,
                decls=p[2],
                typedef_namespace=True,
            )
            p[0] = decls
            return

        # By the standard, you must have at least one declarator unless
        # declaring a structure tag, a union tag, or the members of an
        # enumeration.
        ty = declspecs.type
        s_u_or_e = (c_ast.Struct, c_ast.Union, c_ast.Enum)
        if len(ty) == 1 and isinstance(ty[0], s_u_or_e):
            decls = [c_ast.Decl(
                name=None,
                quals=declspecs.qualifiers,
                align=declspecs.alignment,
                storage=declspecs.storage,
                funcspec=declspecs.function,
                type=ty[0],
                init=None,
                bitsize=None,
                coord=ty[0].coord,
            )]

        # However, this case can also occur on redeclared identifiers in
        # an inner scope.  The trouble is that the redeclared type's name
        # gets grouped into declaration_specifiers; _build_declarations
        # compensates for this.
        else:
            decls = self._build_declarations(
                decl_specifiers=declspecs,
                decls=[InitDeclarator()],
                typedef_namespace=True,
            )

        p[0] = decls

    # The declaration has been split to a decl_body sub-rule and
    # SEMI, because having them in a single rule created a problem
    # for defining typedefs.
    #
    # If a typedef line was directly followed by a line using the
    # type defined with the typedef, the type would not be
    # recognized. This is because to reduce the declaration rule,
    # the parser's lookahead asked for the token after SEMI, which
    # was the type from the next line, and the lexer had no chance
    # to see the updated type symbol table.
    #
    # Splitting solves this problem, because after seeing SEMI,
    # the parser reduces decl_body, which actually adds the new
    # type into the table to be seen by the lexer before the next
    # line is reached.
    def p_declaration(self, p):
        """ declaration : decl_body SEMI
        """
        p[0] = p[1]

    # Since each declaration is a list of declarations, this
    # rule will combine all the declarations and return a single
    # list
    #
    def p_declaration_list(self, p):
        """ declaration_list    : declaration
                                | declaration_list declaration
        """
        p[0] = p[1] if len(p) == 2 else p[1] + p[2]

    # To know when declaration-specifiers end and declarators begin,
    # we require declaration-specifiers to have at least one
    # type-specifier, and disallow typedef-names after we've seen any
    # type-specifier. These are both required by the spec.
    #
    def p_declaration_specifiers_no_type_1(self, p):
        """ declaration_specifiers_no_type  : type_qualifier declaration_specifiers_no_type_opt
        """
        declspecs = p[2] or DeclSpecifiers()
        declspecs.qualifiers |= p[1]
        p[0] = declspecs

    def p_declaration_specifiers_no_type_2(self, p):
        """ declaration_specifiers_no_type  : storage_class_specifier declaration_specifiers_no_type_opt
        """
        declspecs = p[2] or DeclSpecifiers()
        declspecs.storage |= p[1]
        p[0] = declspecs

    def p_declaration_specifiers_no_type_3(self, p):
        """ declaration_specifiers_no_type  : function_specifier declaration_specifiers_no_type_opt """
        declspecs = p[2] or DeclSpecifiers()
        declspecs.function |= p[1]
        p[0] = declspecs

    # Without this, `typedef _Atomic(T) U` will parse incorrectly because the
    # _Atomic qualifier will match, instead of the specifier.
    def p_declaration_specifiers_no_type_4(self, p):
        """ declaration_specifiers_no_type  : atomic_specifier declaration_specifiers_no_type_opt """
        declspecs = p[2] or DeclSpecifiers()
        declspecs.type.insert(0, p[1])
        p[0] = declspecs


    def p_declaration_specifiers_no_type_5(self, p):
        """ declaration_specifiers_no_type  : alignment_specifier declaration_specifiers_no_type_opt """
        declspecs = p[2] or DeclSpecifiers()
        declspecs.alignment.append(p[1])
        p[0] = declspecs

    def p_declaration_specifiers_1(self, p):
        """ declaration_specifiers  : declaration_specifiers type_qualifier """
        p[1].qualifiers |= p[2]
        p[0] = p[1]

    def p_declaration_specifiers_2(self, p):
        """ declaration_specifiers  : declaration_specifiers storage_class_specifier """
        p[1].storage |= p[2]
        p[0] = p[1]

    def p_declaration_specifiers_3(self, p):
        """ declaration_specifiers  : declaration_specifiers function_specifier """
        p[1].function |= p[2]
        p[0] = p[1]

    def p_declaration_specifiers_4(self, p):
        """ declaration_specifiers  : declaration_specifiers type_specifier_no_typeid """
        p[1].type.append(p[2])
        p[0] = p[1]

    def p_declaration_specifiers_5(self, p):
        """ declaration_specifiers  : type_specifier """
        p[0] = DeclSpecifiers(type=[p[1]])

    def p_declaration_specifiers_6(self, p):
        """ declaration_specifiers  : declaration_specifiers_no_type type_specifier
        """
        p[1].type.append(p[2])
        p[0] = p[1]

    def p_declaration_specifiers_7(self, p):
        """ declaration_specifiers  : declaration_specifiers alignment_specifier
        """
        p[1].alignment.append(p[2])
        p[0] = p[1]

    def p_storage_class_specifier_1(self, p):
        """ storage_class_specifier : AUTO """
        p[0] = StorageSpecifierKind.AUTO
    def p_storage_class_specifier_2(self, p):
        """ storage_class_specifier : REGISTER """
        p[0] = StorageSpecifierKind.REGISTER
    def p_storage_class_specifier_3(self, p):
        """ storage_class_specifier : STATIC """
        p[0] = StorageSpecifierKind.STATIC
    def p_storage_class_specifier_4(self, p):
        """ storage_class_specifier : EXTERN """
        p[0] = StorageSpecifierKind.EXTERN
    def p_storage_class_specifier_5(self, p):
        """ storage_class_specifier : TYPEDEF """
        p[0] = StorageSpecifierKind.TYPEDEF
    def p_storage_class_specifier_6(self, p):
        """ storage_class_specifier : _THREAD_LOCAL """
        p[0] = StorageSpecifierKind.THREAD_LOCAL

    def p_function_specifier_1(self, p):
        """ function_specifier  : INLINE """
        p[0] = FunctionSpecifierKind.INLINE
    def p_function_specifier_2(self, p):
        """ function_specifier  : _NORETURN """
        p[0] = FunctionSpecifierKind.NORETURN

    def p_type_specifier_no_typeid_1(self, p):
        """ type_specifier_no_typeid  : VOID     """
        p[0] = c_ast.IdentifierType([TypeSpecifier.VOID], coord=self._token_coord(p, 1))
    def p_type_specifier_no_typeid_2(self, p):
        """ type_specifier_no_typeid  : _BOOL    """
        p[0] = c_ast.IdentifierType([TypeSpecifier.BOOL], coord=self._token_coord(p, 1))
    def p_type_specifier_no_typeid_3(self, p):
        """ type_specifier_no_typeid  : CHAR     """
        p[0] = c_ast.IdentifierType([TypeSpecifier.CHAR], coord=self._token_coord(p, 1))
    def p_type_specifier_no_typeid_4(self, p):
        """ type_specifier_no_typeid  : SHORT    """
        p[0] = c_ast.IdentifierType([TypeSpecifier.SHORT], coord=self._token_coord(p, 1))
    def p_type_specifier_no_typeid_5(self, p):
        """ type_specifier_no_typeid  : INT      """
        p[0] = c_ast.IdentifierType([TypeSpecifier.INT], coord=self._token_coord(p, 1))
    def p_type_specifier_no_typeid_6(self, p):
        """ type_specifier_no_typeid  : LONG     """
        p[0] = c_ast.IdentifierType([TypeSpecifier.LONG], coord=self._token_coord(p, 1))
    def p_type_specifier_no_typeid_7(self, p):
        """ type_specifier_no_typeid  : FLOAT    """
        p[0] = c_ast.IdentifierType([TypeSpecifier.FLOAT], coord=self._token_coord(p, 1))
    def p_type_specifier_no_typeid_8(self, p):
        """ type_specifier_no_typeid  : DOUBLE   """
        p[0] = c_ast.IdentifierType([TypeSpecifier.DOUBLE], coord=self._token_coord(p, 1))
    def p_type_specifier_no_typeid_9(self, p):
        """ type_specifier_no_typeid  : _COMPLEX """
        p[0] = c_ast.IdentifierType([TypeSpecifier.COMPLEX], coord=self._token_coord(p, 1))
    def p_type_specifier_no_typeid_10(self, p):
        """ type_specifier_no_typeid  : SIGNED   """
        p[0] = c_ast.IdentifierType([TypeSpecifier.SIGNED], coord=self._token_coord(p, 1))
    def p_type_specifier_no_typeid_11(self, p):
        """ type_specifier_no_typeid  : UNSIGNED """
        p[0] = c_ast.IdentifierType([TypeSpecifier.UNSIGNED], coord=self._token_coord(p, 1))
    def p_type_specifier_no_typeid_12(self, p):
        """ type_specifier_no_typeid  : __INT128 """
        p[0] = c_ast.IdentifierType([TypeSpecifier.INT128], coord=self._token_coord(p, 1))

    def p_type_specifier(self, p):
        """ type_specifier  : typedef_name
                            | enum_specifier
                            | struct_or_union_specifier
                            | type_specifier_no_typeid
                            | atomic_specifier
        """
        p[0] = p[1]

    # See section 6.7.2.4 of the C11 standard.
    def p_atomic_specifier(self, p):
        """ atomic_specifier  : _ATOMIC LPAREN type_name RPAREN
        """
        p[3].quals |= TypeQualifierSpecifierKind.ATOMIC
        p[0] = p[3]

    # NOTE: does this mean that this parser allows for 'restrict' on nonpointers?
    def p_type_qualifier_1(self, p):
        """ type_qualifier : CONST """
        p[0] = TypeQualifierSpecifierKind.CONST
    def p_type_qualifier_2(self, p):
        """ type_qualifier : RESTRICT """
        p[0] = TypeQualifierSpecifierKind.RESTRICT
    def p_type_qualifier_3(self, p):
        """ type_qualifier : VOLATILE """
        p[0] = TypeQualifierSpecifierKind.VOLATILE
    def p_type_qualifier_4(self, p):
        """ type_qualifier : _ATOMIC """
        p[0] = TypeQualifierSpecifierKind.ATOMIC

    def p_init_declarator_list_1(self, p):
        """ init_declarator_list : init_declarator """
        p[0] = [ p[1] ]
    def p_init_declarator_list_2(self, p):
        """ init_declarator_list : init_declarator_list COMMA init_declarator """
        p[0] = p[1] + [ p[3] ]

    def p_init_declarator_1(self, p):
        """ init_declarator : declarator """
        p[0] = InitDeclarator(decl=p[1])
    def p_init_declarator_2(self, p):
        """ init_declarator : declarator EQUALS initializer """
        p[0] = InitDeclarator(decl=p[1], init=p[3])

    def p_id_init_declarator_list_1(self, p):
        """ id_init_declarator_list : id_init_declarator """
        p[0] = [ p[1] ]
    def p_id_init_declarator_list_2(self, p):
        """ id_init_declarator_list : id_init_declarator_list COMMA init_declarator """
        p[0] = p[1] + [ p[3] ]

    def p_id_init_declarator_1(self, p):
        """ id_init_declarator : id_declarator """
        p[0] = InitDeclarator(decl=p[1])
    def p_id_init_declarator_2(self, p):
        """ id_init_declarator : id_declarator EQUALS initializer """
        p[0] = InitDeclarator(decl=p[1], init=p[3])

    # Require at least one type specifier in a specifier-qualifier-list
    #
    def p_specifier_qualifier_list_1(self, p):
        """ specifier_qualifier_list : specifier_qualifier_list type_specifier_no_typeid """
        p[1].type.append(p[2])
        p[0] = p[1]

    def p_specifier_qualifier_list_2(self, p):
        """ specifier_qualifier_list : specifier_qualifier_list type_qualifier """
        p[1].qualifiers |= p[2]
        p[0] = p[1]

    def p_specifier_qualifier_list_3(self, p):
        """ specifier_qualifier_list  : type_specifier """
        p[0] = DeclSpecifiers(type=[p[1]])

    def p_specifier_qualifier_list_4(self, p):
        """ specifier_qualifier_list  : type_qualifier_list type_specifier """
        p[0] = DeclSpecifiers(qualifiers=p[1], type=[p[2]])

    def p_specifier_qualifier_list_5(self, p):
        """ specifier_qualifier_list  : alignment_specifier """
        p[0] = DeclSpecifiers(alignment=[p[1]])

    def p_specifier_qualifier_list_6(self, p):
        """ specifier_qualifier_list  : specifier_qualifier_list alignment_specifier """
        p[1].alignment.append(p[2])
        p[0] = p[1]

    # TYPEID is allowed here (and in other struct/enum related tag names), because
    # struct/enum tags reside in their own namespace and can be named the same as types
    #
    def p_struct_or_union_specifier_1(self, p):
        """ struct_or_union_specifier   : struct_or_union ID
                                        | struct_or_union TYPEID
        """
        klass = self._select_struct_union_class(p[1])
        # None means no list of members
        p[0] = klass(name=p[2], decls=None, coord=self._token_coord(p, 2))

    def p_struct_or_union_specifier_2(self, p):
        """ struct_or_union_specifier : struct_or_union brace_open struct_declaration_list brace_close
                                      | struct_or_union brace_open brace_close
        """
        klass = self._select_struct_union_class(p[1])
        if len(p) == 4:
            # Empty sequence means an empty list of members
            p[0] = klass(name=None, decls=[], coord=self._token_coord(p, 2))
        else:
            p[0] = klass(name=None, decls=p[3], coord=self._token_coord(p, 2))

    def p_struct_or_union_specifier_3(self, p):
        """ struct_or_union_specifier   : struct_or_union ID brace_open struct_declaration_list brace_close
                                        | struct_or_union ID brace_open brace_close
                                        | struct_or_union TYPEID brace_open struct_declaration_list brace_close
                                        | struct_or_union TYPEID brace_open brace_close
        """
        klass = self._select_struct_union_class(p[1])
        if len(p) == 5:
            # Empty sequence means an empty list of members
            p[0] = klass(name=p[2], decls=[], coord=self._token_coord(p, 2))
        else:
            p[0] = klass(name=p[2], decls=p[4], coord=self._token_coord(p, 2))

    def p_struct_or_union(self, p):
        """ struct_or_union : STRUCT
                            | UNION
        """
        p[0] = p[1]

    # Combine all declarations into a single list
    #
    def p_struct_declaration_list(self, p):
        """ struct_declaration_list     : struct_declaration
                                        | struct_declaration_list struct_declaration
        """
        if len(p) == 2:
            p[0] = p[1] or []
        else:
            p[0] = p[1] + (p[2] or [])

    def p_struct_declaration_1(self, p):
        """ struct_declaration : specifier_qualifier_list struct_declarator_list_opt SEMI
        """
        declspecs = p[1]
        assert StorageSpecifierKind.TYPEDEF not in declspecs.storage

        if p[2] is not None:
            decls = self._build_declarations(decl_specifiers=declspecs, decls=p[2])

        elif len(declspecs.type) == 1:
            # Anonymous struct/union, gcc extension, C1x feature.
            # Although the standard only allows structs/unions here, I see no
            # reason to disallow other types since some compilers have typedefs
            # here, and cgrparser isn't about rejecting all invalid code.
            #
            node = declspecs.type[0]
            if isinstance(node, c_ast.Node):
                decl_type = node
            else:
                decl_type = c_ast.IdentifierType(node)

            decls = self._build_declarations(
                decl_specifiers=declspecs,
                decls=[StructDeclarator(decl=decl_type)],
            )

        else:
            # Structure/union members can have the same names as typedefs.
            # The trouble is that the member's name gets grouped into
            # specifier_qualifier_list; _build_declarations compensates.
            #
            decls = self._build_declarations(
                decl_specifiers=declspecs,
                decls=[InitDeclarator()],
            )

        p[0] = decls

    def p_struct_declaration_2(self, p):
        """ struct_declaration : SEMI """
        p[0] = None
    def p_struct_declaration_3(self, p):
        """ struct_declaration : pppragma_directive """
        p[0] = [p[1]]

    def p_struct_declarator_list_1(self, p):
        """ struct_declarator_list : struct_declarator """
        p[0] = [ p[1] ]
    def p_struct_declarator_list_2(self, p):
        """ struct_declarator_list : struct_declarator_list COMMA struct_declarator """
        p[0] = p[1] + [ p[3] ]

    def p_struct_declarator_1(self, p):
        """ struct_declarator : declarator """
        p[0] = StructDeclarator(decl=p[1])
    def p_struct_declarator_2(self, p):
        """ struct_declarator : declarator COLON constant_expression """
        p[0] = StructDeclarator(decl=p[1], bitsize=p[3])
    def p_struct_declarator_3(self, p):
        """ struct_declarator : COLON constant_expression """
        p[0] = StructDeclarator(decl=_dummy_typedecl(), bitsize=p[2])

    def p_enum_specifier_1(self, p):
        """ enum_specifier  : ENUM ID
                            | ENUM TYPEID
        """
        p[0] = c_ast.Enum(p[2], None, self._token_coord(p, 1))

    def p_enum_specifier_2(self, p):
        """ enum_specifier  : ENUM brace_open enumerator_list brace_close
        """
        p[0] = c_ast.Enum(None, p[3], self._token_coord(p, 1))

    def p_enum_specifier_3(self, p):
        """ enum_specifier  : ENUM ID brace_open enumerator_list brace_close
                            | ENUM TYPEID brace_open enumerator_list brace_close
        """
        p[0] = c_ast.Enum(p[2], p[4], self._token_coord(p, 1))

    def p_enumerator_list(self, p):
        """ enumerator_list : enumerator
                            | enumerator_list COMMA
                            | enumerator_list COMMA enumerator
        """
        if len(p) == 2:
            p[0] = c_ast.EnumeratorList([p[1]], p[1].coord)
        elif len(p) == 3:
            p[0] = p[1]
        else:
            p[1].enumerators.append(p[3])
            p[0] = p[1]

    def p_alignment_specifier(self, p):
        """ alignment_specifier  : _ALIGNAS LPAREN type_name RPAREN
                                 | _ALIGNAS LPAREN constant_expression RPAREN
        """
        p[0] = c_ast.Alignas(p[3], self._token_coord(p, 1))

    def p_enumerator(self, p):
        """ enumerator  : ID
                        | ID EQUALS constant_expression
        """
        if len(p) == 2:
            enumerator = c_ast.Enumerator(p[1], None, self._token_coord(p, 1))
        else:
            enumerator = c_ast.Enumerator(p[1], p[3], self._token_coord(p, 1))
        self._add_identifier(enumerator.name, enumerator.coord)

        p[0] = enumerator

    def p_declarator(self, p):
        """ declarator  : id_declarator
                        | typeid_declarator
        """
        p[0] = p[1]

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'))
    # @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_xxx_declarator_1(self, p):
        """ xxx_declarator  : direct_xxx_declarator
        """
        p[0] = p[1]

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'))
    # @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_xxx_declarator_2(self, p):
        """ xxx_declarator  : pointer direct_xxx_declarator
        """
        p[0] = self._type_modify_decl(p[2], p[1])

    def p_pointer_intent_1(self, p):
        """ pointer_intent : CGR_IN """
        p[0] = PtrIntent.IN
    def p_pointer_intent_2(self, p):
        """ pointer_intent : CGR_OUT """
        p[0] = PtrIntent.OUT
    def p_pointer_intent_3(self, p):
        """ pointer_intent : CGR_INOUT """
        p[0] = PtrIntent.INOUT

    @parameterized(('id', 'ID'), ('typeid_noparen', 'TYPEID'))
    def p_xxx_arg_declarator_1(self, p):
        """ xxx_arg_declarator  : direct_xxx_declarator """
        p[0] = p[1]
    @parameterized(('id', 'ID'), ('typeid_noparen', 'TYPEID'))
    def p_xxx_arg_declarator_2(self, p):
        """ xxx_arg_declarator  : pointer_intent pointer direct_xxx_declarator """
        p[2].intent = p[1]
        p[0] = self._type_modify_decl(p[3], p[2])
    @parameterized(('id', 'ID'), ('typeid_noparen', 'TYPEID'))
    def p_xxx_arg_declarator_3(self, p):
        """ xxx_arg_declarator  : pointer direct_xxx_declarator """
        p[0] = self._type_modify_decl(p[2], p[1])

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_direct_xxx_declarator_1(self, p):
        """ direct_xxx_declarator   : yyy
        """
        p[0] = c_ast.TypeDecl(
            declname=p[1],
            type=None,
            quals=TypeQualifierSpecifierKind.EMPTY,
            align=None,
            coord=self._token_coord(p, 1),
        )

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'))
    def p_direct_xxx_declarator_2(self, p):
        """ direct_xxx_declarator   : LPAREN xxx_declarator RPAREN
        """
        p[0] = p[2]

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_direct_xxx_declarator_3(self, p):
        """ direct_xxx_declarator   : direct_xxx_declarator LBRACKET type_qualifier_list_opt assignment_expression_opt RBRACKET
        """
        quals = (p[3] if len(p) > 5 else TypeQualifierSpecifierKind.EMPTY)
        quals = TypeQualifierSpecifierKind.EMPTY if quals is None else quals
        # Accept dimension qualifiers
        # Per C99 6.7.5.3 p7
        arr = c_ast.ArrayDecl(type=None, dim=p[4] if len(p) > 5 else p[3], dim_quals=quals, is_static_index=False, coord=p[1].coord)

        p[0] = self._type_modify_decl(decl=p[1], modifier=arr)

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_direct_xxx_declarator_4(self, p):
        """ direct_xxx_declarator   : direct_xxx_declarator LBRACKET STATIC type_qualifier_list_opt assignment_expression RBRACKET
                                    | direct_xxx_declarator LBRACKET type_qualifier_list STATIC assignment_expression RBRACKET
        """
        # Using slice notation for PLY objects doesn't work in Python 3 for the
        # version of PLY embedded with cgrparser; see PLY Google Code issue 30.
        # Work around that here by listing the two elements separately.
        if isinstance(p[3], TypeQualifierSpecifierKind):
            quals = p[3]
        elif isinstance(p[4], TypeQualifierSpecifierKind):
            quals = p[4]
        else:
            quals = TypeQualifierSpecifierKind.EMPTY
        arr = c_ast.ArrayDecl(type=None, dim=p[5], dim_quals=quals, is_static_index=True, coord=p[1].coord)

        p[0] = self._type_modify_decl(decl=p[1], modifier=arr)

    # Special for VLAs
    #
    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_direct_xxx_declarator_5(self, p):
        """ direct_xxx_declarator   : direct_xxx_declarator LBRACKET type_qualifier_list_opt TIMES RBRACKET
        """
        arr = c_ast.ArrayDecl(
            type=None,
            dim=c_ast.ID(p[4], self._token_coord(p, 4)),
            dim_quals=p[3] if p[3] is not None else TypeQualifierSpecifierKind.EMPTY,
            is_static_index=False,
            coord=p[1].coord,
        )

        p[0] = self._type_modify_decl(decl=p[1], modifier=arr)

    @parameterized(('id', 'ID'), ('typeid', 'TYPEID'), ('typeid_noparen', 'TYPEID'))
    def p_direct_xxx_declarator_6(self, p):
        """ direct_xxx_declarator   : direct_xxx_declarator LPAREN parameter_type_list RPAREN
                                    | direct_xxx_declarator LPAREN identifier_list_opt RPAREN
        """
        func = c_ast.FuncDecl(args=p[3], type=None, coord=p[1].coord)

        # To see why _get_yacc_lookahead_token is needed, consider:
        #   typedef char TT;
        #   void foo(int TT) { TT = 10; }
        # Outside the function, TT is a typedef, but inside (starting and
        # ending with the braces) it's a parameter.  The trouble begins with
        # yacc's lookahead token.  We don't know if we're declaring or
        # defining a function until we see LBRACE, but if we wait for yacc to
        # trigger a rule on that token, then TT will have already been read
        # and incorrectly interpreted as TYPEID.  We need to add the
        # parameters to the scope the moment the lexer sees LBRACE.
        #
        if self._get_yacc_lookahead_token().type == "LBRACE":
            if func.args is not None:
                for param in func.args.params:
                    if isinstance(param, c_ast.EllipsisParam):
                        break
                    self._add_identifier(param.name, param.coord)

        p[0] = self._type_modify_decl(decl=p[1], modifier=func)

    def p_nullness_qualifier_1(self, p):
        """ nullness_qualifier : CGR_NULLABLE """
        p[0] = PtrNullness.NULLABLE
    def p_nullness_qualifier_2(self, p):
        """ nullness_qualifier : CGR_NOT_NULL """
        p[0] = PtrNullness.NOT_NULL

    def p_pointer(self, p):
        """ pointer : TIMES nullness_qualifier_opt type_qualifier_list_opt
                    | TIMES nullness_qualifier_opt type_qualifier_list_opt pointer
        """
        coord = self._token_coord(p, 1)
        # Pointer decls nest from inside out. This is important when different
        # levels have different qualifiers. For example:
        #
        #   'char * const * p'   !=   'char ** const p'
        #
        # So when we construct PtrDecl nestings, the leftmost pointer goes in
        # as the most nested type.

        nested_type = c_ast.PtrDecl(
            quals=TypeQualifierSpecifierKind.EMPTY if p[3] is None else p[3],
            nullness=PtrNullness.UNKNOWN if p[2] is None else p[2],
            intent=PtrIntent.UNKNOWN,
            type=None,
            coord=coord,
        )
        if len(p) == 4:
            p[0] = nested_type
            return

        tail_type = p[4]
        while tail_type.type is not None:
            tail_type = tail_type.type
        tail_type.type = nested_type
        p[0] = p[4]

    def p_type_qualifier_list_1(self, p):
        """ type_qualifier_list : type_qualifier """
        p[0] = p[1]
    def p_type_qualifier_list_2(self, p):
        """ type_qualifier_list : type_qualifier_list type_qualifier """
        p[0] = p[1] | p[2]

    def p_parameter_type_list(self, p):
        """ parameter_type_list : parameter_list
                                | parameter_list COMMA ELLIPSIS
        """
        if len(p) > 2:
            p[1].params.append(c_ast.EllipsisParam(self._token_coord(p, 3)))

        p[0] = p[1]

    def p_parameter_list(self, p):
        """ parameter_list  : parameter_declaration
                            | parameter_list COMMA parameter_declaration
        """
        if len(p) == 2:   # single parameter
            p[0] = c_ast.ParamList([p[1]], p[1].coord)
        else:
            p[1].params.append(p[3])
            p[0] = p[1]

    # From ISO/IEC 9899:TC2, 6.7.5.3.11:
    # "If, in a parameter declaration, an identifier can be treated either
    #  as a typedef name or as a parameter name, it shall be taken as a
    #  typedef name."
    #
    # Inside a parameter declaration, once we've reduced declaration specifiers,
    # if we shift in an LPAREN and see a TYPEID, it could be either an abstract
    # declarator or a declarator nested inside parens. This rule tells us to
    # always treat it as an abstract declarator. Therefore, we only accept
    # `id_declarator`s and `typeid_noparen_declarator`s.
    def p_parameter_declaration_1(self, p):
        """ parameter_declaration   : declaration_specifiers id_arg_declarator
                                    | declaration_specifiers typeid_noparen_arg_declarator
        """
        decl_specifiers = p[1]
        if len(decl_specifiers.type) == 0:
            decl_specifiers.type.append( c_ast.IdentifierType(['int'], coord=self._token_coord(p, 1)) )
        p[0] = self._build_declarations(
            decl_specifiers=decl_specifiers,
            decls=[InitDeclarator(decl=p[2])]
        )[0]

    def p_parameter_declaration_2(self, p):
        """ parameter_declaration   : declaration_specifiers arg_abstract_declarator_opt
        """
        spec: DeclSpecifiers = p[1]
        if len(spec.type) == 0:
            spec.type.append(c_ast.IdentifierType(['int'], coord=self._token_coord(p, 1)))

        # Parameters can have the same names as typedefs.  The trouble is that
        # the parameter's name gets grouped into declaration_specifiers, making
        # it look like an old-style declaration; compensate.
        #
        if (    len(spec.type) > 1
            and len(names := typing.cast(c_ast.IdentifierType, spec.type[-1]).names) == 1
            and self._is_type_in_scope(names[0])
        ):
            decl = self._build_declarations(decl_specifiers=spec, decls=[InitDeclarator(decl=p[2])])[0]

        # This truly is an old-style parameter declaration
        #
        else:
            decl = c_ast.Typename(
                name='',
                quals=spec.qualifiers,
                align=None,
                type=p[2] or _dummy_typedecl(),
                coord=self._token_coord(p, 2),
            )
            typename = spec.type
            decl = self._fix_decl_name_type(decl, typename)

        p[0] = decl

    def p_identifier_list(self, p):
        """ identifier_list : identifier
                            | identifier_list COMMA identifier
        """
        if len(p) == 2:   # single parameter
            p[0] = c_ast.ParamList([p[1]], p[1].coord)
        else:
            p[1].params.append(p[3])
            p[0] = p[1]

    def p_initializer_1(self, p):
        """ initializer : assignment_expression
        """
        p[0] = p[1]

    def p_initializer_2(self, p):
        """ initializer : brace_open initializer_list_opt brace_close
                        | brace_open initializer_list COMMA brace_close
        """
        if p[2] is None:
            p[0] = c_ast.InitList([], self._token_coord(p, 1))
        else:
            p[0] = p[2]

    def p_initializer_list(self, p):
        """ initializer_list    : designation_opt initializer
                                | initializer_list COMMA designation_opt initializer
        """
        if len(p) == 3:   # single initializer
            init = p[2] if p[1] is None else c_ast.NamedInitializer(p[1], p[2])
            p[0] = c_ast.InitList([init], p[2].coord)
        else:
            init = p[4] if p[3] is None else c_ast.NamedInitializer(p[3], p[4])
            p[1].exprs.append(init)
            p[0] = p[1]

    def p_designation(self, p):
        """ designation : designator_list EQUALS
        """
        p[0] = p[1]

    # Designators are represented as a list of nodes, in the order in which
    # they're written in the code.
    #
    def p_designator_list(self, p):
        """ designator_list : designator
                            | designator_list designator
        """
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]

    def p_designator(self, p):
        """ designator  : LBRACKET constant_expression RBRACKET
                        | PERIOD identifier
        """
        p[0] = p[2]

    def p_type_name(self, p):
        """ type_name   : specifier_qualifier_list abstract_declarator_opt """
        typename = c_ast.Typename(
            name='',
            quals=p[1].qualifiers,
            align=None,
            type=p[2] or _dummy_typedecl(),
            coord=self._token_coord(p, 2),
        )

        p[0] = self._fix_decl_name_type(typename, p[1].type)

    def p_abstract_declarator_1(self, p):
        """ abstract_declarator     : pointer """
        p[0] = self._type_modify_decl(decl=_dummy_typedecl(), modifier=p[1])
    def p_abstract_declarator_2(self, p):
        """ abstract_declarator     : pointer direct_abstract_declarator """
        p[0] = self._type_modify_decl(p[2], p[1])
    def p_abstract_declarator_3(self, p):
        """ abstract_declarator     : direct_abstract_declarator """
        p[0] = p[1]

    def p_arg_abstract_declarator_1(self, p):
        """ arg_abstract_declarator     : pointer """
        p[0] = self._type_modify_decl(decl=_dummy_typedecl(), modifier=p[1])
    def p_arg_abstract_declarator_2(self, p):
        """ arg_abstract_declarator     : pointer_intent pointer """
        p[2].intent = p[1]
        p[0] = self._type_modify_decl(decl=_dummy_typedecl(), modifier=p[2])
    def p_arg_abstract_declarator_3(self, p):
        """ arg_abstract_declarator     : pointer_intent pointer direct_abstract_declarator """
        p[2].intent = p[1]
        p[0] = self._type_modify_decl(p[3], p[2])
    def p_arg_abstract_declarator_4(self, p):
        """ arg_abstract_declarator     : pointer direct_abstract_declarator """
        p[0] = self._type_modify_decl(p[2], p[1])

    def p_arg_abstract_declarator_5(self, p):
        """ arg_abstract_declarator     : direct_abstract_declarator """
        p[0] = p[1]

    # Creating and using direct_abstract_declarator_opt here
    # instead of listing both direct_abstract_declarator and the
    # lack of it in the beginning of _1 and _2 caused two
    # shift/reduce errors.
    #
    def p_direct_abstract_declarator_1(self, p):
        """ direct_abstract_declarator  : LPAREN abstract_declarator RPAREN """
        p[0] = p[2]

    def p_direct_abstract_declarator_2(self, p):
        """ direct_abstract_declarator  : direct_abstract_declarator LBRACKET assignment_expression_opt RBRACKET
        """
        arr = c_ast.ArrayDecl(
            type=None,
            dim=p[3],
            dim_quals=TypeQualifierSpecifierKind.EMPTY,
            is_static_index=False,
            coord=p[1].coord,
        )

        p[0] = self._type_modify_decl(decl=p[1], modifier=arr)

    def p_direct_abstract_declarator_3(self, p):
        """ direct_abstract_declarator  : LBRACKET type_qualifier_list_opt assignment_expression_opt RBRACKET
        """

        # TODO: remove this assert
        assert len(p) == 5, "I guess this was necessaire: 'quals = (p[2] if len(p) > 4 else []) or []'"

        p[0] = c_ast.ArrayDecl(
            type=_dummy_typedecl(),
            dim=p[3] if len(p) > 4 else p[2],
            dim_quals=TypeQualifierSpecifierKind.EMPTY if p[2] is None else p[2],
            is_static_index=False,
            coord=self._token_coord(p, 1),
        )

    def p_direct_abstract_declarator_4(self, p):
        """ direct_abstract_declarator  : direct_abstract_declarator LBRACKET TIMES RBRACKET
        """
        arr = c_ast.ArrayDecl(
            type=None,
            dim=c_ast.ID(p[3], self._token_coord(p, 3)),
            dim_quals=TypeQualifierSpecifierKind.EMPTY,
            is_static_index=False,
            coord=p[1].coord,
        )

        p[0] = self._type_modify_decl(decl=p[1], modifier=arr)

    def p_direct_abstract_declarator_5(self, p):
        """ direct_abstract_declarator  : LBRACKET TIMES RBRACKET
        """
        p[0] = c_ast.ArrayDecl(
            type=_dummy_typedecl(),
            dim=c_ast.ID(p[3], self._token_coord(p, 3)),
            dim_quals=TypeQualifierSpecifierKind.EMPTY,
            is_static_index=False,
            coord=self._token_coord(p, 1),
        )

    def p_direct_abstract_declarator_6(self, p):
        """ direct_abstract_declarator  : direct_abstract_declarator LPAREN parameter_type_list_opt RPAREN
        """
        func = c_ast.FuncDecl(args=p[3], type=None, coord=p[1].coord)

        p[0] = self._type_modify_decl(decl=p[1], modifier=func)

    def p_direct_abstract_declarator_7(self, p):
        """ direct_abstract_declarator  : LPAREN parameter_type_list_opt RPAREN
        """
        p[0] = c_ast.FuncDecl(
            args=p[2],
            type=_dummy_typedecl(),
            coord=self._token_coord(p, 1),
        )

    def p_direct_abstract_declarator_8(self, p):
        """ direct_abstract_declarator  : LBRACKET STATIC type_qualifier_list_opt assignment_expression RBRACKET
                                        | LBRACKET type_qualifier_list     STATIC assignment_expression RBRACKET
        """
        if isinstance(p[2], TypeQualifierSpecifierKind):
            quals = p[2]
        elif isinstance(p[3], TypeQualifierSpecifierKind):
            quals = p[3]
        else:
            quals = TypeQualifierSpecifierKind.EMPTY
        p[0] = c_ast.ArrayDecl(
            type=_dummy_typedecl(),
            dim=p[4],
            dim_quals=quals,
            is_static_index=True,
            coord=self._token_coord(p, 1),
        )

    # declaration is a list, statement isn't. To make it consistent, block_item
    # will always be a list
    #
    def p_block_item(self, p):
        """ block_item  : declaration
                        | statement
        """
        p[0] = p[1] if isinstance(p[1], list) else [p[1]]

    # Since we made block_item a list, this just combines lists
    #
    def p_block_item_list(self, p):
        """ block_item_list : block_item
                            | block_item_list block_item
        """
        # Empty block items (plain ';') produce [None], so ignore them
        p[0] = p[1] if (len(p) == 2 or p[2] == [None]) else p[1] + p[2]

    def p_compound_statement_1(self, p):
        """ compound_statement : brace_open block_item_list_opt brace_close """
        items = [] if p[2] is None else p[2]
        if not isinstance(items, list):
            raise ValueError('items aint no list')
        p[0] = c_ast.Compound(block_items=items, coord=self._token_coord(p, 1))

    def p_labeled_statement_1(self, p):
        """ labeled_statement : ID COLON pragmacomp_or_statement """
        p[0] = c_ast.Label(p[1], p[3], self._token_coord(p, 1))

    def p_labeled_statement_2(self, p):
        """ labeled_statement : CASE constant_expression COLON pragmacomp_or_statement_case
        """
        p[0] = c_ast.Case(p[2], [p[4]], self._token_coord(p, 1))

    def p_labeled_statement_3(self, p):
        """ labeled_statement : DEFAULT COLON pragmacomp_or_statement """
        p[0] = c_ast.Default([p[3]], self._token_coord(p, 1))

    def p_selection_statement_1(self, p):
        """ selection_statement : IF LPAREN expression RPAREN pragmacomp_or_statement """
        p[0] = c_ast.If(p[3], p[5], None, self._token_coord(p, 1))

    def p_selection_statement_2(self, p):
        """ selection_statement : IF LPAREN expression RPAREN statement ELSE pragmacomp_or_statement """
        p[0] = c_ast.If(p[3], p[5], p[7], self._token_coord(p, 1))

    def p_selection_statement_3(self, p):
        """ selection_statement : SWITCH LPAREN expression RPAREN pragmacomp_or_statement """
        p[0] = fix_switch_cases(c_ast.Switch(p[3], p[5], self._token_coord(p, 1)))

    def p_iteration_statement_1(self, p):
        """ iteration_statement : WHILE LPAREN expression RPAREN pragmacomp_or_statement """
        p[0] = c_ast.While(p[3], p[5], self._token_coord(p, 1))

    def p_iteration_statement_2(self, p):
        """ iteration_statement : DO pragmacomp_or_statement WHILE LPAREN expression RPAREN SEMI """
        p[0] = c_ast.DoWhile(p[5], p[2], self._token_coord(p, 1))

    def p_iteration_statement_3(self, p):
        """ iteration_statement : FOR LPAREN expression_opt SEMI expression_opt SEMI expression_opt RPAREN pragmacomp_or_statement """
        p[0] = c_ast.For(p[3], p[5], p[7], p[9], self._token_coord(p, 1))

    def p_iteration_statement_4(self, p):
        """ iteration_statement : FOR LPAREN declaration expression_opt SEMI expression_opt RPAREN pragmacomp_or_statement """
        p[0] = c_ast.For(
            c_ast.DeclList(p[3], self._token_coord(p, 1)),
            p[4],
            p[6],
            p[8],
            self._token_coord(p, 1),
        )

    def p_jump_statement_1(self, p):
        """ jump_statement  : GOTO ID SEMI """
        p[0] = c_ast.Goto(p[2], self._token_coord(p, 1))

    def p_jump_statement_2(self, p):
        """ jump_statement  : BREAK SEMI """
        p[0] = c_ast.Break(self._token_coord(p, 1))

    def p_jump_statement_3(self, p):
        """ jump_statement  : CONTINUE SEMI """
        p[0] = c_ast.Continue(self._token_coord(p, 1))

    def p_jump_statement_4(self, p):
        """ jump_statement  : RETURN expression SEMI
                            | RETURN SEMI
        """
        p[0] = c_ast.Return(p[2] if len(p) == 4 else None, self._token_coord(p, 1))

    def p_expression_statement(self, p):
        """ expression_statement : expression_opt SEMI """
        if p[1] is None:
            p[0] = c_ast.EmptyStatement(self._token_coord(p, 2))
        else:
            p[0] = p[1]

    def p_expression(self, p):
        """ expression  : assignment_expression
                        | expression COMMA assignment_expression
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            if not isinstance(p[1], c_ast.ExprList):
                p[1] = c_ast.ExprList([p[1]], p[1].coord)

            p[1].exprs.append(p[3])
            p[0] = p[1]

    def p_parenthesized_compound_expression(self, p):
        """ assignment_expression : LPAREN compound_statement RPAREN """
        p[0] = p[2]

    def p_typedef_name(self, p):
        """ typedef_name : TYPEID """
        p[0] = c_ast.IdentifierType([p[1]], coord=self._token_coord(p, 1))

    def p_assignment_expression(self, p):
        """ assignment_expression   : conditional_expression
                                    | unary_expression assignment_operator assignment_expression
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = c_ast.Assignment(p[2], p[1], p[3], p[1].coord)

    # K&R2 defines these as many separate rules, to encode
    # precedence and associativity. Why work hard ? I'll just use
    # the built in precedence/associativity specification feature
    # of PLY. (see precedence declaration above)
    def p_assignment_operator(self, p):
        """ assignment_operator : EQUALS
                                | XOREQUAL
                                | TIMESEQUAL
                                | DIVEQUAL
                                | MODEQUAL
                                | PLUSEQUAL
                                | MINUSEQUAL
                                | LSHIFTEQUAL
                                | RSHIFTEQUAL
                                | ANDEQUAL
                                | OREQUAL
        """
        p[0] = p[1]

    def p_constant_expression(self, p):
        """ constant_expression : conditional_expression """
        p[0] = p[1]

    def p_conditional_expression(self, p):
        """ conditional_expression  : binary_expression
                                    | binary_expression CONDOP expression COLON conditional_expression
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = c_ast.TernaryOp(p[1], p[3], p[5], p[1].coord)

    def p_binary_expression(self, p):
        """ binary_expression   : cast_expression
                                | binary_expression TIMES binary_expression
                                | binary_expression DIVIDE binary_expression
                                | binary_expression MOD binary_expression
                                | binary_expression PLUS binary_expression
                                | binary_expression MINUS binary_expression
                                | binary_expression RSHIFT binary_expression
                                | binary_expression LSHIFT binary_expression
                                | binary_expression LT binary_expression
                                | binary_expression LE binary_expression
                                | binary_expression GE binary_expression
                                | binary_expression GT binary_expression
                                | binary_expression EQ binary_expression
                                | binary_expression NE binary_expression
                                | binary_expression AND binary_expression
                                | binary_expression OR binary_expression
                                | binary_expression XOR binary_expression
                                | binary_expression LAND binary_expression
                                | binary_expression LOR binary_expression
        """
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = c_ast.BinaryOp(p[2], p[1], p[3], p[1].coord)

    def p_cast_expression_1(self, p):
        """ cast_expression : unary_expression """
        p[0] = p[1]

    def p_cast_expression_2(self, p):
        """ cast_expression : LPAREN type_name RPAREN cast_expression """
        p[0] = c_ast.Cast(p[2], p[4], self._token_coord(p, 1))

    def p_unary_expression_1(self, p):
        """ unary_expression    : postfix_expression """
        p[0] = p[1]

    def p_unary_expression_2(self, p):
        """ unary_expression    : PLUSPLUS unary_expression
                                | MINUSMINUS unary_expression
                                | unary_operator cast_expression
        """
        p[0] = c_ast.UnaryOp(p[1], p[2], p[2].coord)

    def p_unary_expression_3(self, p):
        """ unary_expression    : SIZEOF unary_expression
                                | SIZEOF LPAREN type_name RPAREN
                                | _ALIGNOF LPAREN type_name RPAREN
        """
        p[0] = c_ast.UnaryOp(p[1], p[2] if len(p) == 3 else p[3], self._token_coord(p, 1))

    def p_unary_operator(self, p):
        """ unary_operator  : AND
                            | TIMES
                            | PLUS
                            | MINUS
                            | NOT
                            | LNOT
        """
        p[0] = p[1]

    def p_postfix_expression_1(self, p):
        """ postfix_expression  : primary_expression """
        p[0] = p[1]

    def p_postfix_expression_2(self, p):
        """ postfix_expression  : postfix_expression LBRACKET expression RBRACKET """
        p[0] = c_ast.ArrayRef(p[1], p[3], p[1].coord)

    def p_postfix_expression_3(self, p):
        """ postfix_expression  : postfix_expression LPAREN argument_expression_list RPAREN
                                | postfix_expression LPAREN RPAREN
        """
        p[0] = c_ast.FuncCall(p[1], p[3] if len(p) == 5 else None, p[1].coord)

    def p_postfix_expression_4(self, p):
        """ postfix_expression  : postfix_expression PERIOD ID
                                | postfix_expression PERIOD TYPEID
                                | postfix_expression ARROW ID
                                | postfix_expression ARROW TYPEID
        """
        field = c_ast.ID(p[3], self._token_coord(p, 3))
        p[0] = c_ast.StructRef(p[1], p[2], field, p[1].coord)

    def p_postfix_expression_5(self, p):
        """ postfix_expression  : postfix_expression PLUSPLUS
                                | postfix_expression MINUSMINUS
        """
        p[0] = c_ast.UnaryOp('p' + p[2], p[1], p[1].coord)

    def p_postfix_expression_6(self, p):
        """ postfix_expression  : LPAREN type_name RPAREN brace_open initializer_list brace_close
                                | LPAREN type_name RPAREN brace_open initializer_list COMMA brace_close
        """
        p[0] = c_ast.CompoundLiteral(p[2], p[5])

    def p_primary_expression_1(self, p):
        """ primary_expression  : identifier """
        p[0] = p[1]

    def p_primary_expression_2(self, p):
        """ primary_expression  : constant """
        p[0] = p[1]

    def p_primary_expression_3(self, p):
        """ primary_expression  : unified_string_literal
                                | unified_wstring_literal
        """
        p[0] = p[1]

    def p_primary_expression_4(self, p):
        """ primary_expression  : LPAREN expression RPAREN """
        p[0] = p[2]

    def p_primary_expression_5(self, p):
        """ primary_expression  : OFFSETOF LPAREN type_name COMMA offsetof_member_designator RPAREN
        """
        coord = self._token_coord(p, 1)
        p[0] = c_ast.FuncCall(c_ast.ID(p[1], coord), c_ast.ExprList([p[3], p[5]], coord), coord)

    def p_offsetof_member_designator(self, p):
        """ offsetof_member_designator : identifier
                                         | offsetof_member_designator PERIOD identifier
                                         | offsetof_member_designator LBRACKET expression RBRACKET
        """
        if len(p) == 2:
            p[0] = p[1]
        elif len(p) == 4:
            p[0] = c_ast.StructRef(p[1], p[2], p[3], p[1].coord)
        elif len(p) == 5:
            p[0] = c_ast.ArrayRef(p[1], p[3], p[1].coord)
        else:
            raise NotImplementedError("Unexpected parsing state. len(p): %u" % len(p))

    def p_argument_expression_list(self, p):
        """ argument_expression_list    : assignment_expression
                                        | argument_expression_list COMMA assignment_expression
        """
        if len(p) == 2:   # single expr
            p[0] = c_ast.ExprList([p[1]], p[1].coord)
        else:
            p[1].exprs.append(p[3])
            p[0] = p[1]

    def p_identifier(self, p):
        """ identifier  : ID """
        p[0] = c_ast.ID(p[1], self._token_coord(p, 1))

    def p_constant_1(self, p):
        """ constant    : INT_CONST_DEC
                        | INT_CONST_OCT
                        | INT_CONST_HEX
                        | INT_CONST_BIN
                        | INT_CONST_CHAR
        """
        uCount = 0
        lCount = 0
        for x in p[1][-3:]:
            if x in ('l', 'L'):
                lCount += 1
            elif x in ('u', 'U'):
                uCount += 1
        t = ''
        if uCount > 1:
            raise ValueError('Constant cannot have more than one u/U suffix.')
        elif lCount > 2:
            raise ValueError('Constant cannot have more than two l/L suffix.')
        prefix = 'unsigned ' * uCount + 'long ' * lCount
        p[0] = c_ast.Constant(prefix + 'int', p[1], self._token_coord(p, 1))

    def p_constant_2(self, p):
        """ constant    : FLOAT_CONST
                        | HEX_FLOAT_CONST
        """
        if 'x' in p[1].lower():
            t = 'float'
        else:
            if p[1][-1] in ('f', 'F'):
                t = 'float'
            elif p[1][-1] in ('l', 'L'):
                t = 'long double'
            else:
                t = 'double'

        p[0] = c_ast.Constant(t, p[1], self._token_coord(p, 1))

    def p_constant_3(self, p):
        """ constant    : CHAR_CONST
                        | WCHAR_CONST
                        | U8CHAR_CONST
                        | U16CHAR_CONST
                        | U32CHAR_CONST
        """
        p[0] = c_ast.Constant('char', p[1], self._token_coord(p, 1))

    # The "unified" string and wstring literal rules are for supporting
    # concatenation of adjacent string literals.
    # I.e. "hello " "world" is seen by the C compiler as a single string literal
    # with the value "hello world"
    #
    def p_unified_string_literal(self, p):
        """ unified_string_literal  : STRING_LITERAL
                                    | unified_string_literal STRING_LITERAL
        """
        if len(p) == 2:   # single literal
            p[0] = c_ast.Constant('string', p[1], self._token_coord(p, 1))
        else:
            p[1].value = p[1].value[:-1] + p[2][1:]
            p[0] = p[1]

    def p_unified_wstring_literal(self, p):
        """ unified_wstring_literal : WSTRING_LITERAL
                                    | U8STRING_LITERAL
                                    | U16STRING_LITERAL
                                    | U32STRING_LITERAL
                                    | unified_wstring_literal WSTRING_LITERAL
                                    | unified_wstring_literal U8STRING_LITERAL
                                    | unified_wstring_literal U16STRING_LITERAL
                                    | unified_wstring_literal U32STRING_LITERAL
        """
        if len(p) == 2:   # single literal
            p[0] = c_ast.Constant('string', p[1], self._token_coord(p, 1))
        else:
            p[1].value = p[1].value.rstrip()[:-1] + p[2][2:]
            p[0] = p[1]

    def p_brace_open(self, p):
        """ brace_open  :   LBRACE
        """
        p[0] = p[1]
        p.set_lineno(0, p.lineno(1))

    def p_brace_close(self, p):
        """ brace_close :   RBRACE
        """
        p[0] = p[1]
        p.set_lineno(0, p.lineno(1))

    def p_empty(self, p):
        'empty : '
        p[0] = None

    def p_error(self, p):
        # If error recovery is added here in the future, make sure
        # _get_yacc_lookahead_token still works!
        #
        if p:
            self._parse_error(
                f'expected other tokens before: {p.value!r}',
                self._coord(lineno=p.lineno, column=self.clex.find_tok_column(p)),
                note=_PARSE_ERROR_HELP_NOTES.get(p.value),
            )
        else:
            self._parse_error(
                'expected other tokens before reaching end-of-file...',
                self.clex.filename,
                note='Perhaps your code has unbalanced braces/brackets/parentheses?'
            )
