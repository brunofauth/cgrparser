from __future__ import annotations

import dataclasses as dc
import enum
import typing

from .c_ast import IdentifierType, Enum, Struct, Union, Typename, Alignas, Node


type TypeSpecifierKind = IdentifierType | Enum | Struct | Union | Typename

@enum.unique
class TypeSpecifier(enum.IntEnum):
    VOID = 1
    BOOL = enum.auto()
    CHAR = enum.auto()
    SHORT = enum.auto()
    INT = enum.auto()
    LONG = enum.auto()
    FLOAT = enum.auto()
    DOUBLE = enum.auto()
    COMPLEX = enum.auto()
    SIGNED = enum.auto()
    UNSIGNED = enum.auto()
    INT128 = enum.auto()

    def __str__(self) -> str:
        return self.__class__._str_table[self]

# We use +1 here because this enum starts at 1
_table = TypeSpecifier._str_table = [None for _ in range(len(TypeSpecifier) + 1)]
_table[TypeSpecifier.VOID]     = "void"
_table[TypeSpecifier.BOOL]     = "_Bool"
_table[TypeSpecifier.CHAR]     = "char"
_table[TypeSpecifier.SHORT]    = "short"
_table[TypeSpecifier.INT]      = "int"
_table[TypeSpecifier.LONG]     = "long"
_table[TypeSpecifier.FLOAT]    = "float"
_table[TypeSpecifier.DOUBLE]   = "double"
_table[TypeSpecifier.COMPLEX]  = "_Complex"
_table[TypeSpecifier.SIGNED]   = "signed"
_table[TypeSpecifier.UNSIGNED] = "unsigned"
_table[TypeSpecifier.INT128]   = "__int128"

# Hence we have to take that +1 into account here
assert _table.count(None) == 1, \
    f"It looks like {TypeSpecifier.__name__!r} has grown, but its table hasn't been updated"


@enum.unique
class TypeQualifierSpecifierKind(enum.IntFlag):
    EMPTY = 0
    CONST = enum.auto()
    RESTRICT = enum.auto()
    VOLATILE = enum.auto()
    ATOMIC = enum.auto()

    def __str__(self) -> str:
        match self:
            case TypeQualifierSpecifierKind.EMPTY: return ""
            case TypeQualifierSpecifierKind.CONST: return "const"
            case TypeQualifierSpecifierKind.RESTRICT: return "restrict"
            case TypeQualifierSpecifierKind.VOLATILE: return "volatile"
            case TypeQualifierSpecifierKind.ATOMIC: return "_Atomic"


@enum.unique
class StorageSpecifierKind(enum.IntFlag):
    EMPTY = 0
    AUTO = enum.auto()
    REGISTER = enum.auto()
    STATIC = enum.auto()
    EXTERN = enum.auto()
    TYPEDEF = enum.auto()
    THREAD_LOCAL = enum.auto()

    def __str__(self) -> str:
        match self:
            case StorageSpecifierKind.EMPTY: return ""
            case StorageSpecifierKind.AUTO: return "auto"
            case StorageSpecifierKind.REGISTER: return "register"
            case StorageSpecifierKind.STATIC: return "static"
            case StorageSpecifierKind.EXTERN: return "extern"
            case StorageSpecifierKind.TYPEDEF: return "typedef"
            case StorageSpecifierKind.THREAD_LOCAL: return "_Thread_local"


@enum.unique
class FunctionSpecifierKind(enum.IntFlag):
    EMPTY = 0
    INLINE = enum.auto()
    NORETURN = enum.auto()

    def __str__(self) -> str:
        match self:
            case FunctionSpecifierKind.EMPTY: return ""
            case FunctionSpecifierKind.INLINE: return "inline"
            case FunctionSpecifierKind.NORETURN: return "_Noreturn"


@dc.dataclass(kw_only=True, slots=True)
class DeclSpecifiers:
    type: list[TypeSpecifierKind] = dc.field(default_factory=list)
    qualifiers: TypeQualifierSpecifierKind = TypeQualifierSpecifierKind.EMPTY
    storage: StorageSpecifierKind = StorageSpecifierKind.EMPTY
    function: FunctionSpecifierKind = FunctionSpecifierKind.EMPTY
    alignment: list[Alignas] = dc.field(default_factory=list)


@dc.dataclass(kw_only=True, slots=True)
class StructDeclarator:
    decl: Declarator
    bitsize: Node | None = None

@dc.dataclass(kw_only=True, slots=True)
class InitDeclarator:
    decl: Declarator | None = None
    init: Node | None = None

@enum.unique
class PtrNullness(enum.IntEnum):
    UNKNOWN = 0
    NULLABLE = enum.auto()
    NOT_NULL = enum.auto()

    def __str__(self) -> str:
        return self.__class__._str_table[self]

_table = PtrNullness._str_table = [None for _ in range(len(PtrNullness))]
_table[PtrNullness.UNKNOWN]  = ""
_table[PtrNullness.NULLABLE] = "cgr_nullable"
_table[PtrNullness.NOT_NULL] = "cgr_not_null"
assert _table.count(None) == 0, \
    f"It looks like {PtrNullness.__name__!r} has grown, but its table hasn't been updated"


@enum.unique
class PtrIntent(enum.IntEnum):
    UNKNOWN = 0
    IN = enum.auto()
    OUT = enum.auto()
    INOUT = enum.auto()

    def __str__(self) -> str:
        return self.__class__._str_table[self]

_table = PtrIntent._str_table = [None for _ in range(len(PtrIntent))]
_table[PtrIntent.UNKNOWN] = ""
_table[PtrIntent.IN]      = "cgr_in"
_table[PtrIntent.OUT]     = "cgr_out"
_table[PtrIntent.INOUT]   = "cgr_inout"
assert _table.count(None) == 0, \
    f"It looks like {PtrIntent.__name__!r} has grown, but its table hasn't been updated"

