from __future__ import annotations

import dataclasses as dc
import enum
import typing

from .c_ast import IdentifierType, Enum, Struct, Union, Typename, Alignas, Node


type TypeSpecifierKind = IdentifierType | Enum | Struct | Union | Typename

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
        match self:
            case PtrNullness.UNKNOWN: return ""
            case PtrNullness.NULLABLE: return "cgr_nullable"
            case PtrNullness.NOT_NULL: return "cgr_not_null"

@enum.unique
class PtrIntent(enum.IntEnum):
    UNKNOWN = 0
    IN = enum.auto()
    OUT = enum.auto()
    INOUT = enum.auto()

    def __str__(self) -> str:
        match self:
            case PtrIntent.UNKNOWN: return ""
            case PtrIntent.IN: return "cgr_in"
            case PtrIntent.OUT: return "cgr_out"
            case PtrIntent.INOUT: return "cgr_inout"

