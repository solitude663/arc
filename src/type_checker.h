#ifndef TYPE_CHECKER_H
#define TYPE_CHECKER_H

typedef i64 TypeIndex;

enum TypeKind
{
	// Static types
	Type_Int,
	Type_Bool,
	Type_Void,
	Type_Float,

	// Dynamic types
	Type_Pointer,
	Type_Array,
	Type_Struct,
};

struct PointerType
{
	TypeIndex Base;
};

struct StructType
{
	String8 Name;
	String8* FieldNames;
	TypeIndex* FieldTypes;
	u64 FieldCount;
};

struct TypeInfo
{
	TypeKind Type;
	u32 Size;
	union
	{
		PointerType Pointer;
		StructType Struct;
	};
};

struct Symbol;
struct Symbol
{
	TypeIndex Type;
	String8 Name;
	Symbol* Next;
	b32 IsConstant;
};

struct SymbolTable
{
	SymbolTable* Parent;
	Symbol* Symbols;
	u32 SymbolCount;
};

struct TypeChecker
{
	M_Arena* Arena;
#define TYPE_CAPACITY 1024
	TypeInfo Types[TYPE_CAPACITY];
	u64 TypeCount;
};

#endif // Header guard
