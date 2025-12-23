#ifndef TYPE_CHECKER_H
#define TYPE_CHECKER_H

typedef i64 TypeIndex;

enum TypeKind
{
	// Static types
	Type_Void = 0,
	Type_Bool,
	Type_Int,
	Type_Float,

	// Dynamic types
	Type_Pointer,
	Type_Array,
	Type_Struct,
	Type_Function,
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

struct FunctionType
{
	String8 Name;
	TypeIndex ReturnType;

	String8* ArgNames;
	TypeIndex* ArgTypes;
	u32 ArgCount;

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

// struct Symbol;
struct Symbol
{
	TypeIndex Type;
	String8 Name;	
	b32 Constant;

	LLVMValueRef Value;
	
	Symbol* Next;
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
	u32 ErrorCount;

	b32 TreeChanged;
	b32 InFunction;
	
#define TYPE_CAPACITY 1024
	TypeInfo Types[TYPE_CAPACITY];
	u32 TypeCount;	
	
#define FUNCTION_CAPACITY 1024
	FunctionType Functions[FUNCTION_CAPACITY];
	u32 FunctionCount;
};

#endif // Header guard
