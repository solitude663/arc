#ifndef PARSER_H
#define PARSER_H

#include "./type_checker.h"

enum TokenType
{
	Token_OpenParen = '(',
	Token_CloseParen = ')',
	Token_Plus = '+',
	Token_Minus = '-',
	Token_Star = '*',
	Token_Slash = '/',
	Token_Colon = ':',
	Token_SemiColon = ';',
	Token_Equal = '=',
	Token_OpenBracket = '[',
	Token_CloseBracket = ']',
	Token_Comma = ',',
	Token_Dot = '.',

	Token_OpenBrace = '{',
	Token_CloseBrace = '}',
	
	Token_EOF = '\0',
	Token_Invalid = 404,	
	
	Token_IntegerLiteral = 300,
	Token_FloatLiteral,

	Token_ColonColon,
	
	Token_Identifier,
	Token_Print,
	Token_Int,
	Token_Float,
	Token_Bool,
	Token_True,
	Token_False,
	Token_Extern,
	Token_Count,
};

#define TOKEN_RING_BUFFER_SIZE 8

struct Token
{
	TokenType Type;
	String8 Lexeme;

	u32 RowNumber;
	u32 ColumnNumber;
	String8 FilePath; // TODO(afb) :: Should be a index to a file

	union
	{
		i64 IntValue;
		f64 FloatValue;
	};
};

struct Parser
{
	M_Arena* Arena;
	String8 Data;
	String8 FileToCompile;
	
	u64 CurrentOffset;

	u32 ErrorCount;
	u32 RowNumber;
	u32 ColumnNumber;
	
	Token TokenRingBuffer[TOKEN_RING_BUFFER_SIZE];
	u64 CurrentTokenIndex;
	u64 LiveTokens;

	b8 InFunction;
};

struct ASTNode;
struct TypeDef;

enum NodeType
{
	Node_IntegerLiteral,
	Node_FloatLiteral,
	Node_BoolLiteral,
	Node_Binary,
	Node_Lookup,
	Node_VariableDeclaration,
	Node_Assignment,
	Node_Function,
	Node_FunctionPrototype,
	Node_FunctionCall,
	Node_Block,
	Node_Print,
	Node_Cast,
};

enum OperatorType
{
	Op_Addition = '+',
	Op_Subtraction = '-',
	Op_Multiplication = '*',
	Op_Division = '/',
	Op_None,
};

struct TypeDef
{
	TypeKind Type;
	Token Size;
	TypeDef* Base;
};

struct VarDeclaration
{
	Token Ident;
	TypeDef* Type;
};

struct VarAssignment
{
	Token Ident;
	ASTNode* Value;
};

struct PrintNode
{
	ASTNode* Expression;
};

struct BinaryNode
{
	OperatorType Operator;
	ASTNode* Left;
	ASTNode* Right;
};

struct FunctionArgument
{
	Token Ident;
	TypeDef* Type;
	TypeIndex ArgType;
	FunctionArgument* Next;
};

struct FunctionPrototype
{	
	Token Name;
	TypeDef* ReturnType;
	FunctionArgument* Args;
	u32 ArgCount;
	b32 Extern;
};

struct FunctionNode
{
	ASTNode* Prototype;
	ASTNode* Body;
};

struct FunctionCallNode
{
	Token Name;
	ASTNode* Params;
	u32 ParamCount;
};

struct BlockNode
{
	ASTNode* Stmts;
};

struct CastNode
{
	TypeDef* TargetType;
	ASTNode* Value;
	b32 Implicit;
};

struct ASTNode
{
	NodeType Type;
	TypeIndex EvalType;
	u64 StackSize;

	SymbolTable* Scope;
	
	union
	{
		Token Value;
		PrintNode Print;
		BinaryNode Binary;
		VarAssignment Assignment;
		VarDeclaration VDecl;
		FunctionNode Func;
		FunctionPrototype Proto;
		FunctionCallNode FCall;
		BlockNode Block;
		CastNode Cast;
	};

	ASTNode* Next;
};

#endif // Header guard
