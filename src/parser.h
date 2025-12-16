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

	Token_OpenBrace = '{',
	Token_CloseBrace = '}',
	
	Token_EOF = '\0',
	Token_Invalid = 404,	
	
	Token_IntegerLiteral = 300,

	Token_ColonColon,
	
	Token_Identifier,
	Token_Print,
	Token_Int,
	Token_Bool,
	Token_True,
	Token_False,

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
};

struct ASTNode;
struct TypeDef;

enum NodeType
{
	Node_IntegerLiteral,
	Node_BoolLiteral,
	Node_Binary,
	Node_Lookup,
	Node_VariableDeclaration,
	Node_Assignment,
	Node_Function,
	Node_FunctionCall,
	Node_Block,
	Node_Print,
};

enum OperatorType
{
	Op_Addition,
	Op_Subtraction,
	Op_Multiplication,
	Op_Division,
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

struct FunctionNode
{
	Token Name;
	TypeDef* ReturnType;
	FunctionArgument* Args;
	ASTNode* Body;
	u32 ArgCount;
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

struct ASTNode
{
	NodeType Type;
	TypeIndex EvalType;
	u64 StackSize;
	
	union
	{
		Token Value;
		PrintNode Print;
		BinaryNode Binary;
		VarAssignment Assignment;
		VarDeclaration VDecl;
		FunctionNode Func;
		FunctionCallNode FCall;
		BlockNode Block;
	};

	ASTNode* Next;
};

#endif // Header guard
