#ifndef PARSER_H
#define PARSER_H

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

	Token_EOF = '\0',
	Token_Invalid = 404,

	Token_IntegerLiteral = 300,

	Token_Identifier,
	Token_Print,

	Token_Count,
};

#define TOKEN_RING_BUFFER_SIZE 8

struct Token
{
	TokenType Type;
	String8 Lexeme;
};

struct Parser
{
	M_Arena* Arena;
	String8 Data;

	u64 CurrentOffset;

	Token TokenRingBuffer[TOKEN_RING_BUFFER_SIZE];
	u64 CurrentTokenIndex;
	u64 LiveTokens;
};

#endif // Header guard
