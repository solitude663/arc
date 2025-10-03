inline u8 CurrentChar(Parser* parser)
{
	u8 result = 0;
	if(parser->CurrentOffset < parser->Data.Length)
		result = parser->Data.Str[parser->CurrentOffset];
	return result;
}

internal void EatWhitespace(Parser* parser)
{
	u8 current = CurrentChar(parser);
	while(((current == ' ') ||
		   (current == '\n') ||
		   (current == '\r') ||
		   (current == '\t')))
	{
		parser->CurrentOffset++;
		current = CurrentChar(parser);
	}
}

internal TokenType GetKeywordOrIdentifier(const String8& lexeme)
{
	TokenType result = Token_Identifier;
	
	if(lexeme == "print") result = Token_Print;
	if(lexeme == "int") result = Token_Int;
	if(lexeme == "bool") result = Token_Bool;
	if(lexeme == "true") result = Token_True;
	if(lexeme == "false") result = Token_False;
	return result;
}

internal const char* GetTokenTypeString(TokenType type)
{
	if(type == '\0') return "EOF";
	if(type == '(') return "Open Paren";
	if(type == ')') return "Close Paren";
	if(type == '+') return "Plus";
	if(type == '-') return "Minus";
	if(type == '*') return "Star";
	if(type == '/') return "Slash";
	if(type == ':') return "Colon";
	if(type == ';') return "Semi Colon";
	if(type == '=') return "Equal";

	if(type == Token_Invalid) return "<Invalid>";
	if(type == Token_Identifier) return "<identifier>";
	if(type == Token_IntegerLiteral) return "<int>";
	if(type == Token_Print) return "Print";
	if(type == Token_Invalid) return "Token_Invalid";

	return "(null)";
}

internal Token ParseToken(Parser* parser)
{
	Token result = {};	
	EatWhitespace(parser);

	u8 current = CurrentChar(parser);
	switch(current)
	{
		case('('):
		case(')'):
		case('+'):
		case('*'):
		case('-'):
		case('/'):			
		case(':'):
		case(';'):
		case('='):
		case('['):
		case(']'):
		{			
			result.Type = (TokenType)current;
			result.Lexeme = Substr8(parser->Data, parser->CurrentOffset, 1);
			parser->CurrentOffset++;
		}break;		
		
		case(0):
		{
			result.Type = Token_EOF;
			result.Lexeme = Str8CLit("EOF");
			parser->CurrentOffset++;
		}break;
		
		default:
		{
			if(IsDigit(current))
			{
				u64 start = parser->CurrentOffset;
				while(IsDigit(current))
				{
					parser->CurrentOffset++;
					current = CurrentChar(parser);
				}
				u64 end = parser->CurrentOffset;
				
				result.Type = Token_IntegerLiteral;
				result.Lexeme = Substr8(parser->Data, start, end - start);
			}
			else if(IsAlpha(current) || current == '_')
			{
				u64 start = parser->CurrentOffset;				
				while((IsDigit(current) || IsAlpha(current) || current == '_'))
				{
					parser->CurrentOffset++;
					current = CurrentChar(parser);
				}				
				u64 end = parser->CurrentOffset;

				result.Lexeme = Substr8(parser->Data, start, end - start);
				result.Type = GetKeywordOrIdentifier(result.Lexeme);
			}
			else
			{
				result.Type = Token_Invalid;
				result.Lexeme = Substr8(parser->Data, parser->CurrentOffset, 1);
				parser->CurrentOffset++;
			}
		}
	}
	
	return result;
}


internal Token PeekToken(Parser* parser, i32 depth = 0)
{
	/* NOTE(afb) ::
	 * Should not request deeper than the buffer can hold. It will eat tokens that 
	 * were not used yet used. */
	Assert(depth < TOKEN_RING_BUFFER_SIZE);
	
	i32 tokens_processed = 0;
	i32 tokens_to_process = (depth+1) - parser->LiveTokens;
	for(i32 i = 0; i < tokens_to_process; i++)
	{
		Token token = ParseToken(parser);
		u32 offset = parser->CurrentTokenIndex + parser->LiveTokens + i;
		u32 index = offset & (TOKEN_RING_BUFFER_SIZE - 1);
		// u32 index = (lexer->CurrentTokenIndex + lexer->LiveTokens + i) % TOKEN_RING_BUFFER_SIZE;
		parser->TokenRingBuffer[index] = token;
		tokens_processed++;
	}
	// NOTE(afb) ::If you ask for a token futhers than what is already parsed then	
	// adjust the amount of LiveTokens accordingly, if not then don't.
	// lexer->LiveTokens += ((depth+1) > lexer->LiveTokens) ? ((depth+1) - lexer->LiveTokens) : 0;
	parser->LiveTokens += tokens_processed;
	
	u32 index = (parser->CurrentTokenIndex + depth) & (TOKEN_RING_BUFFER_SIZE - 1);
	Token result = parser->TokenRingBuffer[index];
	return result;
}


internal void AdvanceToken(Parser* parser, u32 distance = 1)
{
	for(u32 i = 0; i < distance; i++)
	{
		parser->CurrentTokenIndex = (parser->CurrentTokenIndex+1) % TOKEN_RING_BUFFER_SIZE;
		parser->LiveTokens--;
	}
}

internal Token NextToken(Parser* parser)
{	
	Token result = PeekToken(parser, 0);	
	AdvanceToken(parser);
	return result;
}

internal ASTNode* CreateASTNode(M_Arena* arena, NodeType type)
{
	ASTNode* result = PushStructZero(arena, ASTNode);
	result->Type = type;
	return result;
}

internal Token MatchToken(Parser* parser, TokenType type)
{
	Token result = PeekToken(parser, 0);
	if(result.Type != type)
	{
		// TODO(afb) :: Report error
		LogErrorF(0, "Match error: Expected %s but recieved %s",
				  GetTokenTypeString(type), GetTokenTypeString(result.Type));
		result.Type = type;
	}
	AdvanceToken(parser);
	return result;
}

internal b8 IsBinaryOp(TokenType type)
{
	b8 result = (type == '+') || (type == '-') || (type == '*') || (type == '/');
	return result;
}

internal i32 OperatorPrecedence(TokenType type)
{
	switch(type)
	{
		// case Token_EQ:
		// case Token_NE:
		// 	return 0;
		// case Token_GT:
		// case Token_GE:
		// case Token_LT:
		// case Token_LE:
		// 	return 10;
		case '+':
		case '-':
			return 20;
		case '*':
		case '/':
		case '%':
			return 30;
		default:
			return 0;
	}
}

internal b8 IsLeftAssociative(TokenType type)
{
	return ((type == '+') || (type == '-') || (type == '*') || (type == '/'));/* ||
			(type == Token_GE) || (type == Token_GT) || (type == Token_LE) || (type == Token_LT) ||
			(type == Token_EQ) || (type == Token_NE));*/
}

inline OperatorType GetOperatorFromToken(TokenType type)
{
	if(type == Token_Plus) return Op_Addition;
	if(type == Token_Minus) return Op_Subtraction;
	if(type == Token_Star) return Op_Multiplication;
	if(type == Token_Slash) return Op_Division;
	return Op_None;
}

internal ASTNode* ParsePrimary(Parser* parser)
{
	ASTNode* result = 0;	

	Token token = PeekToken(parser);
	if(token.Type == Token_IntegerLiteral)
	{
		AdvanceToken(parser);
		result = CreateASTNode(parser->Arena, Node_IntegerLiteral);
		result->Value = token;
	}
	else if(token.Type == Token_True || token.Type == Token_False)
	{
		AdvanceToken(parser);
		result = CreateASTNode(parser->Arena, Node_BoolLiteral);
		result->Value = token;		
	}
	else if(token.Type == Token_Identifier)
	{
		AdvanceToken(parser);
		result = CreateASTNode(parser->Arena, Node_Lookup);
		result->Value = token;
	}
	else
	{
		LogPanicF(0, "Error: Unhandled token <%s> in ParsePrimary",
				  GetTokenTypeString(token.Type));
	}

	return result;
}


internal ASTNode* ParseBinaryExpression(Parser* parser, i32 min_precedence = 0)
{
	ASTNode* left = ParsePrimary(parser);

	for(;;)
	{
		Token op = PeekToken(parser);
		if(!IsBinaryOp(op.Type)) break;

		i32 precedence = OperatorPrecedence(op.Type);
		if(precedence < min_precedence)
			break;
		
		b8 left_associative = IsLeftAssociative(op.Type);
		i32 new_min_precedence = left_associative ? precedence + 1 : precedence;

		AdvanceToken(parser);
		
		ASTNode* right = ParseBinaryExpression(parser, new_min_precedence);
		ASTNode* temp = CreateASTNode(parser->Arena, Node_Binary);
		temp->Binary.Operator = GetOperatorFromToken(op.Type);
		temp->Binary.Left = left;
		temp->Binary.Right = right;
		left = temp;
	}

	return left;
}

internal ASTNode* ParseExpression(Parser* parser)
{
	return ParseBinaryExpression(parser);
}

internal TypeDef* ParseType(Parser* parser)
{
	TypeDef* result = 0;
	
	Token tk = PeekToken(parser);
	if(tk.Type == Token_Int)
	{
		AdvanceToken(parser);
		result = PushStructZero(parser->Arena, TypeDef);
		result->Type = Type_Int;
	}
	if(tk.Type == Token_Bool)
	{
		AdvanceToken(parser);
		result = PushStructZero(parser->Arena, TypeDef);
		result->Type = Type_Bool;
	}
	else if(tk.Type == Token_Star) // Poitners
	{
		AdvanceToken(parser);
		result = PushStructZero(parser->Arena, TypeDef);
		result->Type = Type_Pointer;
		result->Base = ParseType(parser);
	}
	else if(tk.Type == Token_OpenBracket) // Arrays
	{
		AdvanceToken(parser);
		result = PushStructZero(parser->Arena, TypeDef);
		result->Type = Type_Array;
		Token size = MatchToken(parser, Token_IntegerLiteral);
		result->Size = size;
		MatchToken(parser, Token_CloseBracket);
		result->Base = ParseType(parser);		
	}

	return result;
}

internal ASTNode* ParseStatement(Parser* parser)
{
	ASTNode* result;
	Token current_token = PeekToken(parser);
	switch(current_token.Type)
	{
		case(Token_Identifier):
		{
			Token next_token = PeekToken(parser, 1);
			
			switch(next_token.Type)
			{
				case(Token_Colon):
				{
					AdvanceToken(parser, 2);
					TypeDef* type = ParseType(parser);
					// TODO(afb) :: Ensure type is valid
					MatchToken(parser, Token_SemiColon);
					
					result = CreateASTNode(parser->Arena, Node_VariableDeclaration);
					result->VDecl.Ident = current_token;
					result->VDecl.Type  = type;
				}break;

				case(Token_Equal):
				{
					AdvanceToken(parser, 2);
					ASTNode* expr = ParseExpression(parser);
					MatchToken(parser, Token_SemiColon);

					result = CreateASTNode(parser->Arena, Node_Assignment);
					result->Assignment.Ident = current_token;
					result->Assignment.Value = expr;
				}break;
				
				default:
				{
					result = ParseExpression(parser);
					MatchToken(parser, Token_SemiColon);
				}break;
			}
			
		}break;
		
		case(Token_Print):
		{
			AdvanceToken(parser);
			MatchToken(parser, Token_OpenParen);
			ASTNode* expr = ParseExpression(parser);
			MatchToken(parser, Token_CloseParen);
			MatchToken(parser, Token_SemiColon);
			
			result = CreateASTNode(parser->Arena, Node_Print);
			result->Print.Expression = expr;
			
		}break;
		
		default:
		{
			result = ParseExpression(parser);
			MatchToken(parser, Token_SemiColon);
		}
	}
	return result;
}

internal ASTNode* Parse(Parser* parser)
{
	ASTNode* result = 0;
	ASTNode* last_node = 0;
	for(;;)
	{
#if 1
		ASTNode* node = ParseStatement(parser);
		if(result)
		{
			last_node->Next = node;
			last_node = node;
		}
		else
		{
			result = last_node = node;
		}

		if(PeekToken(parser).Type == Token_EOF) break;
#else
		Token t = NextToken(parser);
		LogInfoF(0, "Lexeme: %S of Type: %s", t.Lexeme, GetTokenTypeString(t));
		if(t.Type == Token_EOF)
			break;
#endif
	}
	return result;
}
