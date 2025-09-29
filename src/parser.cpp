
internal void EatWhitespace(Parser* parser)
{
	u8 current = parser->Data.Str[parser->CurrentOffset];
	while(((current == ' ') || (current == '\n') || current == '\r') ||
		  (current == '\t'))
	{
		u8 current = parser->Data.Str[++parser->CurrentOffset];
	}
}

internal TokenType GetKeywordOrIdentifier(String8 lexeme)
{
	TokenType result = Token_Identifier;
	
	if(lexeme == "print") result = Token_Print;
	return result;
}

internal const char* GetTokenTypeString(Token t)
{
	if(t.Type == '(') return "Open Paren";
	if(t.Type == ')') return "Close Paren";
	if(t.Type == '+') return "Plus";
	if(t.Type == '-') return "Minus";
	if(t.Type == '*') return "Star";
	if(t.Type == '/') return "Slash";
	if(t.Type == ':') return "Colon";
	if(t.Type == ';') return "Semi Colon";
	if(t.Type == '=') return "Equal";

	return "";
}

internal Token ParseToken(Parser* parser)
{
	Token result = {};
	EatWhitespace(parser);
	
	u8 current = parser->Data.Str[parser->CurrentOffset];
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
		case('\0'):
		{			
			result.Type = (TokenType)current;
			result.Lexeme = Substr8(parser->Data, parser->CurrentOffset, 1);
			parser->CurrentOffset++;
		}break;		
		
		default:
		{
			if(IsDigit(current))
			{
				u64 start = parser->CurrentOffset;
				while(IsDigit(current))
				{
					current = parser->Data.Str[++parser->CurrentOffset];
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
					current = parser->Data.Str[++parser->CurrentOffset];
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

internal void Parse(Parser* parser)
{
	for(;;)
	{
		Token t = NextToken(parser);		
		LogInfoF(0, "%S of type %s\n", t.Lexeme, GetTokenTypeString(t));
		if(t.Type == Token_EOF)
			break;
	}
}
