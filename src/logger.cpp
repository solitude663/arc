#define MAX_ERRORS 16
global int ErrorCounter;

#include <stdio.h>

#define ParserError(parser, token, message, ...) do{					\
		(parser)->ErrorCount++;											\
		fprintf(stderr, "%.*s: line %d:%d [parser] ", Str8Print((parser)->FileToCompile), (token).RowNumber+1, (token).ColumnNumber+1); \
		fprintf(stderr, (message), ##__VA_ARGS__);						\
		fprintf(stderr, "\n");											\
	}while(0)

#define TypeCheckerError(tc, row, col, message, ...) do{				\
		(tc)->ErrorCount++;											\
		fprintf(stderr, "%d:%d [typer] ", row, col); \
		fprintf(stderr, (message), ##__VA_ARGS__);						\
		fprintf(stderr, "\n");											\
	}while(0)


internal void LogError_(String8 message)
{
	ErrorCounter++;

	fprintf(stderr, "%.*s\n", Str8Print(message));
	
	if(ErrorCounter > MAX_ERRORS)
	{
		exit(1);
	}
}

internal void LogParserError(String8 location, String8 error_msg)
{
	char error_msg_buffer[4096] = {0};
	snprintf(error_msg_buffer, ArrayCount(error_msg_buffer), "%.*s: [%s] %.*s",
			   Str8Print(location), "parser", Str8Print(error_msg));
	LogError_(error_msg_buffer);
}
