#include <base_inc.h>
#include <base_inc.cpp>

#include "./parser.h"
#include "./parser.cpp"

internal void MainEntry(i32 argc, char** argv)
{
	if(argc < 2)
	{
		LogPanicF(0, "usage %s <file_to_compile>", argv[1]);
	}

	M_Arena* arena = ArenaAlloc(MB(128));
	String8 file_to_compile = Str8C(argv[1]);
	String8 file_contents = OS_FileReadAll(arena, file_to_compile);
	
	printf("File: %.*s\n%.*s\n", Str8Print(file_to_compile), Str8Print(file_contents));

	Parser parser = {0};
	parser.Arena = arena;
	parser.Data = file_contents;
	Parse(&parser);
	
}

int main(int argc, char** argv)
{
	BaseMainThreadEntry(MainEntry, argc, argv);
	return 0;
}
