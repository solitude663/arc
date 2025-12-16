#include <base_inc.h>
#include <base_inc.cpp>

#include "./logger.cpp"

#include "./parser.h"
#include "./parser.cpp"

#include "./code_generator.h"
#include "./code_generator.cpp"

#include "./type_checker.h"
#include "./type_checker.cpp"


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
	ASTNode* program = Parse(&parser);

	if(parser.ErrorCount)
	{
		LogPanicF(0, "Parsing failed with %d errors", parser.ErrorCount);
	}
	// CodeGenerator code_gen = {0};
	// code_gen.Arena = arena;

#if 0
	TypeChecker type_checker = {};
	TypeCheckerInit(&type_checker);
	TypeCheck(&type_checker, program);
   
	String8 output = GenerateAssembly(arena, program);
	String8 output_filename = "./out.nasm";
	OS_Handle handle = OS_FileOpen(output_filename,
								   OS_AccessFlag_CreateNew | OS_AccessFlag_Read | 
								   OS_AccessFlag_Write);

	if(OS_FileIsValid(handle))
	{
		OS_FileWrite(handle, output);
		OS_FileClose(handle);
		LogInfoF(0, "Output %S", output_filename);
	}
#endif
}

int main(int argc, char** argv)
{
	BaseMainThreadEntry(MainEntry, argc, argv);
	return 0;
}
