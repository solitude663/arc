#include <base_inc.h>
#include <base_inc.cpp>

#include "./logger.cpp"

#include "./parser.h"
#include "./parser.cpp"

#include "./code_generator.h"
#include "./code_generator.cpp"

#include "./type_checker.h"
#include "./type_checker.cpp"

internal void PrintASTNode(ASTNode* node, i32 level);
internal void PrintAST(ASTNode* node, i32 level);
	
internal void PrintASTNode(ASTNode* node, i32 level = 0)
{
	for(i32 i = 0; i < level; i++)
	{
		printf("   |");
	}		
	
	switch(node->Type)
	{
		case(Node_IntegerLiteral):
		{
			printf("INT_LIT(%.*s)\n", Str8Print(node->Value.Lexeme));
		}break;

		case(Node_Binary):
		{
			// TODO(afb) :: ImplementOpTypeToString
			printf("Operator(%c)\n", (char)node->Binary.Operator);
			PrintASTNode(node->Binary.Left, level + 1);
			PrintASTNode(node->Binary.Right, level + 1);
		}break;

		case(Node_VariableDeclaration):
		{
			printf("VAR_DECL(%.*s)\n", Str8Print(node->VDecl.Ident.Lexeme));
		}break;

		case(Node_Function):
		{
			printf("FUNC_DECL(%.*s)\n", Str8Print(node->Func.Name.Lexeme));
			PrintASTNode(node->Func.Body, level + 1);
			/*
			  TypeDef* ReturnType;
			  FunctionArgument* Args;
			  u32 ArgCount;
			*/
		}break;

		case(Node_Block):
		{
			printf("SCOPE_START\n");
			PrintAST(node->Block.Stmts, 0);
			for(i32 i = 0; i < level; i++)
			{
				printf("   |");
			}		
			printf("SCOPE_END\n");
		}break;

		case(Node_Print):			
		{
			printf("PRINT\n");
			PrintASTNode(node->Print.Expression, level + 1);
		}break;
		
		default:
		{
			LogPanic(0, "DIDIDIDID");
		}break;
	}	
}

internal void PrintAST(ASTNode* node, i32 level = 0)
{
	for(ASTNode* current_node = node; current_node != 0; current_node = current_node->Next)
	{
		PrintASTNode(current_node, level);
	}
}


internal void MainEntry(i32 argc, char** argv)
{
	if(argc < 2)
	{
		LogPanicF(0, "usage %s <file_to_compile>", argv[1]);
	}

	M_Arena* arena = ArenaAlloc(MB(128));
	String8 file_to_compile = Str8C(argv[1]);
	String8 file_contents = OS_FileReadAll(arena, file_to_compile);

	String8 working_directory = GetWorkingDirectory(arena);
	String8 target_full_path = OS_PathConcat(arena, working_directory, file_to_compile);

	printf("\n===== %.*s\n%.*s\n",
		   Str8Print(target_full_path),
		   Str8Print(file_contents));

	Parser parser = {0};
	parser.Arena = arena;
	parser.Data = file_contents;
	parser.FileToCompile = target_full_path;
	ASTNode* program = Parse(&parser);

	PrintAST(program);

	if(parser.ErrorCount)
	{
		LogPanicF(0, "Parsing failed with %d error(s)", parser.ErrorCount);
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
