#include <base_inc.h>
#include <base_inc.cpp>

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>


#include "./logger.cpp"

#include "./parser.h"
#include "./parser.cpp"

#include "./type_checker.h"
#include "./type_checker.cpp"

#include "./llvm_ir_generator.h"
#include "./llvm_ir_generator.cpp"

// #include "./code_generator.h"
// #include "./code_generator.cpp"


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

		case(Node_FloatLiteral):
		{
			printf("FLOAT_LIT(%.*s)\n", Str8Print(node->Value.Lexeme));
		}break;

		case(Node_BoolLiteral):
		{
			printf("BOOL_LIT(%.*s)\n", Str8Print(node->Value.Lexeme));
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
			printf("FUNC_DECL\n");
			
			PrintASTNode(node->Func.Prototype, level + 1);
			PrintASTNode(node->Func.Body, level + 1);
		}break;

		case(Node_FunctionPrototype):
		{
			printf("%s(%.*s)\n", node->Proto.Extern ? "EXTERN" : "PROTO",
				   Str8Print(node->Proto.Name.Lexeme));
		}break;
		
		case(Node_Block):
		{
			printf("SCOPE_START\n");
			PrintAST(node->Block.Stmts, level + 1);
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

		case(Node_Assignment):
		{
			printf("VAR_ASSIGN(%.*s)\n", Str8Print(node->Assignment.Ident.Lexeme));
			PrintASTNode(node->Assignment.Value, level + 1);
		}break;

		case(Node_Lookup):
		{
			printf("LOOKUP(%.*s)\n", Str8Print(node->Value.Lexeme));
		}break;

		case(Node_FunctionCall):
		{
			printf("CALL(%.*s)\n", Str8Print(node->Value.Lexeme));
			PrintAST(node->FCall.Params, level + 1);
		}break;
		
		case(Node_Cast):
		{
			printf("CAST\n");
			PrintAST(node->Cast.Value, level + 1);
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

enum BuildMode
{
	Build_AST,
	Build_TypedIR,
	Build_Executable,
	Build_None,	
};

internal void MainEntry(i32 argc, char** argv)
{
	if(argc < 2)
	{
		LogPanicF(0, "usage %s <file_to_compile>", argv[1]);
	}

	BuildMode build_mode = Build_AST;
	
	M_Arena* arena = ArenaAlloc(MB(128));
	String8 file_to_compile = Str8C(argv[1]);
	String8 file_contents = OS_FileReadAll(arena, file_to_compile);

	String8 working_directory = GetWorkingDirectory(arena);
	String8 target_full_path = OS_PathConcat(arena, working_directory, file_to_compile);

	printf("\n===== %.*s\n", Str8Print(target_full_path));
	
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
	printf("===== Parsing complete\n");

	printf("\n===== Type checking\n");
	TypeChecker type_checker = {};
	TypeCheckerInit(&type_checker);
	TypeCheck(&type_checker, program);

	if(type_checker.TreeChanged)
	{
		PrintAST(program);
	}
	
	if(type_checker.ErrorCount)
	{
		LogPanicF(0, "Type checking failed with %d error(s)", type_checker.ErrorCount);
	}
	printf("===== Type checking complete. Found %u type(s)\n", type_checker.TypeCount);
	
	LLVMIRGen llvm = {0};
	LLVMInit(&llvm, type_checker.Arena);
	llvm.Types = type_checker.Types;
	llvm.TypeCount = type_checker.TypeCount;

	LLVMGenIR(&llvm, program);

	// After your function generation is complete (e.g., after GenIR for the function)
	LLVMPrintModuleToFile(llvm.Module, "output.ll", NULL);  // Saves to file

// Or print directly to stderr (great for debugging)
	// char* s = LLVMPrintModuleToString(llvm.Module);
	// printf(s);

#if 0
		
	
   	// CodeGenerator code_gen = {0};
	// code_gen.Arena = arena;

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
