
internal i32 AllocRegister(CodeGenerator* cg)
{
	for(i32 i = 0; i < Register_Count; i++)
	{
		if(!cg->Registers[i])
		{
			cg->Registers[i] = 1;
			return i;
		}
	}

	LogPanic(0, "Error: No available registers");
	return -1;
}

internal void FreeRegister(CodeGenerator* cg, i32 reg)
{
	cg->Registers[reg] = 0;
}

internal i32 GenerateNode(CodeGenerator* cg, ASTNode* node, b8 is_local = false)
{
	i32 result = 0;
	switch(node->Type)
	{
		case(Node_VariableDeclaration):
		{
			// TODO(afb) :: Handle other types
			if(node->VDecl.Type->Type != Type_Array)
			{
				Str8ListPushF(cg->Arena, cg->DataBuilder, "\t%S resq 1",
							  node->VDecl.Ident.Lexeme);
			}
			else
			{
				Str8ListPushF(cg->Arena, cg->DataBuilder, "\t%S resq %S",
							  node->VDecl.Ident.Lexeme,
							  node->VDecl.Type->Size.Lexeme);
			}

		}break;		

		case(Node_Assignment):
		{
			i32 reg = GenerateNode(cg, node->Assignment.Value);
			Str8ListPushF(cg->Arena, cg->CodeBuilder, "\tmov [%S], %s",
						  node->VDecl.Ident.Lexeme, RegisterNames[reg]);
			FreeRegister(cg, reg);
		}break;

		case(Node_Lookup):
		{
			i32 reg = AllocRegister(cg);
			Str8ListPushF(cg->Arena, cg->CodeBuilder, "\tmov %s, qword[%S]",
						  RegisterNames[reg], node->Value.Lexeme);
			result = reg;
		}break;
		
		case(Node_IntegerLiteral):
		{
			i32 reg = AllocRegister(cg);
			Str8ListPushF(cg->Arena, cg->CodeBuilder, "\tmov %s, %S",
						  RegisterNames[reg], node->Value.Lexeme);
			result = reg;
		}break;

		case(Node_BoolLiteral):
		{
			i32 reg = AllocRegister(cg);
			Str8ListPushF(cg->Arena, cg->CodeBuilder, "\tmov %s, %s",
						  RegisterNames[reg],
						  node->Value.Lexeme.Str[0] == 't' ? "1" : "0");
			result = reg;
		}break;
		
		case(Node_Binary):
		{
			i32 reg1 = GenerateNode(cg, node->Binary.Left);
			i32 reg2 = GenerateNode(cg, node->Binary.Right);
			
			switch(node->Binary.Operator)
			{
				case(Op_Addition):
				{
					Str8ListPushF(cg->Arena, cg->CodeBuilder, "\tadd %s, %s",
								  RegisterNames[reg1], RegisterNames[reg2]);
					FreeRegister(cg, reg2);
					result = reg1;
				}break;

				case(Op_Subtraction):
				{
					Str8ListPushF(cg->Arena, cg->CodeBuilder, "\tsub %s, %s",
								  RegisterNames[reg1], RegisterNames[reg2]);
					FreeRegister(cg, reg2);
					result = reg1;
				}break;

				case(Op_Multiplication):
				{
					Str8ListPushF(cg->Arena, cg->CodeBuilder, "\timul %s, %s",
								  RegisterNames[reg1], RegisterNames[reg2]);
					FreeRegister(cg, reg2);
					result = reg1;
				}break;

				default:
				{
					LogPanic(0, "Error: CodeGen Unhandled operator");
				}
			}
		}break;

		case(Node_Function):
		{
#define AlignForward(x, align) (((x) + (align) - 1) & ~((align) - 1))

			if(!(node->Func.Name.Lexeme == "main"))
			{				
				i32 alloc_amount = AlignForward(node->StackSize, 16);
				Str8ListPushF(cg->Arena, cg->CodeBuilder,
							  "%S:\n"
							  "\tpush rbp\n"
							  "\tmov rbp, rsp\n"
							  "\tsub rsp, %d",
							  node->Func.Name.Lexeme, alloc_amount);
			
				Str8ListPushF(cg->Arena, cg->CodeBuilder,
							  "\tadd rsp, %d\n"
							  "\tpop rbp\n"
							  "\tret",
							  alloc_amount);
			}
			else
			{
				Str8ListPushF(cg->Arena, cg->CodeBuilder,
							  "main:\n"
							  "\tsub rsp, 16\n\n"
							  "\tmov rax, 60 ; sys_exit\n"
							  "\tmov rdi, 0\n"
							  "\tsyscall\n",
							  node->Func.Name.Lexeme);

			}
		}break;
		
		case(Node_Print):
		{
			i32 reg = GenerateNode(cg, node->Print.Expression);

			Str8ListPushF(cg->Arena, cg->CodeBuilder,
						  "\tmov rsi, %s\n"
						  "\tlea rdi, [int_format]\n"
						  "\txor rax, rax\n"
						  "\tcall printf\n",
						  RegisterNames[reg]);
			FreeRegister(cg, reg);
		}break;

		default:
		{
			LogPanicF(0, "Code Generation: Unknown node type %d", node->Type);
		};
	}
	return result;
}

internal String8 GenerateAssembly(M_Arena* arena, ASTNode* program)
{
	CodeGenerator cg = {0};
	cg.Arena = arena;
	String8List code_builder = {0};
	String8List data_builder = {0};

	cg.CodeBuilder = &code_builder;
	cg.DataBuilder = &data_builder;

	Str8ListPush(arena, cg.DataBuilder,
				 "\nsection .bss");
	
	Str8ListPush(arena, cg.CodeBuilder,
				 "BITS 64\n"
				 "CPU X64\n\n"
				 "section .text\n"
				 "\textern printf\n"
				 "\tglobal main");/*\n"
								   "main:");*/
	for(ASTNode* node = program; node != 0; node = node->Next)
	{
		GenerateNode(&cg, node);
	}

	Str8ListPush(arena, cg.CodeBuilder,
				 "\nsection .data\n"
				 "\tint_format db \"%d\", 10, 0");

	String8 data = Str8Join(arena, data_builder, "\n");

	Str8ListPush(arena, cg.CodeBuilder, data);
	String8 result = Str8Join(arena, code_builder, "\n");
	return result;
}
