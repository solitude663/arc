
internal LLVMValueRef GenIR(LLVMIRGen* llvm, ASTNode* node);

internal LLVMValueRef GetValue(SymbolTable* scope, const String8& name)
{
	Assert(scope); // Should always be there as the type checker already checked it.
	
	for(Symbol* s = scope->Symbols; s != 0; s = s->Next)
	{
		if(s->Name == name)
		{			
			return s->Value;
		}
	}
	
	if(scope->Parent != 0)
	{
		return GetValue(scope->Parent, name);
	}

	return {0};
}

internal void SetValue(SymbolTable* scope, const String8& name, LLVMValueRef value)
{
	Assert(scope);
	
	for(Symbol* s = scope->Symbols; s != 0; s = s->Next)
	{
		if(s->Name == name)
		{			
			s->Value = value;
			return;
		}
	}

	if(scope->Parent != 0)
	{
		SetValue(scope->Parent, name, value);
	}
}

internal LLVMValueRef GenIR(LLVMIRGen* llvm, ASTNode* node)
{
	LLVMValueRef result = {0};
	
	switch(node->Type)
	{
		case(Node_IntegerLiteral):
		{
			result = LLVMConstReal(LLVMInt64TypeInContext(llvm->Context),
								   node->Value.IntValue);
		}break;

		case(Node_FloatLiteral):
		{			
			result = LLVMConstReal(LLVMDoubleTypeInContext(llvm->Context),
									node->Value.FloatValue);
		}break;
		
		case(Node_Binary):
		{
			LLVMValueRef left = GenIR(llvm, node->Binary.Left);
			LLVMValueRef right = GenIR(llvm, node->Binary.Right);
			
			switch(node->Binary.Operator)
			{
				case(Op_Addition):
				{
					result = LLVMBuildFAdd(llvm->Builder, left, right, "addtmp");
				}break;
				
				case(Op_Multiplication):
				{
					result = LLVMBuildFAdd(llvm->Builder, left, right, "multmp");
				}break;
				
				case(Op_Subtraction):
				{
					result = LLVMBuildFAdd(llvm->Builder, left, right, "subtmp");
				}break;
									
				case(Op_Division):
				{
					Assert(0);
				}break;

				default:
				{
					Assert(0);
				}break;
			}
			
		}break;

		case(Node_Lookup):
		{
			// TODO(afb) :: Create names before so I dont have to allocate all the time
			TempArena temp = GetScratch(llvm->Arena);
			char* name = ToCString(temp.Arena, node->Value.Lexeme);
			LLVMValueRef value = GetValue(node->Scope, node->Value.Lexeme);
			result = LLVMBuildLoad2(llvm->Builder, llvm->FloatType, value, name);
			ReleaseScratch(temp);
		}break;

		case(Node_VariableDeclaration):
		{
			TempArena temp = GetScratch(llvm->Arena);
			char* name = ToCString(temp.Arena, node->VDecl.Ident.Lexeme);
			LLVMValueRef new_var = LLVMBuildAlloca(llvm->Builder, llvm->IntType, name);
			SetValue(node->Scope, node->VDecl.Ident.Lexeme, new_var);
			ReleaseScratch(temp);
		}break;

#if 0
		case(Node_Assignment):
		{				
			Symbol* sym = GetSymbol(sym_table, node->Assignment.Ident.Lexeme);
			if(sym)
			{
				TypeIndex t1 = CheckNode(tc, sym_table, node->Assignment.Value);
				if(t1 != sym->Type)
				{
					TypeCheckerError(tc, 1, 1, "Type of LHS and does not match type of RHS");
				}
			}
			else
			{
				TypeCheckerError(tc, 1, 1, "Reference to undeclared variable '%.*s'",
								 Str8Print(node->Assignment.Ident.Lexeme));
			}

			result = GetTypeIndex(tc, Type_Void);
			node->Scope = sym_table;
		}break;

		case(Node_Block):
		{
			SymbolTable* table = PushStructZero(tc->Arena, SymbolTable);
			table->Parent = sym_table;			

			for(ASTNode* n = node->Block.Stmts; n != 0; n = n->Next)
			{
				CheckNode(tc, table, n);
				node->StackSize += n->StackSize;
			}

			result = GetTypeIndex(tc, Type_Void);
			node->Scope = table;
		}break;

		case(Node_Function):
		{
			SymbolTable* table = PushStructZero(tc->Arena, SymbolTable);
			table->Parent = sym_table;
			TypeIndex return_type = CheckNode(tc, table, node->Func.Prototype);

			tc->InFunction = true;
			CheckNode(tc, table, node->Func.Body); // TODO(afb) :: Return statements
			tc->InFunction = false;
			
			result = return_type;
			node->Scope = table;
		}break;

		case(Node_FunctionPrototype):
		{			
			SymbolTable* table = sym_table;
			if(!tc->InFunction)
			{
				PushStructZero(tc->Arena, SymbolTable);
				table->Parent = sym_table;
			}

			FunctionType func = {0};
			if(node->Proto.ArgCount)
			{
				func.ArgNames = PushArray(tc->Arena, String8, node->Proto.ArgCount);
				func.ArgTypes = PushArray(tc->Arena, TypeIndex, node->Proto.ArgCount);
				func.ArgCount = node->Proto.ArgCount;
			}

			u32 index = 0;
			for(FunctionArgument* arg = node->Proto.Args; arg != 0; arg = arg->Next)
			{
				TypeIndex type = ResolveType(tc, arg->Type);
				func.ArgNames[index] = arg->Ident.Lexeme;
				func.ArgTypes[index] = type;
				index++;
				
				// node->StackSize += tc->Types[type].Size;
				AddSymbol(tc->Arena, table, arg->Ident.Lexeme, type);
			}

			TypeIndex return_type;
			if(node->Proto.ReturnType)
			{
				return_type = ResolveType(tc, node->Proto.ReturnType);
			}
			else
			{
				return_type = GetTypeIndex(tc, Type_Void);
			}

			func.Name = node->Proto.Name.Lexeme;
			func.ReturnType = return_type;

			tc->Functions[tc->FunctionCount++] = func;
			result = return_type;			
		}break;
		
		case(Node_FunctionCall):
		{
			FunctionCallNode* func = &node->FCall;
			FunctionType* func_type = GetFunction(tc, func->Name.Lexeme);
			if(func_type == 0)
			{
				LogPanicF(0, "Function '%S' does not exist", func->Name.Lexeme);
			}

			node->Scope = sym_table;
		}break;
		
		case(Node_Print):
		{
			TypeIndex index = CheckNode(tc, sym_table, node->Print.Expression);
			Assert(index > -1);
			TypeKind kind = tc->Types[index].Type;
			
			if(kind != Type_Int && kind != Type_Bool)
			{
				LogPanic(0, "Print only supports int and bool currently");
			}
			result = GetTypeIndex(tc, Type_Void);
			node->Scope = sym_table;
		}break;
#endif
		
		default:
		{
			LogPanicF(0, "Error: Unhandled node in type checker %d", node->Type);
		}break;
	}

	return result;
}


internal LLVMValueRef LLVMGenIR(LLVMIRGen* llvm, ASTNode* program)
{	
	for(ASTNode* node = program; node != 0; node = node->Next)
	{
		GenIR(llvm, node);
	}

	return {0};
}

internal void LLVMInit(LLVMIRGen* llvm)
{
	llvm->Context = LLVMContextCreate();
    llvm->Module = LLVMModuleCreateWithNameInContext("arc", llvm->Context);
    llvm->Builder = LLVMCreateBuilderInContext(llvm->Context);	
}
