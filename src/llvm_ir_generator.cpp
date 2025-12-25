
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

internal LLVMTypeRef GetLLVMType(LLVMIRGen* llvm, TypeIndex type)
{
	LLVMTypeRef result = 0;
	TypeKind kind = llvm->Types[type].Type;
	if(kind == Type_Int)
		result = llvm->IntType;
	else if(kind == Type_Float)
		result = llvm->FloatType;
	return result;
}

internal LLVMValueRef GenIR(LLVMIRGen* llvm, ASTNode* node)
{
	LLVMValueRef result = {0};
	
	switch(node->Type)
	{
		case(Node_IntegerLiteral):
		{
			result = LLVMConstInt(llvm->IntType, node->Value.IntValue, true);
		}break;

		case(Node_FloatLiteral):
		{			
			result = LLVMConstReal(llvm->FloatType, node->Value.FloatValue);
		}break;
		
		case(Node_Binary):
		{
			LLVMValueRef left = GenIR(llvm, node->Binary.Left);
			LLVMValueRef right = GenIR(llvm, node->Binary.Right);
			
			if(llvm->Types[node->Binary.Left->EvalType].Type == Type_Int)
			{
				switch(node->Binary.Operator)
				{
					case(Op_Addition):
					{
						result = LLVMBuildAdd(llvm->Builder, left, right, "addtmp");
					}break;
				
					case(Op_Multiplication):
					{
						result = LLVMBuildMul(llvm->Builder, left, right, "multmp");
					}break;
				
					case(Op_Subtraction):
					{
						result = LLVMBuildSub(llvm->Builder, left, right, "subtmp");
					}break;
									
					case(Op_Division):
					{
						result = LLVMBuildSDiv(llvm->Builder, left, right, "divtemp");
					}break;

					default:
					{
						Unhandled();
						Assert(0);
					}break;
				}				
			}
			else
			{
				switch(node->Binary.Operator)
				{
					case(Op_Addition):
					{
						result = LLVMBuildFAdd(llvm->Builder, left, right, "faddtmp");
					}break;
				
					case(Op_Multiplication):
					{
						result = LLVMBuildFMul(llvm->Builder, left, right, "fmultmp");
					}break;
				
					case(Op_Subtraction):
					{
						result = LLVMBuildFSub(llvm->Builder, left, right, "fsubtmp");
					}break;
									
					case(Op_Division):
					{
						result = LLVMBuildFDiv(llvm->Builder, left, right, "fdivtemp");
					}break;

					default:
					{
						Unhandled();
						Assert(0);
					}break;
				}
			}			
		}break;

		case(Node_Block):
		{			
			for(ASTNode* n = node->Block.Stmts; n != 0; n = n->Next)
			{
				GenIR(llvm, n);
			}
		}break;
		
		case(Node_Lookup):
		{
			// TODO(afb) :: Create names before so I dont have to allocate all the time
			TempArena temp = GetScratch(llvm->Arena);
			const char* name = ToCString(temp.Arena, node->Value.Lexeme);
			LLVMValueRef value = GetValue(node->Scope, node->Value.Lexeme);
			LLVMTypeRef type = GetLLVMType(llvm, node->EvalType);
			result = LLVMBuildLoad2(llvm->Builder, type, value, name);
			ReleaseScratch(temp);
		}break;

		case(Node_VariableDeclaration):
		{
			if(llvm->InFunction)
			{
				TempArena temp = GetScratch(llvm->Arena);
				const char* name = ToCString(temp.Arena, node->VDecl.Ident.Lexeme);
				
				Symbol* sym = GetSymbol(node->Scope, node->VDecl.Ident.Lexeme);
				Assert(sym);
				
				LLVMTypeRef type = GetLLVMType(llvm, sym->Type);
				LLVMValueRef value = LLVMBuildAlloca(llvm->Builder, type, name);
				sym->Value = value;
				
				ReleaseScratch(temp);
			}
			else
			{
				Unhandled();
				Assert(0);
			}
		}break;

		
		case(Node_FunctionPrototype):
		{
			TempArena temp = GetScratch(llvm->Arena);
			
			i32 arg_count = node->Proto.ArgCount;
			LLVMTypeRef* arg_types = PushArray(temp.Arena, LLVMTypeRef, arg_count);
			char** arg_names = PushArray(temp.Arena, char*, arg_count);

			FunctionArgument* arg = node->Proto.Args;
			for(i32 i = 0; i < arg_count; i++)
			{
				TypeIndex type_index = arg->ArgType;
				// TODO(afb)
				// arg_types[i] = ConvertToLLVMType(llvm, type_index);
				arg_types[i] = llvm->FloatType;
				arg_names[i] = ToCString(temp.Arena, arg->Ident.Lexeme);
				arg = arg->Next;
			}
			
			// TODO return_type = ConvertToLLVMType(llvm, type_index);
			LLVMTypeRef fn_type = LLVMFunctionType(llvm->FloatType, arg_types,
													 arg_count, 0);
			
			char* fn_name = ToCString(temp.Arena, node->Proto.Name.Lexeme);
			LLVMValueRef fn = LLVMAddFunction(llvm->Module, fn_name, fn_type);
			
			arg = node->Proto.Args;
			for(i32 i = 0; i < arg_count; i++)
			{
				LLVMValueRef arg_value = LLVMGetParam(fn, i);
				LLVMSetValueName2(arg_value, (const char*)arg->Ident.Lexeme.Str,
								  arg->Ident.Lexeme.Length);
				arg = arg->Next;
			}

			result = fn;
			ReleaseScratch(temp);
		}break;

		case(Node_Function):
		{
			ASTNode* proto = node->Func.Prototype;
			const char* fn_name = (const char*)proto->Proto.Name.Lexeme.Str;
			u64 fn_name_length = proto->Proto.Name.Lexeme.Length;
			LLVMValueRef fn = LLVMGetNamedFunctionWithLength(llvm->Module, fn_name,
															 fn_name_length);

			if (!fn)
				fn = GenIR(llvm, proto);

			LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(llvm->Context, fn, "entry");
			LLVMPositionBuilderAtEnd(llvm->Builder, entry);

			// Populate symbol table with parameters
			u64 arg_count = LLVMCountParams(fn);  // better than assuming arg_count
			for (u64 i = 0; i < arg_count; i++) {
				LLVMValueRef param = LLVMGetParam(fn, i);

				LLVMTypeRef param_type = LLVMTypeOf(param);

				// Build alloca for this parameter (name it after the param for clarity)
				const char* param_name = LLVMGetValueName(param);  // reuse the name from prototype
				LLVMValueRef alloca = LLVMBuildAlloca(llvm->Builder, param_type, param_name);

				// Store incoming param into alloca
				LLVMBuildStore(llvm->Builder, param, alloca);

				// Bind the variable name to this alloca in your symbol table
				// (so later lookups for this variable use the alloca)
				// bind(param_name, alloca);  //
				SetValue(node->Scope, Str8Copy(llvm->Arena, param_name), alloca);
			}

			llvm->InFunction = true;
			GenIR(llvm, node->Func.Body);
			llvm->InFunction = false;
			
#if 0
			// Return (adjust based on return type)
			LLVMTypeRef ret_type = LLVMGetReturnType(LLVMTypeOf(fn));  // or from prototype
			if (LLVMGetTypeKind(ret_type) == LLVMVoidTypeKind) {
				LLVMBuildRetVoid(llvm->Builder);
			} else {
				// Must return a value of ret_type
				LLVMValueRef return_val = LLVMConstReal(llvm->FloatType, 0.0); /* your computed value, e.g. last expression */;
				LLVMBuildRet(llvm->Builder, return_val);
			}
#else			
			LLVMValueRef return_val = LLVMConstReal(llvm->FloatType, 69.0); /* your computed value, e.g. last expression */;
			LLVMBuildRet(llvm->Builder, return_val);
#endif
			
			// Verify
			result = fn;
			if(LLVMVerifyFunction(fn, LLVMPrintMessageAction))
			{
				LLVMDeleteFunction(fn);
				result = NULL;
			}
		}break;

		case(Node_Assignment):
		{				
			LLVMValueRef value = GenIR(llvm, node->Assignment.Value);
			SetValue(node->Scope, node->Assignment.Ident.Lexeme, value);
			result = value;
		}break;

		case(Node_Cast):
		{
			if(node->Cast.Implicit)
			{
				LLVMValueRef value = GenIR(llvm, node->Cast.Value);
				if(llvm->Types[node->EvalType].Type == Type_Int)
				{
					LLVMTypeRef target_type = GetLLVMType(llvm, node->EvalType);					
					result = LLVMBuildFPToSI(llvm->Builder, value,
											 llvm->IntType, "imp_float_to_int");
				}
				else
				{
					result = LLVMBuildSIToFP(llvm->Builder, value,
											 llvm->FloatType, "imp_int_to_float");
				}
			}
			else
			{
				Unhandled();
				Assert(0);
			}
		}break;
				
		case(Node_FunctionCall):
		{
			FunctionCallNode* fn = &node->FCall;
			
		}break;

#if 0		
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
			Unhandled();
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

internal void LLVMInit(LLVMIRGen* llvm, M_Arena* arena)
{
	llvm->Arena = arena;
	llvm->Context = LLVMContextCreate();
    llvm->Module = LLVMModuleCreateWithNameInContext("arc", llvm->Context);
    llvm->Builder = LLVMCreateBuilderInContext(llvm->Context);

	llvm->IntType = LLVMInt64TypeInContext(llvm->Context);
	llvm->FloatType = LLVMDoubleTypeInContext(llvm->Context);
}
