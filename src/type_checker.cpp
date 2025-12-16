// internal Symbol* GetSymbol(SymbolTable* sym_table, const String8& name, b32 check_parent);

internal TypeIndex GetTypeIndex(TypeChecker* tc, TypeKind type)
{
	TypeIndex result = -1;
	for(u32 i = 0; i < tc->TypeCount; i++)
	{
		if(tc->Types[i].Type == type)
		{
			result = i;
			break;
		}
	}
	return result;
}

internal TypeIndex ResolveType(TypeChecker* tc, TypeDef* type)
{
	TypeIndex result = -1;
	
	switch(type->Type)
	{
		case(Type_Int):
		case(Type_Bool):
		case(Type_Void):
		case(Type_Float):
		{
			result = GetTypeIndex(tc, type->Type);
		}break;

		case(Type_Pointer):
		{
			Assert(tc->TypeCount < TYPE_CAPACITY);
			
			TypeIndex base = ResolveType(tc, type->Base);
			for(u64 i = 0; i < tc->TypeCount; i++)
			{
				if(tc->Types[i].Type == Type_Pointer &&
				   tc->Types[i].Pointer.Base == base)
				{
					result = i;
					return result;
				}
			}
			
			TypeIndex index = tc->TypeCount++;
			TypeInfo* type_info = &tc->Types[index];
			type_info->Type = Type_Pointer;
			type_info->Size = 8;
			type_info->Pointer.Base = base;
			result = index;
		}break;
		
	}

	return result;
}

internal b32 TypesAreCompatible(TypeIndex t1, TypeIndex t2)
{
	b32 result = false;
	if(t1 == t2) result = true;
	return result;
}

internal Symbol* GetSymbol(SymbolTable* sym_table, const String8& name,
						   b32 check_parent = true)
{
	Assert(sym_table);
	Symbol* result = 0;
	for(Symbol* s = sym_table->Symbols; s != 0; s = s->Next)
	{
		if(s->Name == name)
		{
			result = s;
			break;
		}
	}

	if(check_parent && (result == 0) && sym_table->Parent)
	{
		result = GetSymbol(sym_table->Parent, name);
	}
	return result;
}

internal void AddSymbol(M_Arena* arena, SymbolTable* sym_table,
						String8 ident, TypeIndex type)
{
	Symbol* sym = PushStruct(arena, Symbol);
	sym->Type = type;
	sym->Name = ident;
	sym->Next = sym_table->Symbols;
	sym_table->Symbols = sym;
	sym_table->SymbolCount++;
}

internal FunctionType* GetFunction(TypeChecker* tc, const String8& function_name)
{
	FunctionType* result = 0;

	for(u32 i = 0; i < tc->FunctionCount; i++)
	{
		if(tc->Functions[i].Name == function_name)
		{
			result = &tc->Functions[i];
			break;
		}
	}
	
	return result;
}

internal TypeIndex CheckNode(TypeChecker* tc, SymbolTable* sym_table, ASTNode* node)
{
	TypeIndex result = -1;
	
	switch(node->Type)
	{
		case(Node_IntegerLiteral):
		{
			result = GetTypeIndex(tc, Type_Int);
			Assert(result > -1);
			
			// node->StackSize = tc->Types[result].Size;
			node->EvalType = result;
		}break;

		case(Node_BoolLiteral):
		{
			result = GetTypeIndex(tc, Type_Bool);
			Assert(result > -1);
			
			// node->StackSize = tc->Types[result].Size;
			node->EvalType = result;
		}break;

		case(Node_Binary):
		{
			TypeIndex t1 = CheckNode(tc, sym_table, node->Binary.Left);
			TypeIndex t2 = CheckNode(tc, sym_table, node->Binary.Right);

			node->StackSize = 0;
			switch(node->Binary.Operator)
			{
				case(Op_Addition):
				case(Op_Multiplication):
				case(Op_Subtraction):
				case(Op_Division):
				{
					if(tc->Types[t1].Type != Type_Int ||
					   !TypesAreCompatible(t1, t2))
					{
						LogPanicF(0, "Types are not compatible %l & %l", t1, t2);
					}					
					node->EvalType = t1;
					result = t1;
					
				}break;

				default:
				{
					LogPanicF(0, "Types are not compatible %ld & %ld", t1, t2);
				}break;
			}
			
		}break;

		case(Node_Lookup):
		{
			Symbol* sym = GetSymbol(sym_table, node->Value.Lexeme);
			if(sym == 0)
			{
				LogPanicF(0, "Error: Lookup cannot find symbol %S", node->Value.Lexeme);
			}
			result = sym->Type;
			node->EvalType = result;
		}break;

		case(Node_VariableDeclaration):
		{
			String8 ident = node->VDecl.Ident.Lexeme;
			Symbol* sym = GetSymbol(sym_table, ident, false);
			if(sym != 0)
			{
				LogPanicF(0, "Error: Redefinition of variable %S in scope", ident);
			}

			// TODO(afb) :: Handle multiple types
			TypeIndex type = ResolveType(tc, node->VDecl.Type);
			AddSymbol(tc->Arena, sym_table, ident, type);
			result = GetTypeIndex(tc, Type_Void);
			node->StackSize = tc->Types[result].Size;
		}break;

		case(Node_Assignment):
		{
			Symbol* sym = GetSymbol(sym_table, node->Assignment.Ident.Lexeme);
			TypeIndex t1 = CheckNode(tc, sym_table, node->Assignment.Value);
			if(t1 != sym->Type)
			{
				LogPanic(0, "Error: Assignment lhs and rhs does not match");
			}
			result = GetTypeIndex(tc, Type_Void);
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
		}break;

		case(Node_Function):
		{
			SymbolTable* table = PushStructZero(tc->Arena, SymbolTable);
			table->Parent = sym_table;

			FunctionType func = {0};
			if(node->Func.ArgCount)
			{
				func.ArgNames = PushArray(tc->Arena, String8, node->Func.ArgCount);
				func.ArgTypes = PushArray(tc->Arena, TypeIndex, node->Func.ArgCount);
				func.ArgCount = node->Func.ArgCount;
			}

			u32 index = 0;
			for(FunctionArgument* n = node->Func.Args;
				n != 0;
				n = n->Next)
			{
				TypeIndex type = ResolveType(tc, n->Type);
				func.ArgNames[index] = n->Ident.Lexeme;
				func.ArgTypes[index] = type;
				index++;
				
				// node->StackSize += tc->Types[type].Size;
				AddSymbol(tc->Arena, table, n->Ident.Lexeme, type);
			}

			CheckNode(tc, table, node->Func.Body);

			TypeIndex return_type;
			if(node->Func.ReturnType)
			{
				return_type = ResolveType(tc, node->Func.ReturnType);
			}
			else
			{
				return_type = GetTypeIndex(tc, Type_Void);
			}

			func.Name = node->Func.Name.Lexeme;
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
		}break;
		
		default:
		{
			LogPanicF(0, "Error: Unhandled node in type checker %d", node->Type);
		}break;
	}

	return result;
}

internal void TypeCheck(TypeChecker* tc, ASTNode* tree)
{
	SymbolTable* global_sym_table =
		PushStructZero(tc->Arena, SymbolTable);
	
	for(ASTNode* node = tree; node != 0; node = node->Next)
	{
		CheckNode(tc, global_sym_table, node);
	}

	LogInfoF(0, "Type Count %ul", tc->TypeCount);
}

internal void TypeCheckerInit(TypeChecker* tc)
{
	tc->Arena = ArenaAlloc(MB(32));

	tc->Types[0].Type = Type_Void;
	tc->Types[0].Size = 0;
	tc->TypeCount++;

	tc->Types[1].Type = Type_Int;
	tc->Types[1].Size = 8;
	tc->TypeCount++;
	
	tc->Types[2].Type = Type_Bool;
	tc->Types[2].Size = 1;
	tc->TypeCount++;

	ArenaFree(tc->Arena);
}

