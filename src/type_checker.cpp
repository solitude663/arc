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
	// TODO(afb) :: Probaly shouldn't do a Assert.
	if(sym_table == 0) return 0;
	
	for(Symbol* s = sym_table->Symbols; s != 0; s = s->Next)
	{
		if(s->Name == name)
		{
			return s;
		}
	}
	
	if(check_parent && sym_table->Parent != 0)
	{
		return GetSymbol(sym_table->Parent, name);
	}
	return 0;
}

internal void AddSymbol(M_Arena* arena, SymbolTable* sym_table,
						String8 ident, TypeIndex type, b32 constant)
{
	Symbol* sym = PushStruct(arena, Symbol);
	sym->Name = ident;
	sym->Type = type;
	sym->Constant = constant;
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

internal const char* OpTypeToString(OperatorType op)
{
	if(op == Op_Addition) return "<addition>";
	if(op == Op_Subtraction) return "<subtraction>";
	if(op == Op_Multiplication) return "<multiplication>";
	if(op == Op_Division) return "<division>";

	return "<unknonw_operation>";
}

internal ASTNode* GenerateCastNode(TypeChecker* tc, TypeIndex target_type,
								   ASTNode* original)
{
	ASTNode* result = CreateASTNode(tc->Arena, Node_Cast);
	result->EvalType = target_type;
	result->Cast.Value = original;
	result->Cast.Implicit = true;
	return result;
}

internal b8 TypeIsNumeric(TypeChecker* tc, TypeIndex type)
{
	b8 result = false;
	if(tc->Types[type].Type == Type_Int || tc->Types[type].Type == Type_Float)
	{
		result = true;
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
			node->EvalType = result;
		}break;

		case(Node_FloatLiteral):
		{
			result = GetTypeIndex(tc, Type_Float);
			Assert(result > -1);
			node->EvalType = result;
		}break;

#if 0
		case(Node_BoolLiteral):
		{
			result = GetTypeIndex(tc, Type_Bool);
			Assert(result > -1);
			node->EvalType = result;
		}break;
#endif

		case(Node_Cast):
		{
			if(node->Cast.Implicit)
			{
				result = node->EvalType;
			}
			else
			{
				Unhandled();
			}
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
					if(t1 == t2) // TODO(afb) :: A bunch of checks about types
					{
						node->EvalType = t1;
						result = t1;
					}
					else if(TypeIsNumeric(tc, t1) && TypeIsNumeric(tc, t2))
					{
						tc->TreeChanged = true;
						if(tc->Types[t1].Type == Type_Int)
						{
							ASTNode* new_node = GenerateCastNode(tc, t2, node->Binary.Left);
							node->Binary.Left = new_node;
							node->EvalType = t2;
						}
						else
						{
							ASTNode* new_node = GenerateCastNode(tc, t1, node->Binary.Right);
							node->Binary.Right = new_node;
							node->EvalType = t1;
						}
						
						result = node->EvalType;
					}
					else
					{
						Unhandled();
						// TODO(afb) :: Type checking error
						TypeCheckerError(tc, 1, 1,
										 "Types (%d) and (%d) are not compatible for operation (%s).\n",
										 t1, t2, OpTypeToString(node->Binary.Operator));
					}

					node->Scope = sym_table;
				}break;

				default:
				{
					Unhandled();
					fprintf(stderr, "Types (%d) and (%d) are not compatible for operation (%s).\n",
							t1, t2, OpTypeToString(node->Binary.Operator));
				}break;
			}
			
		}break;

		case(Node_Lookup):
		{
			Symbol* sym = GetSymbol(sym_table, node->Value.Lexeme);
			if(sym == 0)
			{
				TypeCheckerError(tc, 1, 1, "Referece to undefined symbol(%.*s)", Str8Print(node->Value.Lexeme));
				result = GetTypeIndex(tc, Type_Void);				
			}
			else
			{
				result = sym->Type;
			}
			
			node->EvalType = result;
			node->Scope = sym_table;
		}break;

		case(Node_VariableDeclaration):
		{
			String8 ident = node->VDecl.Ident.Lexeme;
			Symbol* sym = GetSymbol(sym_table, ident, false);
			if(sym != 0)
			{
				TypeCheckerError(tc, 1, 1, "Redefinition of symbol %.*s in the same scope",
								 Str8Print(ident));
				result = GetTypeIndex(tc, Type_Void);
				node->Scope = sym_table;
				return result;
			}

			TypeIndex type = -1;
			if(node->VDecl.Implicit)
			{
				type = CheckNode(tc, sym_table, node->VDecl.Value);
			}
			else
			{
				// TODO(afb) :: Handle multiple types
				type = ResolveType(tc, node->VDecl.Type);
			}
			
			AddSymbol(tc->Arena, sym_table, ident, type, node->VDecl.Constant);
			// TODO(afb) :: Default type void to index 0
			result = GetTypeIndex(tc, Type_Void);
			node->StackSize = tc->Types[type].Size;

			node->Scope = sym_table;
		}break;

		case(Node_Assignment):
		{
			Symbol* sym = GetSymbol(sym_table, node->Assignment.Ident.Lexeme);
			if(sym)
			{
				if(sym->Constant)
				{
					Unhandled();
					TypeCheckerError(tc, 1, 1, "Trying to modify constant '%.*s'",
									 Str8Print(sym->Name));

					node->Scope = sym_table;
					return GetTypeIndex(tc, Type_Void);
				}
				
				TypeIndex t1 = sym->Type;
				TypeIndex t2 = CheckNode(tc, sym_table, node->Assignment.Value);
				if(TypeIsNumeric(tc, t1) && TypeIsNumeric(tc, t2))
				{
					if(t1 != t2)
					{
						TypeIndex target_type = t1;
						ASTNode* new_node = GenerateCastNode(tc, t1, node->Assignment.Value);
						node->Assignment.Value = new_node;						
					}
					node->EvalType = t1;
				}
				else
				{
					Unhandled();
					TypeCheckerError(tc, 1, 1, "Types cannot perform binary ops");
				}
			}
			else
			{
				Unhandled();
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

			tc->InFunction = true;
			TypeIndex return_type = CheckNode(tc, table, node->Func.Prototype);
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
				AddSymbol(tc->Arena, table, arg->Ident.Lexeme, type, false);
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
}

internal void TypeCheckerInit(TypeChecker* tc)
{
	*tc = {0};
	tc->Arena = ArenaAlloc(MB(32));

	tc->Types[tc->TypeCount].Type = Type_Void;
	tc->Types[tc->TypeCount].Size = 0;
	tc->TypeCount++;

	tc->Types[tc->TypeCount].Type = Type_Int;
	tc->Types[tc->TypeCount].Size = 8;
	tc->TypeCount++;

	tc->Types[tc->TypeCount].Type = Type_Float;
	tc->Types[tc->TypeCount].Size = 8;
	tc->TypeCount++;

	// tc->types[tc->typecount].type = type_bool;
	// tc->types[tc->typecount].size = 1;
	// tc->typecount++;

	// ArenaFree(tc->Arena);
}

