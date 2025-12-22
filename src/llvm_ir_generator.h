#ifndef LLVM_IR_GENERATOR_H
#define LLVM_IR_GENERATOR_H

struct LLVMIRGen
{
	M_Arena* Arena;
	LLVMContextRef Context;
	LLVMModuleRef Module;
	LLVMBuilderRef Builder;

	b32 InFunction;
	
	TypeInfo* Types;
	u32 TypeCount;
	
	LLVMTypeRef IntType;
	LLVMTypeRef FloatType;
};

#endif // Header guard
