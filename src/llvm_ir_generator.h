#ifndef LLVM_IR_GENERATOR_H
#define LLVM_IR_GENERATOR_H

struct LLVMIRGen
{
	M_Arena* Arena;
	LLVMContextRef Context;
	LLVMModuleRef Module;
	LLVMBuilderRef Builder;

	LLVMTypeRef IntType;
	LLVMTypeRef FloatType;
};

#endif // Header guard
