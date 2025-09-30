#ifndef CODE_GENERATOR_H
#define CODE_GENERATOR_H

enum Registers
{
	rax, rbx, rcx, rdx, rdi, rsi,
	r8, r11, r12, r13, r14, r15,
	Register_Count,
};

global const char* RegisterNames[Register_Count] = {
	"rax", "rbx", "rcx", "rdx", "rdi", "rsi",
	"r8", "r11", "r12", "r13", "r14", "r15"	
};

struct CodeGenerator
{
	M_Arena* Arena;

	i32 Registers[Register_Count];
	String8List* CodeBuilder;
	String8List* DataBuilder;
};

#endif // Header guard
