library: compiler-cback
unique-id-base: 10750
shared-library: yes
linker-options: /usr/local/lib/LLVMCore.o /usr/local/lib/LLVMSystem.o -lLLVMSupport
files:	cback-exports
	cback
	primemit
	heap
	stackanal
c-object-files:	llvm-glue.o
