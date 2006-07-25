library: compiler-cback
unique-id-base: 10750
shared-library: yes
linker-options: -lLLVMCore -lLLVMSupport -lLLVMSystem
files:	cback-exports
	cback
	primemit
	heap
	stackanal
c-object-files:	llvm-glue.o
