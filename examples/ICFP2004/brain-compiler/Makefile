bin:
	d2c -M brain-compiler.lid

run:	bin
	./brain-compiler

clean:
	-rm -f *.o *.lo *.s *.a *.c *.mak *~ brain-compiler
	-rm -rf .libs

-include brain-compiler.deps
