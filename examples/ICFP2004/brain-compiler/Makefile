bin:
	d2c -M -L.. brain-compiler.lid

run:	bin
	./brain-compiler

alex:
	d2c -s -g --debug -L.. alex.lid

clean:
	-rm -f *.o *.lo *.s *.a *.c *.mak *~ brain-compiler alex
	-rm -rf .libs

-include brain-compiler.deps
