bin:
	d2c -M -L.. brain-compiler.lid

run:	bin
	./brain-compiler

bin-alex:
	rm -rf alex; d2c -s -L.. alex.lid

run-alex:
	rm -rf ../ant-brains/alex-generated.ant; ./alex > ../ant-brains/alex-generated.ant

clean:
	-rm -f *.o *.lo *.s *.a *.c *.mak *~ brain-compiler alex
	-rm -rf .libs

-include brain-compiler.deps
