bin:
	d2c -M -L.. brain-compiler.lid

run:	bin
	./brain-compiler

bin-alex:
	rm -rf alex; d2c -M -L.. alex.lid

run-alex:
	rm -rf ../ant-brains/alex-generated.ant; ./alex > ../ant-brains/alex-generated.ant

clean:
	-rm -f *.o *.lo *.s *.a *.c *.mak *~ brain-compiler alex
	-rm -rf .libs

bin-chris2:
	rm -rf chris2; d2c -M -L.. chris2.lid

run-chris2:
	rm -rf ../ant-brains/chris2.ant; ./chris2 > ../ant-brains/chris2.ant

bin-chris1:
	rm -rf chris1; d2c -M -L.. chris1.lid

run-chris1:
	rm -rf ../ant-brains/chris1.ant; ./chris1 > ../ant-brains/chris1.ant

-include brain-compiler.deps
