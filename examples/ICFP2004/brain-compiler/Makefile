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

bin-chris4:
	rm -rf chris4; d2c -M -L.. chris4.lid

run-chris4:
	rm -rf ../ant-brains/chris4.ant; ./chris4 > ../ant-brains/chris4.ant

bin-chris3:
	rm -rf chris3; d2c -M -L.. chris3.lid

run-chris3:
	rm -rf ../ant-brains/chris3.ant; ./chris3 > ../ant-brains/chris3.ant

bin-chris2:
	rm -rf chris2; d2c -M -L.. chris2.lid

run-chris2:
	rm -rf ../ant-brains/chris2.ant; ./chris2 > ../ant-brains/chris2.ant

bin-chris1:
	rm -rf chris1; d2c -M -L.. chris1.lid

run-chris1:
	rm -rf ../ant-brains/chris1.ant; ./chris1 > ../ant-brains/chris1.ant

-include brain-compiler.deps
