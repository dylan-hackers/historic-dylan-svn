validate: validate.lid validate.dylan validate-exports.dylan
	d2c validate.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ validate
	-rm -rf .libs

test: validate
	./validate ../tests/test5 ../tests/test5.out
