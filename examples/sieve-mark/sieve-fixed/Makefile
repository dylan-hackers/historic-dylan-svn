D2C=DYLANPATH='../../../runtime-src/d2c/runtime/dylan:../../../runtime-src/d2c/runtime/c-code:../../../src/d2c/runtime/gc' ../../../src/d2c/compiler/main/d2c

sieve-fixed: sieve-fixed.lid sieve-fixed.dylan sieve-fixed-exports.dylan
	$(D2C) sieve-fixed.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ sieve-fixed
	-rm -rf .libs
