D2C=DYLANPATH='../../../runtime-src/d2c/runtime/dylan:../../../runtime-src/d2c/runtime/c-code:../../../src/d2c/runtime/gc' ../../../src/d2c/compiler/main/d2c

sieve-general: sieve-general.lid sieve-general.dylan sieve-general-exports.dylan
	$(D2C) sieve-general.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ sieve-general
	-rm -rf .libs
