balls: balls.lid
	d2c -L../../ $<

balls.lid: balls-exports.dylan balls.dylan
	touch $@

clean:
	-rm -f *.mak *.lib.du *.o *.a *.s *.c ,* balls *~
