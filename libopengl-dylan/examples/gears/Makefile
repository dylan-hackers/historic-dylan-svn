dylan-gears: dylan-gears.lid
	d2c -g -L../../ $<

dylan-gears.lid: dylan-gears-exports.dylan dylan-gears.dylan
	touch $@

clean:
	-rm -f *.mak *.lib.du *.o *.a *.s *.c ,* dylan-gears *~
