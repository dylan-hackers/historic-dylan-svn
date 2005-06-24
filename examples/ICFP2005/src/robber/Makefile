go: robber
	./robber

robber: robber.lid robber.dylan robber-exports.dylan
	d2c -s robber.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ robber
	-rm -rf .libs
