shared: libworld-dylan.a

libworld-dylan.a: world.lid world.dylan world-exports.dylan util.dylan agent.dylan
	d2c -g -s world.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *.du *~
	-rm -rf .libs

