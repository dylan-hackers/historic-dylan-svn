SOURCES=icfp2002.dylan icfp2002-exports.dylan \
  path.dylan \
  board.dylan \
  robot.dylan \
  package.dylan \
  command.dylan \
  robot-agent.dylan \
  messages.dylan \
  point.dylan \
  dumbot.dylan


D2C_FLAGS=-s -g -L.

icfp2002: icfp2002.lid $(SOURCES) network.lib.du
	d2c $(D2C_FLAGS) icfp2002.lid

clean:
	rm -f *.o *.s *.a *.c *.mak *~ icfp2002 *.du
	rm -rf .libs


install: icfp2002
	strip icfp2002
	cd ..; tar czf dylan-hackers.tgz README buildme runme \
           src/Makefile src/icfp2002.lid src/*.dylan src/icfp2002
	   md5sum ../dylan-hackers.tgz

test: icfp2002
	(time ./icfp2002) && echo **Success**

network-internal.dylan: network-internal.intr network-internal.h
	melange --d2c -I`gcc --print-file-name=include` network-internal.intr network-internal.dylan

network.lib.du: network.lid network.dylan network-exports.dylan network-internal.dylan Makefile
	d2c $(D2C_FLAGS) network.lid

test-server: test-server.lid test-server.dylan test-server-exports.dylan Makefile
	d2c $(D2C_FLAGS) test-server.lid
