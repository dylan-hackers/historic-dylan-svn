SOURCES=icfp2002.dylan icfp2002-exports.dylan \
  utils.dylan \
  path.dylan \
  board.dylan \
  robot.dylan \
  package.dylan \
  command.dylan \
  robot-agent.dylan \
  messages.dylan \
  point.dylan \
  dumbot.dylan \
  dumber-bot.dylan \
  pushbot.dylan \
  thomas.dylan \
  gabot.dylan \
  tour.dylan


D2C_FLAGS=-s -g -L.

icfp2002: icfp2002.lid $(SOURCES) network.lib.du
	d2c $(D2C_FLAGS) icfp2002.lid

clean:
	rm -f *.o *.s *.a *.c *.mak *~ icfp2002 *.du
	rm -rf .libs


install: icfp2002
	strip icfp2002
	cd ..; tar czf dylan-hackers.tgz README buildme runme \
           src/Makefile src/icfp2002.lid src/*.dylan src/icfp2002 src/*.intr src/*.lid src/c-opengl-visualizer.c src/network-internal.h TODO
	   md5sum ../dylan-hackers.tgz

test: icfp2002
	./icfp2002 cantor.gwydiondylan.org 6969

test-dumbot: icfp2002
	./icfp2002 cantor.gwydiondylan.org 6969 dumbot

test-pushbot: icfp2002
	./icfp2002 cantor.gwydiondylan.org 6969 pushbot

test-gabot: icfp2002
	./icfp2002 cantor.gwydiondylan.org 6969 gabot

test-thomas: icfp2002
	./icfp2002 cantor.gwydiondylan.org 6969 thomas

single: icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22001

multi:  icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22101


test-all-22101: icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22101 dumbot > dumbot-22101.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22101 thomas > thomas-22101.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22101 pushbot > pushbot-22101.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22101 gabot > gabot-22101.log 2>&1 &

test-all-22102: icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22102 dumbot > dumbot-22102.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22102 thomas > thomas-22102.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22102 pushbot > pushbot-22102.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22102 gabot > gabot-22102.log 2>&1 &

test-all-22103: icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22103 dumbot > dumbot-22103.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22103 thomas > thomas-22103.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22103 pushbot > pushbot-22103.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22103 gabot > gabot-22103.log 2>&1 &

test-all-22104: icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22104 dumbot > dumbot-22104.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22104 thomas > thomas-22104.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22104 pushbot > pushbot-22104.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22104 gabot > gabot-22104.log 2>&1 &

test-all-22105: icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22105 dumbot > dumbot-22105.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22105 thomas > thomas-22105.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22105 pushbot > pushbot-22105.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22105 gabot > gabot-22105.log 2>&1 &

test-all-22106: icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22106 dumbot > dumbot-22106.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22106 thomas > thomas-22106.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22106 pushbot > pushbot-22106.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22106 gabot > gabot-22106.log 2>&1 &

test-all-22107: icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22107 dumbot > dumbot-22107.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22107 thomas > thomas-22107.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22107 pushbot > pushbot-22107.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22107 gabot > gabot-22107.log 2>&1 &

test-all-22108: icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22108 dumbot > dumbot-22108.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22108 thomas > thomas-22108.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22108 pushbot > pushbot-22108.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22108 gabot > gabot-22108.log 2>&1 &

test-all-22109: icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22109 dumbot > dumbot-22109.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22109 thomas > thomas-22109.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22109 pushbot > pushbot-22109.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22109 gabot > gabot-22109.log 2>&1 &

test-all-22110: icfp2002
	./icfp2002 icfp1.cse.ogi.edu 22110 dumbot > dumbot-22110.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22110 thomas > thomas-22110.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22110 pushbot > pushbot-22110.log 2>&1 &
	./icfp2002 icfp1.cse.ogi.edu 22110 gabot > gabot-22110.log 2>&1 &


# This doesn't work. Probably I don't understand shell quoting rules.
#kill-all-bots:
#	ps -C icfp2002 | grep '[i]cfp' | awk '{print $1}' | xargs kill

network-internal.dylan: network-internal.intr network-internal.h
	melange --d2c -I`gcc --print-file-name=include` network-internal.intr network-internal.dylan

network.lib.du: network.lid network.dylan network-exports.dylan network-internal.dylan
	d2c $(D2C_FLAGS) network.lid

test-server: test-server.lid test-server.dylan test-server-exports.dylan
	d2c $(D2C_FLAGS) test-server.lid

c-opengl-visualizer:	c-opengl-visualizer.c
	g++ -g c-opengl-visualizer.c -lGL -lGLU -lglut -lX11 -lXmu -lXext -lXi -L/usr/X11R6/lib -o c-opengl-visualizer

macosx-visualizer:	c-opengl-visualizer.c
	cc c-opengl-visualizer.c -framework OpenGL -framework GLUT -framework Cocoa -o c-opengl-visualizer

c-opengl-visualizer-run:	c-opengl-visualizer
	./c-opengl-visualizer cantor.gwydiondylan.org 6969
