
bot-driver: bot-driver.lid bot-driver.dylan bot-driver-exports.dylan ../shared/world.lib.du ../cop/cop.lib.du ../robber/robber.lib.du
	d2c -g -s -L../shared -L../cop -L../robber bot-driver.lid

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ bot-driver
	-rm -rf .libs
