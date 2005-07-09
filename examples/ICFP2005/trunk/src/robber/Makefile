
robber: robber.lid robber.dylan alterna-robber.dylan random-walk-robber.dylan bruce-robber.dylan robber-exports.dylan cheating-robber.dylan ../shared/world.lib.du
	d2c -g -s -L../shared robber.lid

run: robber
	server --gui --robber "show-robber ./robber-wrapper"

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ robber
	-rm -rf .libs

update:
	cd ../.. && svn update && cd -

commit:
	cd ../.. && svn commit -m "" && cd -
