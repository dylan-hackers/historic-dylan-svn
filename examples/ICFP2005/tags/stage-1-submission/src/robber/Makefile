SHARED=../shared/Makefile ../shared/world.lid ../shared/world-exports.dylan \
../shared/util.dylan  ../shared/world.dylan ../shared/agent.dylan

robber: robber.lid robber.dylan alterna-robber.dylan random-walk-robber.dylan bruce-robber.dylan robber-exports.dylan ${SHARED}
	(cd ../shared; make)
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
