.PHONY: all snap clean distclean reallyclean

all: melange tutorial gdc

snap: distclean
	(cd ..; tar czvf gdref-0.1.`date +%Y%m%d`.tar.gz gdref)

clean:
	rm -rf melange
	rm -rf tutorial
	rm -rf gdc
	rm -f *.ps *.eps

distclean: clean
	rm -f *~

reallyclean: distclean

%: %.sgml
	rm -rf $@
	db2html $<

melange: melange.sgml entities.sgml

tutorial: tutorial.sgml entities.sgml

gdc: gdc.sgml entities.sgml
