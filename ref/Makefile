.PHONY: all snap clean distclean reallyclean

all: gduser gdlibs gdmaint tutorial gdc

snap: distclean
	(cd ..; tar czvf gdref-0.1.`date +%Y%m%d`.tar.gz gdref)

clean:
	rm -rf gduser
	rm -rf gdlibs
	rm -rf gdmaint
	rm -rf tutorial
	rm -rf gdc
	rm -f *.ps *.eps

distclean: clean
	rm -f *~

reallyclean: distclean

%: %.sgml
	rm -rf $@
	db2html $<

gduser: gduser.sgml entities.sgml

gdlibs: gdlibs.sgml entities.sgml

gdmaint: gdmaint.sgml entities.sgml

tutorial: tutorial.sgml entities.sgml

gdc: gdc.sgml entities.sgml
