.PHONY: all snap clean distclean reallyclean

MANUALS = gduser gdlibs gdmaint tutorial

all: $(MANUALS)

snap: distclean
	(cd ..; tar czvf gdref-0.1.`date +%Y%m%d`.tar.gz gdref)

clean:
	rm -rf $(MANUALS)
	rm -f *.ps *.eps
	rm -rf *.junk db2html-dir

distclean: clean
	rm -f *~

reallyclean: distclean

%: %.sgml entities.sgml
	rm -rf $@
	db2html $<
