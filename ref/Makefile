.PHONY: all snap clean distclean reallyclean html-chunked html-unchunked

DYLANDOC_TOOLS = CATALOG dylandoc.dtd dylandoc.dsl
STANDARD_DEPS = entities.sgml $(DYLANDOC_TOOLS)
JADE = jade -d dylandoc.dsl -c CATALOG

SGML_MANUALS = gduser.sgml gdlibs.sgml gdmaint.sgml tutorial.sgml
HTML_CHUNKED_MANUALS = gduser gdlibs gdmaint tutorial
HTML_UNCHUNKED_MANUALS = gduser.html gdlibs.html gdmaint.html tutorial.html
RTF_MANUALS = gduser.rtf gdlibs.rtf gdmaint.rtf tutorial.rtf

######################################################################
#
# Default Rules
#

# Make an HTML directory.
%: %.sgml $(STANDARD_DEPS)
	rm -rf $@
	mkdir $@
	(cd $@; \
	  jade -c ../CATALOG -d ../dylandoc.dsl -t sgml -i html ../$<)

# Make a single HTML file.
%.html: %.sgml $(STANDARD_DEPS)
	$(JADE) -t sgml -i html -V nochunks $< > $@
	rm -f HTML.manifest

# Make an RTF file.
%.rtf: %.sgml $(STANDARD_DEPS)
	$(JADE) -t rtf $<

# Make a flow object tree, just for kicks.
%.fot: %.sgml $(STANDARD_DEPS)
	$(JADE) $<

######################################################################
#
# Building the manuals 
#

all: html-chunked
html-chunked: $(HTML_CHUNKED_MANUALS)
html-unchunked: $(HTML_UNCHUNKED_MANUALS)
rtf: $(RTF_MANUALS)

snap: distclean
	(cd ..; tar czvf gdref-0.1.`date +%Y%m%d`.tar.gz gdref)

clean:
	rm -rf $(HTML_CHUNKED_MANUALS)
	rm -f *.html *.rtf *.fot *.dvi
	rm -f core

distclean: clean
	rm -f *~

reallyclean: distclean

