.PHONY: all snap clean distclean reallyclean html-chunked html-unchunked

SGML_BASE_DIR=/usr/local/share/sgml
SGML_CATALOG_FILES=$(SGML_BASE_DIR)/dsssl/catalog:$(SGML_BASE_DIR)/docbook/docbook.cat:$(SGML_BASE_DIR)/iso/iso.cat:$(SGML_BASE_DIR)/docbook_stylesheets/catalog:$(SGML_BASE_DIR)/dylandoc/dylandoc.cat

DYLANDOC_TOOLS = CATALOG dylandoc.dtd dylandoc.dsl
STANDARD_DEPS = dylandoc.ent $(DYLANDOC_TOOLS)
JADE = SGML_CATALOG_FILES=$(SGML_CATALOG_FILES) jade -E 5 -c CATALOG

SGML_MANUALS = gduser.sgml gdlibs.sgml gdmaint.sgml tutorial.sgml gdapi.sgml
HTML_CHUNKED_MANUALS = gduser gdlibs gdmaint tutorial gdapi
HTML_UNCHUNKED_MANUALS = gduser.html gdlibs.html gdmaint.html tutorial.html gdapi.html
RTF_MANUALS = gduser.rtf gdlibs.rtf gdmaint.rtf tutorial.rtf gdapi.rtf

######################################################################
#
# Default Rules
#

# Make an HTML directory.
%: %.sgml $(STANDARD_DEPS)
	-rm -rf $@
	-rm -rf ,$@
	mkdir ,$@
	ln dylandoc.dsl ,$@/dylandoc.dsl # oh, what a hack...
	(cd ,$@; \
	 SGML_CATALOG_FILES=$(SGML_CATALOG_FILES) jade -E 5 -c ../CATALOG -d dylandoc.dsl#html -t sgml -ihtml ../$<)
	rm ,$@/dylandoc.dsl
	mv ,$@ $@

# Make a single HTML file.
%.html: %.sgml $(STANDARD_DEPS)
	$(JADE) -d dylandoc.dsl#html -t sgml -ihtml -V nochunks $< > $@
	rm -f HTML.manifest

# Make an RTF file.
%.rtf: %.sgml $(STANDARD_DEPS)
	$(JADE) -d dylandoc.dsl#print -t rtf $<

# Make a flow object tree, just for kicks.
%.fot: %.sgml $(STANDARD_DEPS)
	$(JADE) -d dylandoc.dsl#print $<

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
