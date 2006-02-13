.PHONY: all snap clean distclean reallyclean html-chunked html-unchunked

DYLANDOC_TOOLS = CATALOG dylandoc.dtd dylandoc.xsl dylandoc-web.xsl dylandoc-unchunked.xsl
STANDARD_DEPS = dylandoc.ent $(DYLANDOC_TOOLS)
XMLTO=/usr/bin/xmlto

XML_MANUALS = gduser.xml gdlibs.xml gdmaint.xml tutorial.xml gdapi.xml
HTML_CHUNKED_MANUALS = gduser gdmaint tutorial gdapi #gdlibs
HTML_UNCHUNKED_MANUALS = gduser.html gdmaint.html tutorial.html gdapi.html #gdlibs.html
PDF_MANUALS = gduser.pdf gdmaint.pdf tutorial.pdf gdapi.pdf #gdlibs.pdf

######################################################################
#
# Default Rules
#

# Make an HTML directory.
%: %.xml $(STANDARD_DEPS)
	-rm -rf $@
	$(XMLTO) -o $@ -x dylandoc-web.xsl html $<

# Make a single HTML file.
%.html: %.xml $(STANDARD_DEPS)
	$(XMLTO) -x dylandoc-unchunked.xsl html $<
	mv $*.proc $@
	rm -f HTML.manifest

# Make an PDF file.
%.pdf: %.xml $(STANDARD_DEPS)
	$(XMLTO) dylandoc-pdf.xsl pdf $<

# Make a flow object tree, just for kicks.
#%.fot: %.sgml $(STANDARD_DEPS)
#	$(JADE) -d dylandoc.dsl#print $<

######################################################################
#
# Building the manuals 
#

all: html-chunked
html-chunked: $(HTML_CHUNKED_MANUALS)
html-unchunked: $(HTML_UNCHUNKED_MANUALS)
pdf: $(PDF_MANUALS)

snap: distclean
	(cd ..; tar czvf gdref-0.1.`date +%Y%m%d`.tar.gz gdref)

clean:
	rm -rf $(HTML_CHUNKED_MANUALS)
	rm -f *.html *.pdf *.fot *.dvi
	rm -f core

distclean: clean
	rm -f *~

reallyclean: distclean
