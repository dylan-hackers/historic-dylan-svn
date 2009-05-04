all: doctower

SOURCES = \
	support-lib/support-library.lid \
	markup-parser-lib/markup-parser-library.lid \
	dylan-parser-lib/dylan-parser-library.lid \
	main-lib/doctower-library.lid

BASE_LIBDIR = ../..

LIBRARIES = \
	$(BASE_LIBDIR)/utilities/command-line-parser \
	$(BASE_LIBDIR)/utilities/peg-parser \
	$(BASE_LIBDIR)/utilities/dynamic-binding \
	$(BASE_LIBDIR)/monday/lib/program-representation/source-location \
	$(BASE_LIBDIR)/files-and-io/sequence-stream \
	$(BASE_LIBDIR)/files-and-io/wrapper-streams \
	$(BASE_LIBDIR)/data-structures/skip-list

libraries = $(abspath $(LIBRARIES))
sources = $(abspath $(SOURCES))
libflags = $(addprefix -L,$(libraries)) $(addprefix -L,$(dir $(sources)))

.PHONY: all
.PHONY: clean

ifneq ($(MAKECMDGOALS),clean)
-include $(SOURCES:.lid=.dep)
endif

doctower : main-lib/doctower-library.lib.du
	cp main-lib/doctower doctower

support-lib/support-library.lib.du : \
	support-lib/support-library.lid

markup-parser-lib/markup-parser-library.lib.du : \
	markup-parser-lib/markup-parser-library.lid

dylan-parser-lib/dylan-parser-library.lib.du : \
	dylan-parser-lib/dylan-parser-library.lid

main-lib/doctower-library.lib.du : \
	main-lib/doctower-library.lid

%.dep %.lib.du : %.lid
	cd $(dir $<) ; d2c -gM $(libflags) $(notdir $<)
	@sed -i "~" -e "1s'.*:'$*.dep $*.lib.du:'" $*.dep

clean:
	-find . \( -name "doctower" -or \
		   -name "*.o" -or \
		   -name "*.s" -or \
		   -name "*.a" -or \
		   -name "*.c" -or \
		   -name "*.du" -or \
		   -name "*.el" -or \
		   -name "*.mak" -or \
		   -name "*.mak-temp" -or \
		   -name "*.dep" -or \
		   -name "*~" -or \
		   -name ".libs" \) \
		-delete
