module: xml-parser
author: Andreas Bogk <andreas@andreas.org>
copyright: (c) 2000 Gwydion Dylan Maintainers. Licensed under LGPL

define interface
  #include "libxml/parser.h",
    name-mapper: c-to-dylan,
    equate: {"char*" => <c-string>,
	     "xmlChar*" => <c-string>,
	     "unsigned char*" => <c-string>},
    map: {"char*" => <byte-string>, 
	  "xmlChar*" => <byte-string>,
	  "unsigned char*" => <byte-string>};
end interface;
