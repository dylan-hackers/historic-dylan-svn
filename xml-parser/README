Native Dylan XML-parser library

We present a native Dylan XML parser library.  This library needs
the META library (available from www.gwydiondylan.org) and works on both
Functional Developer and d2c (the Gwydion Dylan compiler) that supports
each-subclass slots (2.3.9pre1 dated after 20020415).

This parser comes with additional features.  You may transform the internal 
representation to any form desired (XML, HTML, text, what-have-you).  You may
also extract elements that have a certain shape (e.g. "//dvd/title").  You
may index into attributes or immediate child elements using Dylan syntax.  And,
you may output the XML to a stream, transformed or in its original state.
Please read the code in collect.dylan and xml-test/ to see how to do both.

Of course, this XML parser is very much a work in progress (with Doug Auclair
hacking away at it currently) -- this library neither checks for validity nor 
for well-formedness; perhaps those checks won't be added for a long time to 
come.  However, it is currently being used in production-quality commercial
and non-commercial projects.


xml-test/ converts an XML doc to an HTML-readable one with links to entities
(if they have not be expanded-out in the resulting document).

REQUIRED LIBRARIES
==================

To compile the xml-parser sources and to create an executable using the
xml-parser library, you need the following libraries also available from 
gd/examples:  anaphora, multimap, and meta.

HISTORY
=======
Chris Double started this effort, and he developed an initial, working, parser
using the Dylan META library from David Lichteblau.  He also developed the
Dylan XML class hierarchy (along the lines of the DOM), and had an algorithm
for parsing going.

Andreas Bogk developed the META parse tree into (mostly full) compliance with
the XML spec (BNF?).

Doug Auclair offered suggestions for improvements to META, integrated Andreas'
work with Chris', created some macros to ease the parsing effort, implemented
the transform functionality and entities (both parsing and substituting), and
fixed bugs. 

Doug Auclair
<doug@cotilliongroup.com>

