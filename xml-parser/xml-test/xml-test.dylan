Module:    xml-test
Synopsis:  Exercises the XML library
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

/** Parses an XML and outputs it as HTML-readable text.
 *
 *  Here's the game plan:
 *  - first, scan the <document> for all used entities; store them with their defs
 *  - then, transform the <document> into HTML by doing the following:
 *    * output a header with an internal DTD
 *    * output the document tag and all its children (with xforms along the way)
 *
 *  The entity-scan only needs to override transform on <entity-reference> 
 *  (this is in entity-pass.dylan).
 *
 *  The output task override most the transform methods to convert <tag> to &lt;tag&gt;,
 *  etc (adds some pretty font coloring and entity links, too).  This is in 
 *  html-xform.dylan.
 **/

define function main(name, arguments)
  with-open-file(in = arguments[0])
    transform-document(parse-document(in.stream-contents), // substitute-entities?: #f),
                       state: $html);
  end;
  exit-application(0);
end function main;

main(application-name(), application-arguments());
