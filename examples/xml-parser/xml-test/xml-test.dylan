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
  if(arguments.size = 0 | arguments[0] = "-h" | arguments[0] = "--help")
    show-help();
  else
    let *substitute?* = arguments[0] ~= "-n" 
      & arguments[0] ~= "--no-entity-substitution";
    with-open-file(in = arguments[if(*substitute?*) 0 else 1 end])
      let doc = parse-document(in.stream-contents, 
                               substitute-entities?: *substitute?*);
      transform-document(doc, state: $html);
      let match = copy-sequence(arguments, start: if(*substitute?*) 1 else 2 end);
      if(match.size > 0)
        format-out("Found %d elements with shape //%s\n",
                   collect-elements(doc, match).size, 
                   reduce1(method(x, y) concatenate(x, "/", y) end, match));
      end if;
    end with-open-file;
  end if;
  exit-application(0);
end function main;

main(application-name(), application-arguments());

define function show-help()
  format-out("\nxml-test [--no-entity-substitution|-n] <file-name>\n\n"
    "\tParses an XML document (and its associated optional DTD)\n"
    "\tand outputs the result as an HTML-readable document.\n"
    "\tIf --no-entity-substitution is present, xml-test will build\n"
    "\tan internal DTD and leave the entities untouched in the\n"
    "\tdocument.\n");
end function show-help;

