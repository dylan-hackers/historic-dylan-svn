Module:    xml-test
Synopsis:  Exercises the XML library
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

/** Parses an XML and outputs it as HTML-readable text.
 *
 *  Here's the game plan:
 *  - first, scan the <document> for all used entities; store them with their
 *    defs
 *  - then, transform the <document> into HTML by doing the following:
 *    * output a header with an internal DTD
 *    * output the document tag and all its children (with xforms along the
 *      way)
 *
 *  The entity-scan only needs to override transform on <entity-reference> 
 *  (this is in entity-pass.dylan).
 *
 *  The output task override most the transform methods to convert <tag> to 
 *  &lt;tag&gt;, etc (adds some pretty font coloring and entity links, too).
 *  This is in html-xform.dylan.
 **/

define argument-parser <cmd-line-parser> ()
  synopsis show-help,
   usage: "xml-test [options] <file-name> [pattern]",
   description:
    "Parses an XML document (and its associated optional DTD)\n"
    "and outputs the result as an HTML-readable document.  [pattern] is a\n"
    "space-separated list to find in the document.";

   regular-arguments file-and-pattern;

   option no-sub-ents? :: <boolean>, "do not substitute entities",
    long: "no-entity-substitution", short: "n";
     
   option xform? :: <boolean>, 
    "transform attributes to entities & vice versa",
    long: "transform", short: "t";

  option help? :: <boolean>,
    "display this help and exit",
    long: "help", short: #("?", "h");
end argument-parser;

define function main(program-name, arguments)
  *ampersand* := "&amp;";
  *open-the-tag* := "&lt;";
  *printer-state* := $html;

  let parser = make(<cmd-line-parser>);
  parse-arguments(parser, arguments);

  if(parser.help?)
    show-help(parser, *standard-output*);
    exit-application(0);
  end if;

  with-open-file(in = parser.file-and-pattern[0], direction: #"input-output")
    let doc = parse-document(stream-contents(in, clear-contents?: #f),
                             substitute-entities?: ~ parser.no-sub-ents?);
    let filename = concatenate(as(<string>, doc.name), "-xml.html");
    if(parser.xform?) walk-n-change(doc.node-children[0]); end if;
    with-open-file(file = filename, direction: #"output")
      transform-document(doc, state: $html, stream: file);
    end with-open-file;
    format-out("Wrote out %s\n", filename);
    let match = copy-sequence(arguments, 
                              start: if(*substitute?*) 1 else 2 end);
    let match = copy-sequence(parser.file-and-pattern, start: 1);
    if(match.size > 0)
      format-out("Found %d elements with shape //%s\n",
                 collect-elements(doc, match).size, 
                 reduce1(method(x, y) concatenate(x, "/", y) end, match));
    end if;
  end with-open-file;
  exit-application(0);
end function main;

main(application-name(), application-arguments());

define function walk-n-change(elt :: <element>) => ()
  let new-kids = map(method(x) 
                         make(<element>, name: x.name, parent: elt, 
                              attributes: #[], 
                              children: vector(make(<char-string>, 
                                                    text: x.attribute-value)))
                     end, elt.attributes);

  let (candidate-attribs, original-children)
    = partition(method(x) 
                    instance?(x, <element>)
                    & x.attributes.empty?
                    & x.node-children.size = 1
                    & instance?(x.node-children[0], <char-string>)
                    & element(elt, x.name, always-sequence?: #t).size = 1
                end, elt.node-children);

  let new-attribs = map(method(x)
                            make(<attribute>, name: x.name, value: x.text)
                        end, as(<vector>, candidate-attribs));
  
  elt.node-children := concatenate(as(<vector>, original-children), new-kids);
  elt.attributes := new-attribs;
  do(walk-n-change, choose(rcurry(instance?, <element>), original-children));
end function walk-n-change;
