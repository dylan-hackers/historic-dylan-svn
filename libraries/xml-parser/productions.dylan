module: %productions
synopsis: Implements a META parser for XML 1.0
based-on-work-by: Chris Double <chris@double.co.nz>
fleshed-out-by: Andreas Bogk <andreas@andreas.org>
synthesis: Douglas M. Auclair <doug@cotilliongroup.com> -- DMA
comments-by: DMA unless otherwise indicated
copyright: LGPL

// *debug-meta-functions?* := #t; // uncomment this line for spewage

/*
 * 
 * This library implements a parser for XML 1.0, using the META parser
 * technique.
 * 
 * Source for the specification was:
 * 
 *   http://www.w3.org/TR/2000/REC-xml-20001006
 * 
 * The scan-foo clauses contain a numeric reference to the associated
 * production in the specification.
 *
 * prerequisit:  the meta library (currently at gd/examples/meta/)
 * 
 * Useful for the understanding of this parser:
 *   http://linux.rice.edu/~rahul/hbaker/Prag-Parse.html (a paper on META)
 *
 * Revisions:
 * March 19, 2002 -- The parser now populates elements with attributes that
 *                   have default values (defined in the DTD).
 *
 * March 19, 2002 -- Some people live in a single-serving world; I live in
 *                   the 80-column world.  Cleaned up comments and code for
 *                   80-column viewers.
 *
 * March 19, 2002 -- scan-element now only scans the opening tag once.  The
 *                   BNF indicated two scans (one to check for empty elements),
 *                   but I (DMA) refactored to one scan only ... this 
 *                   refactoring eliminates productions 40 (Stag) and 
 *                   44 (EmptyElemTag)
 */

//-------------------------------------------------------
// Productions

// Document
// 
//    [1]    document    ::=    prolog element Misc*
//
define function parse-document (doc :: <string>, 
                                #key start = 0, end: stop, 
                                substitute-entities? = #t,
                                modify-elements-when-parsing? = #t,
			        print-warnings? = #f,
				dtd-paths = #f,
 			        ignore-instructions? = #f)
 => (stripped-tree :: <document>)
  initialize-latin1-entities();
  *show-warnings?* := print-warnings?;
  *defining-entities?* := #f;
  *ignore-instructions?* := ignore-instructions?;
  *modify?* := modify-elements-when-parsing?;
  *substitute-entities?* := substitute-entities?;
  if(dtd-paths) *dtd-paths* := dtd-paths; end if;
  let (index, document) = scan-document-helper(doc, start: start, end: stop);
  transform-document(document, state: make(<add-parents>));
  document;
end function parse-document;

define variable *modify?* :: <boolean> = #t;
define variable *show-warnings?* :: <boolean> = #f;
define variable *ignore-instructions?* :: <boolean> = #t;
define variable *dtd-paths* :: <sequence> = #(".");

define meta document-helper(prolog, elemnt, misc) =>
 (make(<document>,
       children: if(*ignore-instructions?*) 
                   vector(elemnt)
                 else
                   concatenate(prolog, vector(elemnt), misc)
                 end if))
   scan-prolog(prolog), 
   scan-element(elemnt), scan-miscs(misc)
end meta document-helper;

// Character Range
//
//    [2]    Char   ::= #x9 | #xA | #xD | [#x20-#xD7FF] 
//                          | [#xE000-#xFFFD] | [#x10000-#x10FFFF] 
// /* any Unicode character, excluding the surrogate blocks,
//    FFFE, and FFFF. */
//    
// Andreas: FIXME: we're cheating here, by using UFT8 and just allowing 
//                 anything.
//
define constant <char> = <character>;

// an entirely redundant definition if we had a BNF parser
define meta s?(c) loop(scan-s(c)) end;

// Names and Tokens
// 
//    [4]    NameChar    ::=    Letter | Digit 
//                              | '.' | '-' | '_' | ':' 
//                              | CombiningChar | Extender
//
define constant $name-char = concatenate($version-number, "-");

//    [5]    Name        ::=    (Letter | '_' | ':') (NameChar)*
//
define collector name(c)
  [{element-of($letter, c), '_', ':'}, do(collect(c))],
  loop([element-of($name-char, c), do(collect(c))])
end collector name;

//    [6]    Names       ::=    Name (S Name)*
//
define meta names(name, s)
  scan-name(name), loop([scan-s(s), scan-name(name)])
end meta names;

//    [7]    Nmtoken     ::=    (NameChar)+
//
define meta nmtoken(c)
  element-of($name-char, c), loop(element-of($name-char, c))
end meta nmtoken;

//    [8]    Nmtokens    ::=    Nmtoken (S Nmtoken)*
//    
define meta nmtokens(token, s)
  scan-nmtoken(token), loop([scan-s(s), scan-nmtoken(token)])
end meta nmtokens;

// Literals
//
// [9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
//                     |  "'" ([^%&'] | PEReference | Reference)* "'"
//
define constant not-in-set? = complement(member?);

define macro collect-value-definer
{ define collect-value ?:name (?vars:*) (?properties:*)
    ?single:expression, ?double:expression => ?meta:*
  end }
 => { define collector ?name(c, ?vars)
        {['"',
          loop({[test(rcurry(?properties, ?double), c),
                 do(?=collect(c))], ?meta}),
          '"'],
         ["'",
          loop({[test(rcurry(?properties, ?single), c), 
                 do(?=collect(c))], ?meta}), 
          "'"]}, []
       end collector; }
  properties:
    { } => { not-in-set? }
    { #key ?test:expression } => { ?test }
end macro collect-value-definer;

// This hard-little collector is in need of a good refactoring.
// Unfortunately, I don't see an easy way for it to fit into the
// collect-value-definer macro.  It points to the need for a "declarative 
// continuation" (ouch!).  Or, how do a construct a predicate from other 
// predicates?  Meta-conjoin?
define collector entity-value(contents, s) => (str)
  { ["CDATA", scan-s(s)], [] },
  {["\"",
    loop({[scan-reference(contents), do(do(collect, contents))],
          [{[scan-s?(s), scan-element(contents)], 
            scan-double-char-data(contents)}, 
           do(collect(contents))]}), "\""],
   ["'",
    loop({[scan-reference(contents), do(do(collect, contents))],
          [{[scan-s?(s), scan-element(contents)],
            scan-single-char-data(contents)}, 
           do(collect(contents))]}), "'"]}, 
  {[scan-s(s), scan-comment-guts(s)], [] },
  []
end collector entity-value;

//    [10]    AttValue         ::=    '"' ([^<&"] | Reference)* '"'
//                                    |  "'" ([^<&'] | Reference)* "'"
//
define collect-value att-value(ref) ()
  "<&'", "<&\"" => scan-reference(ref)
end collect-value att-value;

//    [11]    SystemLiteral    ::=    ('"' [^"]* '"') | ("'" [^']* "'")
//
define collect-value system-literal() () "'", "\"" => { } end;

// [12] PubidLiteral ::=    '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
//
define collect-value pubid-literal() (test: method(x, y) member?(x, y) end)
  $pub-id-char-without-quotes, $pub-id-char => { }
end collect-value pubid-literal;

// [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
//    
define constant $pub-id-char-without-quotes =
  concatenate($letter, $digit, " \n\t\r-()+,./:=?;!*#@$_%");

define constant $pub-id-char =
  concatenate($pub-id-char-without-quotes, "'");

// Character Data
//
//    [14]    CharData    ::=    [^<&]* - ([^<&]* ']]>' [^<&]*)
//    

// Andreas: Now this is not very coherent to me. The section in the
// specification talks about CharData being used by the CDATA section,
// where in fact CDATA has it's own rule for matching characters
// [20]. So I don't see why ']]>' shouldn't be allowed, and I'm taking
// the liberty to implement this as
//
//    [14a]   CharData    ::=    [^<&]*
//
// which should be good enough (i.e., parses all compliant XML files,
// but fails to detect some non-compliancies).
//                                                --andreas
//
define macro collect-data-definer
{ define collect-data ?:name(?except:expression) end }
 => {  define collector ?name ## "-data" (c)
        => (make(<char-string>, text: as(<string>, ?=str)))
         [test(rcurry(not-in-set?, ?except), c), do(?=collect(c))],
         loop([test(rcurry(not-in-set?, ?except), c), do(?=collect(c))])
       end collector }
end macro collect-data-definer;

define collect-data char("<&") end;
define collect-data single-char("'&") end;
define collect-data double-char("\"&") end;

// Comments
// [15] Comment    ::=    '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
//
define meta comment(c) => (make(<comment>, comment: c))
  "<!", scan-comment-guts(c), ">"
end meta comment;

define collector comment-guts(c)
  "--", loop({["--", finish()], [accept(c), do(collect(c))]})
end collector comment-guts;

// Processing Instructions
// [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
//
// Doug: but I think processing instructions are, for the most case,
//       tags with attributes ... are there any exceptions?
define meta pi(s, target, attribs)
 => (make(<processing-instruction>, name: target, attributes: attribs))
  "<?", scan-pi-target(target), scan-s?(s), scan-xml-attributes(attribs), "?>"
end meta pi;

//    [17]    PITarget    ::=    Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
//
// Doesn't currently check whether the name is not xml. Shouldn't
// matter (fingers crossed).
//
define constant scan-pi-target = scan-name;

// CDATA Sections
// 
//    [18]    CDSect     ::=    CDStart CData CDEnd
//    [19]    CDStart    ::=    '<![CDATA['
//    [20]    CData      ::=    (Char* - (Char* ']]>' Char*))
//    [21]    CDEnd      ::=    ']]>'
//    
// Now here we require multi-byte lookahead. Great. And we need to
// rearrange things a little because we have no equivalent to the
// - operator. Basically, we're construcing our own non-greedy
// loop operator here.                        --andreas
//
define collector cd-sect(c)
 => (make(<char-string>, text: trim-string(str)))
  "<![CDATA[",
  loop({["]]>", finish()], [accept(c), do(collect(c))]})
end collector cd-sect;

// Prolog
// 
//    [22]    prolog         ::=    XMLDecl? Misc* (doctypedecl Misc*)?
//
define collector prolog(decl, misc, doctype) => (str)
  {[scan-xml-decl(decl), do(collect(decl))], []},
  {[scan-miscs(misc), do(do(collect, misc))], []},
  {scan-doctypedecl(doctype), []},
  {[scan-miscs(misc), do(do(collect, misc))], []}, []
end collector prolog;

// [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
//
define meta xml-decl(version-info, enc-decl, sd-decl, c, attribs)
 => (make(<processing-instruction>, name: "xml",
          attributes: as(<vector>, attribs)))
  set!(attribs, make(<deque>)),
  "<?xml", scan-version-info(version-info), (push(attribs, version-info)), 
  {[scan-encoding-decl(enc-decl), (push(attribs, enc-decl))], []},
  {[scan-sd-decl(sd-decl), (push(attribs, sd-decl))], []},
  scan-s?(c), "?>"
end meta xml-decl;

// [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'"
//                                      | '"' VersionNum '"')
//
define meta version-info(space, eq, version-num)
 => (make(<attribute>, name: "version", value: version-num))
  scan-s(space), "version", scan-eq(eq),
  {['\'', scan-version-num(version-num), '\''],
   ['"', scan-version-num(version-num), '"']}
end meta version-info;

//    [25]    Eq             ::=    S? '=' S?
//
define meta eq(sp) scan-s?(sp), '=', scan-s?(sp) end;

//    [26]    VersionNum     ::=    ([a-zA-Z0-9_.:] | '-')+
//
define collector version-num(c)
  loop([element-of($version-number, c), do(collect(c))])
end collector version-num;

//    [27]    Misc           ::=    Comment | PI | S
//
define meta misc(s, tag)
  {scan-comment(tag), scan-pi(tag), scan-s(s)}, []
end meta misc;

define collector miscs(misc, s) => (str)
  loop({[{scan-comment(misc), scan-pi(misc)}, do(collect(misc))], scan-s(s)})
end collector miscs;

// [28a] DeclSep ::= PEReference | S    [WFC: PE Between Declarations]
//
define meta decl-sep(pe-ref, s)
  { scan-pe-reference(pe-ref), scan-s(s)}, []
end meta decl-sep;

// Document Type Definition
// 
// [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? 
//                            ('[' (markupdecl | DeclSep)* ']' S?)? '>'
//                                    [VC: Root Element Type]
//                                    [WFC: External Subset]
//
// we'll add a returned dtd after developing the element-decl's etc.
// => (make(<dtd>, name: name, sys-id: sys-id, pub-id: pub-id, sys/pub: which))
define meta doctypedecl(s, name, sys-id, pub-id, which, markup)
  "<!DOCTYPE", scan-s(s), scan-name(name), 
  yes!(*defining-entities?*),
  {[scan-s(s), scan-external-id(sys-id, which, pub-id),
   do(let dtd-file = #f;
      aif(any?( method(dir)
		       let file = concatenate(dir, "/", sys-id);
		       file.file-exists? & file
		end, *dtd-paths*))
        dtd-file := it;
      else 
	error("%s is not on on the dtd search paths of %=", sys-id, *dtd-paths*)
      end aif;
// hokay, we've got an external-ID, now let's parse that document
// and bring in its entities and default attribute values.
      with-open-file(in = dtd-file, direction: #"input-output")
        scan-dtd-stuff(stream-contents(in, clear-contents?: #f))
      end)], 
   []},
  scan-s?(s), 
  {['[', scan-dtd-stuff(markup) , ']', scan-s?(s)], []}, ">",
  no!(*defining-entities?*)
end meta doctypedecl;

define meta dtd-stuff(markup, decls, misc)
  loop({scan-markupdecl(markup), scan-decl-sep(decls), scan-misc(misc)})
end meta dtd-stuff;

// [29] markupdecl ::= elementdecl | AttlistDecl | EntityDecl 
//                     | NotationDecl | PI | Comment
//                                    [VC: Proper Declaration/PE Nesting]
//                                    [WFC: PEs in Internal Subset]
//
define meta markupdecl(decl) => (decl)
  {scan-elementdecl(decl), scan-attlist-decl(decl),
   scan-entity-decl(decl), scan-notation-decl(decl), 
   scan-pi(decl), scan-comment(decl)}, []
end meta markupdecl;

// External Subset
// 
// [30] extSubset ::= TextDecl? extSubsetDecl
//
define meta ext-subset(text-decl, subset-decl)
 {scan-text-decl(text-decl), []}, scan-ext-subset-decl(subset-decl)
end meta ext-subset;

// [31] extSubsetDecl ::=  ( markupdecl | conditionalSect | DeclSep)* /* */
//
define meta ext-subset-decl(markup-decl, sect, decl-sep)
  loop({scan-markupdecl(markup-decl), 
        scan-conditional-sect(sect),
        scan-decl-sep(decl-sep)})
end meta ext-subset-decl;

// Standalone Document Declaration
// 
// [32] SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'")
//                                    | ('"' ('yes' | 'no') '"'))
//                                   [VC: Standalone Document Declaration]
//
define meta sd-decl(sp, eq, ans)
 => (make(<attribute>, name: "standalone", value: ans))
  scan-s(sp), "standalone", scan-eq(eq),
  {["'", scan-yesno(ans), "'"], ['"', scan-yesno(ans), '"']}
end meta sd-decl;

define meta yesno(ans) => (ans)
  {["yes", set!(ans, "yes")], ["no", set!(ans, "no")]}
end meta yesno;

//    (Productions 33 through 38 have been removed.)

// Element
// 
//    [39]    element    ::=    EmptyElemTag
//                              | STag content ETag [WFC: Element Type Match]
//                                                  [VC: Element Valid]
//
// here, for a bit more efficiency for most XML docs, we'll
// assume that tags usually have content, and, therefore, look
// for non-empty-element tags first when parsing.
define meta element(elt-name, attribs, content, etag)
  => (make-element(apply(if(*substitute-entities?* & ~ *defining-entities?*)
                           expand-entity
                         else 
                           identity 
                         end if, vector(content)),
                   elt-name, attribs, *modify?*))
  scan-beginning-of-tag(elt-name, attribs),
  { [">", scan-content(content), scan-etag(etag)],
    ["/>", no!(*last-tag-name*), set!(content, "")] }
end meta element;

// This make-element preserves capitalization of the tag-name
define method make-element(k :: <sequence>, n :: <symbol>, a :: <sequence>,
			   mod :: <boolean>) => (elt :: <element>)
  make(<element>, children: k, attributes: a,
       name: pop(*tag-name-with-proper-capitalization*))
end method make-element;

// Production 40 (Stag) has been removed (now handled by 39 (element)).

// a little bit of checking for element-name redundancy:
define variable *last-tag-name* :: false-or(<symbol>) = #f;
// ... and proper capitalization for tag names
define variable *tag-name-with-proper-capitalization* = make(<deque>);

// Helper method for parsing opening tags; also adds attributes with
// default values if there are ones specified in the DTD for this elt
define meta beginning-of-tag(elt-name, sym-name, attribs, s)
 => (sym-name, attribs)
  "<", scan-name(elt-name), scan-s?(s), scan-xml-attributes(attribs),
  (push(*tag-name-with-proper-capitalization*, elt-name)),
  set!(sym-name, as(<symbol>, elt-name)),
  do(attribs := add-default-attributes(sym-name, attribs);
     if(*last-tag-name* == sym-name)
       maybe-tag-mismatch(format-to-string("Opening same tag (<%s>)", 
                                           *last-tag-name*),
                          "Perhaps you meant to close the tag?",
                          index, string);
     end if;
     *last-tag-name* := sym-name)
end meta beginning-of-tag;

define function add-default-attributes(elt :: <symbol>, attrbs :: <vector>)
 => (new-attribs :: <vector>)
  concatenate(attrbs, 
              as(<vector>, reduce(method(result, default) 
                                      if(member?(default, attrbs,
                                                 test: method(x, y)
                                                           x.name == y.name
                                                       end))
                                        result
                                      else
                                        pair(default, result)
                                      end if
                                 end method, #(), 
                                 element($attrib-defaults, elt, default: #()))))
end function add-default-attributes;

define function maybe-tag-mismatch(msg :: <string>, hint :: <string>,
                                   idx :: <integer>, file :: <string>) => ()
  if(*show-warnings?*)
    format(*standard-error*, 
           "\nWARNING!  %s at file location %d.\n%s\n\n%s\n", msg, idx,
           hint, copy-sequence(file, start: max(idx - 80, 0),
                               end: min(idx + 80, file.size)));
  end if;
end function maybe-tag-mismatch;

define collector xml-attributes(attrib, sp) => (str)
  loop([scan-attribute(attrib), do(collect(attrib)), scan-s?(sp)])
end collector xml-attributes;

// [41] Attribute ::= Name Eq AttValue   [VC: Attribute Value Type]
//                                       [WFC: No External Entity References]
//                                       [WFC: No < in Attribute Values]
//
define meta attribute(name, eq, val)
 => (make(<attribute>, name: name, value: val))
  scan-name(name), scan-eq(eq), scan-xml-attribute-value(val)
end meta attribute;

// This function here scans the attribute value part of the
// attribute -- so this function will return "bar" for 
// <foo name="bar"/> (quotes are included)
define collect-value xml-attribute-value(c) () "'", "\"" => { } end;

// End-tag
// 
// [42] ETag ::= '</' Name S? '>'
//    
define meta etag(name, s) => (name)
  "</", scan-name(name),
  do(if(*last-tag-name* & *last-tag-name* ~== name)
       maybe-tag-mismatch("Close tag does not match open tag",
                          format-to-string("Change </%s> to </%s>",
                                           name, *last-tag-name*),
                          index, string);
     end if), no!(*last-tag-name*), scan-s?(s), ">"
end meta etag;

// Content of Elements
// 
// [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) 
//                             CharData?)*
//
define function empty-string?(str :: <string>) => (b :: <boolean>)
  (str.size = 0) | (every?(rcurry(member?, $space), str));
end function empty-string?;

define constant has-content? = complement(empty-string?);

define generic collapse-strings(str :: <sequence>, b :: <boolean>)
 => (ans :: <list>);

define method collapse-strings(str :: <sequence>, b :: <boolean>)
 => (ans :: <list>)
  as(<list>, str);
end method collapse-strings;

// we'll look for adjacent strings and combine them as one
// element in the new sequence
define method collapse-strings(str :: <sequence>, b == #t)
 => (ans :: <list>)
  if(str.empty?)
    #()
  else
    let ans = make(<deque>);
    let quit :: <integer> = str.size;

    local method find-last-adjacent-char-string(start :: <integer>)
           => stop :: <integer>;
      let stop = start;
      while(stop < quit & instance?(str[stop], <char-string>))
        stop := stop + 1; 
      end;
      stop;
    end method find-last-adjacent-char-string;

    local method condense-as-in-milk(start :: <integer>) => stop :: <integer>;
      if(instance?(str[start], <char-string>))
        let stop = find-last-adjacent-char-string(start);
        let new-str 
          = make(<char-string>, 
                 text: reduce(method(x, y) concatenate(x, y.text) end,
                              str[start].text,
                              copy-sequence(str, start: start + 1, 
					    end: stop)));
        push-last(ans, new-str);
        stop;
      else
        push-last(ans, str[start]);
        start + 1;
      end if;
    end method;

    let index :: <integer> = 0;
    while(index < quit)
      index := condense-as-in-milk(index);
    end;

    as(<list>, ans);
  end if;
end method collapse-strings;

define collector content(pi, comment, contents) 
 => (collapse-strings(str, *substitute-entities?*))
  {[scan-char-data(contents), 
   do(contents.text.has-content? & collect(contents))], []},
  loop({[{scan-element(contents), scan-cd-sect(contents)}, 
         do(collect(contents))],
        [scan-reference(contents), do(do(collect, contents))],
        [scan-pi(pi), 
	 do(unless(*ignore-instructions?*) collect(pi) end)],
        [scan-comment(comment),
	 do(unless(*ignore-instructions?*) collect(comment) end)],
        [scan-char-data(contents), 
         do(contents.text.has-content? & collect(contents))]})
end collector content;

// Production 44 (EmptyElemTag) is now handled by 39 (element)

// Element Type Declaration
// 
// [45] elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
//                      [VC: Unique Element Type Declaration]
//
define meta elementdecl(s, name, spec)
  "<!ELEMENT", scan-s(s), scan-name(name), scan-s(s),
  scan-contentspec(spec), scan-s?(s), ">"
end meta elementdecl;

//    [46]    contentspec    ::=    'EMPTY' | 'ANY' | Mixed | children
//
define meta contentspec(mixed, children)
  {"EMPTY", "ANY", scan-mixed(mixed), scan-children(children)}, []
end meta contentspec;

define meta opt-regexp(c) {{"?", "*", "+"}, []} end;

// Element-content Models
// 
//    [47]    children    ::=    (choice | seq) ('?' | '*' | '+')?
//
define meta children(choice, seq, c)
  {scan-choice(choice), scan-seq(seq)}, scan-opt-regexp(c)
end meta children;

//    [48]    cp          ::=    (Name | choice | seq) ('?' | '*' | '+')?
//
define meta cp(name, c, kids)
  { [scan-name(name), scan-opt-regexp(c)], scan-children(kids) }
end meta cp;

// the following meta functions all have the form:
// '(' S? meta (S? a-char S? meta)(*|+) S? ')'
define macro pattern-definer
{ define pattern ?:name(?char:expression) ?meta:* end }
 => { define meta ?name(s, ?=expr)
        "(", scan-s?(s), ?meta,
        loop([scan-s?(s), ?char, scan-s?(s), ?meta]),
        scan-s?(s), ")"
      end meta }
{ define pattern repeated ?:name(?char:expression) ?meta:* end }
 => { define meta ?name(s, ?=expr)
        "(", scan-s?(s), ?meta, scan-s?(s), ?char, scan-s?(s), 
        ?meta, loop([scan-s?(s), ?char, scan-s?(s), ?meta]),
        scan-s?(s), ")"
      end meta }
end macro pattern-definer;

// [49] choice ::=   '(' S? cp ( S? '|' S? cp )+ S? ')'
//             [VC: Proper Group/PE Nesting]
//
define pattern repeated choice("|") scan-cp(expr) end;

// [50] seq ::=    '(' S? cp ( S? ',' S? cp )* S? ')'
//                 [VC: Proper Group/PE Nesting]
//
define pattern seq(",") scan-cp(expr) end;

// Mixed-content Declaration
// 
// [51] Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
//                | '(' S? '#PCDATA' S? ')'
//                [VC: Proper Group/PE Nesting]
//                [VC: No Duplicate Types]
//
define meta mixed(s, name)
  "(", scan-s?(s), "#PCDATA",
  {[scan-s?(s), ")"], 
   [loop([scan-s?(s), "|", scan-s?(s), scan-name(name)]), 
    scan-s?(s), ")*"]}
end meta mixed;

// ----- Attribute-list Declaration -----

// here we store default values and also do validation storing on attrib vals
define constant $attrib-defaults = make(<multimap>);

// 
// [52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
//
define meta attlist-decl(s, elt-name, att-def)
  "<!ATTLIST", scan-s(s), scan-name(elt-name),
  set!(*last-tag-name*, as(<symbol>, elt-name)),
  loop(scan-att-def(att-def)), scan-s?(s), ">", no!(*last-tag-name*)
end meta attlist-decl;

define variable *last-attrib* = #f;

// [53] AttDef ::= S Name S AttType S DefaultDecl
//
define meta att-def(s, attrib-name, att-type, def)
  scan-s(s), scan-name(attrib-name), set!(*last-attrib*, attrib-name),
  scan-s(s), scan-att-type(att-type), scan-s(s), scan-default-decl(def),
  no!(*last-attrib*)
end meta att-def;

// Attribute Types
// 
// [54] AttType ::=    StringType | TokenizedType | EnumeratedType
//
define meta att-type(str)
 {scan-string-type(str), scan-tokenized-type(str), 
  scan-enumerated-type(str)}, []
end meta att-type;

//    [55]    StringType       ::=    'CDATA'
//
define meta string-type(c) "CDATA" end;

//    [56]    TokenizedType    ::=    'ID'        [VC: ID]
//                                                [VC: One ID per Element Type]
//                                                [VC: ID Attribute Default]
//                                    | 'IDREF'   [VC: IDREF]
//                                    | 'IDREFS'  [VC: IDREF]
//                                    | 'ENTITY'  [VC: Entity Name]
//                                    | 'ENTITIES'[VC: Entity Name]
//                                    | 'NMTOKEN' [VC: Name Token]
//                                    | 'NMTOKENS'[VC: Name Token]
//
define meta tokenized-type(c)
  {"ID", "IDREF", "IDREFS", "ENTITY", "ENTITIES", "NMTOKEN", 
   "NMTOKENS"}, []
end meta tokenized-type;

// Enumerated Attribute Types
// 
//    [57]    EnumeratedType    ::=    NotationType | Enumeration
//
define meta enumerated-type(note, enum)
  { scan-notation-type(note), scan-enumeration(enum) }, []
end meta enumerated-type;

// [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
//                       [VC: Notation Attributes]
//                       [VC: One Notation Per Element Type]
//                       [VC: No Notation on Empty Element]
//
// maybe someday add validation on notations
define pattern notation-helper("|") scan-name(expr) end;
define meta notation-type(s, name)
  "NOTATION", scan-s(s),  scan-notation-helper(s)
end meta notation-type;

// [59] Enumeration ::=  '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
//                       [VC: Enumeration]
//
define pattern enumeration("|") scan-nmtoken(expr) end;

// Attribute Defaults
// 
// [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED'
//                      | (('#FIXED' S)? AttValue) [VC: Required Attribute]
//                                                 [VC: Attribute Deflt Legal]
//                                                 [WFC: No < in Attribute Val]
//                                                 [VC: Fixed Attribute Deflt]
//
define meta default-decl(s, att-value)
  {"#REQUIRED", "#IMPLIED", 
   [{["#FIXED", scan-s(s)], []}, 
    scan-att-value(att-value), 
    do($attrib-defaults[*last-tag-name*] 
         := make(<attribute>, name: *last-attrib*, value: att-value))]}, []
end meta default-decl;

// Conditional Section
// 
//    [61]    conditionalSect       ::=    includeSect | ignoreSect
//
define meta conditional-sect(include, ignore)
  {scan-include-sect(include), scan-ignore-sect(ignore)}, []
end meta conditional-sect;

// [62] includeSect ::=    '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
//                         [VC: Proper Conditional Section/PE Nesting]
//
define meta include-sect(s, subset-decl)
  "<![", scan-s?(s), "INCLUDE", scan-s?(s), "[", 
  scan-ext-subset-decl(subset-decl), "]]>"
end meta include-sect;

// [63] ignoreSect ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>' 
//                     [VC: Proper Conditional Section/PE Nesting]
//
define meta ignore-sect(s, ignore-sect)
  "<![", scan-s?(s), "IGNORE", scan-s?(s), "[", 
  loop(scan-ignore-sect-contents(ignore-sect)), "]]>"
end meta ignore-sect;

// [64] ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
// Andreas: They are doing it again.
//
define meta ignore-sect-contents(ignore, ignore-sect)
  scan-ignore(ignore),
  loop(["<![", loop(scan-ignore-sect-contents(ignore-sect)), "]]>", 
        scan-ignore(ignore)])
end meta ignore-sect-contents;

// [65] Ignore ::= Char* - (Char* ('<![' | ']]>') Char*)
// like, wow, what a def ... FIXME!
define method scan-ignore(string, #key start = 0, end: stop)
  values(start, #t);  // DOUG changed index to start
end method scan-ignore;

// Character Reference
// 
//    [66]    CharRef    ::=    '&#' [0-9]+ ';'
//                              | '&#x' [0-9a-fA-F]+ ';' [WFC: Legal Character]
//
define collector int-char-ref(c)
 => (as(<character>, as(<string>, str).string-to-integer), 
        concatenate("#", as(<string>, str)))
  "&#", loop([element-of($digit, c), do(collect(c))]), ";"
end collector int-char-ref;

define collector hex-char-ref(c)
 => (as(<character>, string-to-integer(as(<string>, str), base: 16)),
     concatenate("#x", as(<string>, str)))
  "&#x", loop([element-of($hex-digit, c), do(collect(c))]), ";"
end collector hex-char-ref;

// I had had in the return value this:
// if(*substitute-entities?*)
            // make(<char-string>, text: make(<string>, size: 1, fill: char));
          // else
          // end if))
// but since I now handle char-refs in the expansion phase, I no longer need it
define meta char-ref(char, name)
 => (list(make(<char-reference>, name: name, char: char)))
  { scan-int-char-ref(char, name), scan-hex-char-ref(char, name) }, []
end meta char-ref;

// Entity Reference
// 
//    [67]    Reference      ::=    EntityRef | CharRef
define meta reference(ref) => (ref)
  { scan-entity-ref(ref), scan-char-ref(ref) }, []
end meta reference;

//-------------------------------------------------------
// entity tables
define variable *pe-refs* = make(<table>);
define variable *substitute-entities?* :: <boolean> = #t;
define variable *defining-entities?* :: <boolean> = #f;

define generic do-expand(obj :: <xml>) => (ans :: <list>);
define method do-expand(obj :: <xml>) => (ans :: <list>) list(obj); end;
define method do-expand(elt :: <element>) => (ans :: <list>)
// I'll ignore entities in the element attributes for now
  push(*tag-name-with-proper-capitalization*,
       elt.name-with-proper-capitalization);
  list(make-element(elt.node-children.expand-entity, elt.name, 
                    elt.attributes, *modify?*));
end method do-expand;

define method do-expand(ent :: <entity-reference>) => (ans :: <list>)
  expand-entity(ent.entity-value)
end method do-expand;

define method do-expand(ref :: <char-reference>) => (ans :: <list>)
  list(make(<char-string>, text: make(<string>, size: 1, fill: ref.char)));
end method do-expand;

define function expand-entity(val :: <sequence>) => (ans :: <list>)
  local method build-seq(blt :: <list>, org :: <list>)
    if(org.empty?)
      blt
    else
      build-seq(concatenate(blt, org.head.do-expand), org.tail);
    end if;
  end method;
  collapse-strings(as(type-for-copy(val), 
                      build-seq(#(), as(<list>, val))), #t);
end function expand-entity;

//    [68]    EntityRef      ::=    '&' Name ';'       [WFC: Entity Declared]
//                                                     [VC: Entity Declared]
//                                                     [WFC: Parsed Entity]
//                                                     [WFC: No Recursion]
define meta entity-ref(name)
 => (if(*substitute-entities?* & ~ *defining-entities?*)
      *entities*[as(<symbol>, name)].expand-entity;
     else
      list(make(<entity-reference>, name: name));
     end if)
  "&", scan-name(name), ";"
end meta entity-ref;

//    [69]    PEReference    ::=    '%' Name ';'       [VC: Entity Declared]
//                                                     [WFC: No Recursion]
//                                                     [WFC: In DTD]
//
define meta pe-reference(name) => (*pe-refs*[name])
  "%", scan-name(name), ";"
end meta pe-reference;

// Entity Declaration
// 
//    [70]    EntityDecl    ::=    GEDecl | PEDecl
define meta entity-decl(s, name) => (name)
  "<!ENTITY", scan-s(s), { scan-GE-Decl(name), scan-PE-Decl(name) },
  scan-s?(s), ">"
end meta entity-decl;

//    [71]    GEDecl        ::=    '<!ENTITY' S Name S EntityDef S? '>'
define meta ge-decl(name, s, def) => (name)
  scan-name(name), scan-s(s), scan-entity-def(def), scan-s?(s),
  set!(*entities*[as(<symbol>, name)], def)
end meta ge-decl;

//    [72]    PEDecl        ::=    '<!ENTITY' S '%' S Name S PEDef S? '>'
define meta pe-decl(name, s, def) => (name)
  "%", scan-s(s), scan-name(name), scan-s(s), scan-pe-def(def),
  set!(*pe-refs*[name], def)
end meta pe-decl;

//    [73]    EntityDef     ::=    EntityValue | (ExternalID NDataDecl?)
define meta entity-def(def, id) => (def)
  {scan-entity-value(def),
   [scan-external-id(id), {scan-n-data-decl(def), []}]}, []
end meta entity-def;

//    [74]    PEDef         ::=    EntityValue | ExternalID
//
define meta pe-def(def) => (def)
  { scan-entity-value(def), scan-external-id(def) }, []
end meta pe-def;

// External Entity Declaration
// 
//    [75]    ExternalID    ::=    'SYSTEM' S SystemLiteral
//                                 | 'PUBLIC' S PubidLiteral S SystemLiteral
//
define meta external-id(s, sys, pub, which) => (sys, which, pub)
  set!(which, #"system"),
  { "SYSTEM", [scan-public-id(pub), set!(which, #"public")] },
  scan-s(s), scan-system-literal(sys)
end meta external-id;

//    [76]    NDataDecl     ::=    S 'NDATA' S Name [VC: Notation Declared]
//
define meta n-data-decl(s, name) => (name)
  scan-s(s), "NDATA", scan-s(s), scan-name(name)
end meta n-data-decl;

// Text Declaration
// 
//    [77]    TextDecl    ::=    '<?xml' VersionInfo? EncodingDecl S? '?>'
//    
define meta text-decl(vers, s, decl)
  "<?xml", {scan-version-info(vers), []}, scan-encoding-decl(decl), 
  scan-s?(s), "?>"
end meta text-decl;

// Well-Formed External Parsed Entity
// 
//    [78]    extParsedEnt    ::=    TextDecl? content
//
define meta ext-parsed-ent(decl, content) => (content)
  {scan-text-decl(decl), []}, scan-content(content)
end meta ext-parsed-ent;

// Encoding Declaration
// 
// [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
// [81] EncName      ::=    [A-Za-z] ([A-Za-z0-9._] | '-')*
// Andreas: Encoding name contains only Latin characters
//
define meta encoding-decl(s, eq, name)
 => (make(<attribute>, name: "encoding", value: name))
  scan-s(s), "encoding", scan-eq(eq),
  {['\'', scan-encoding-name(name), '\''],
   ['"', scan-encoding-name(name), '"']}
end meta encoding-decl;

// fudging it here -- I say that encname can start with graphics, but
// that's wrong (esp since enc-name is a subset of version-num)
define constant scan-encoding-name = scan-version-num;

// Notation Declarations
// 
// [82] NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
//                       [VC: Unique Notation Name]
//
define meta notation-decl(s, name, ex, pub)
  "<!NOTATION", scan-s(s), scan-name(name), scan-s(s),
  { scan-external-id(ex), scan-public-id(pub) }, scan-s?(s), ">"
end meta notation-decl;

//    [83]    PublicID        ::=    'PUBLIC' S PubidLiteral
//
define meta public-id(s, pub) => (pub)
  "PUBLIC", scan-s(s), scan-pubid-literal(pub)
end meta public-id;

// Characters
// 
// [84] Letter   ::= BaseChar | Ideographic
// [85] BaseChar ::= [#x0041-#x005A] | [#x0061-#x007A] | [#x00C0-#x00D6]
//                   | [#x00D8-#x00F6] | [#x00F8-#x00FF] | [#x0100-#x0131]
//                   | [#x0134-#x013E] | [#x0141-#x0148] | [#x014A-#x017E]
//                   | [#x0180-#x01C3] | [#x01CD-#x01F0] | [#x01F4-#x01F5]
//                   | [#x01FA-#x0217] | [#x0250-#x02A8] | [#x02BB-#x02C1]
//                   | #x0386 | [#x0388-#x038A] | #x038C | [#x038E-#x03A1]
//                   | [#x03A3-#x03CE] | [#x03D0-#x03D6] | #x03DA | #x03DC
//                   | #x03DE | #x03E0 | [#x03E2-#x03F3] | [#x0401-#x040C]
//                   | [#x040E-#x044F] | [#x0451-#x045C] | [#x045E-#x0481]
//                   | [#x0490-#x04C4] | [#x04C7-#x04C8] | [#x04CB-#x04CC]
//                   | [#x04D0-#x04EB] | [#x04EE-#x04F5] | [#x04F8-#x04F9]
//                   | [#x0531-#x0556] | #x0559 | [#x0561-#x0586]
//                   | [#x05D0-#x05EA] | [#x05F0-#x05F2] | [#x0621-#x063A]
//                   | [#x0641-#x064A] | [#x0671-#x06B7] | [#x06BA-#x06BE]
//                   | [#x06C0-#x06CE] | [#x06D0-#x06D3] | #x06D5
//                   | [#x06E5-#x06E6] | [#x0905-#x0939] | #x093D
//                   | [#x0958-#x0961] | [#x0985-#x098C] | [#x098F-#x0990]
//                   | [#x0993-#x09A8] | [#x09AA-#x09B0] | #x09B2
//                   | [#x09B6-#x09B9] | [#x09DC-#x09DD] | [#x09DF-#x09E1]
//                   | [#x09F0-#x09F1] | [#x0A05-#x0A0A] | [#x0A0F-#x0A10]
//                   | [#x0A13-#x0A28] | [#x0A2A-#x0A30] | [#x0A32-#x0A33]
//                   | [#x0A35-#x0A36] | [#x0A38-#x0A39] | [#x0A59-#x0A5C]
//                   | #x0A5E | [#x0A72-#x0A74] | [#x0A85-#x0A8B] | #x0A8D
//                   | [#x0A8F-#x0A91] | [#x0A93-#x0AA8] | [#x0AAA-#x0AB0]
//                   | [#x0AB2-#x0AB3] | [#x0AB5-#x0AB9] | #x0ABD | #x0AE0
//                   | [#x0B05-#x0B0C] | [#x0B0F-#x0B10] | [#x0B13-#x0B28]
//                   | [#x0B2A-#x0B30] | [#x0B32-#x0B33] | [#x0B36-#x0B39]
//                   | #x0B3D | [#x0B5C-#x0B5D] | [#x0B5F-#x0B61]
//                   | [#x0B85-#x0B8A] | [#x0B8E-#x0B90] | [#x0B92-#x0B95]
//                   | [#x0B99-#x0B9A] | #x0B9C | [#x0B9E-#x0B9F]
//                   | [#x0BA3-#x0BA4] | [#x0BA8-#x0BAA] | [#x0BAE-#x0BB5]
//                   | [#x0BB7-#x0BB9] | [#x0C05-#x0C0C] | [#x0C0E-#x0C10]
//                   | [#x0C12-#x0C28] | [#x0C2A-#x0C33] | [#x0C35-#x0C39]
//                   | [#x0C60-#x0C61] | [#x0C85-#x0C8C] | [#x0C8E-#x0C90]
//                   | [#x0C92-#x0CA8] | [#x0CAA-#x0CB3] | [#x0CB5-#x0CB9]
//                   | #x0CDE | [#x0CE0-#x0CE1] | [#x0D05-#x0D0C]
//                   | [#x0D0E-#x0D10] | [#x0D12-#x0D28] | [#x0D2A-#x0D39]
//                   | [#x0D60-#x0D61] | [#x0E01-#x0E2E] | #x0E30
//                   | [#x0E32-#x0E33] | [#x0E40-#x0E45] | [#x0E81-#x0E82]
//                   | #x0E84 | [#x0E87-#x0E88] | #x0E8A | #x0E8D
//                   | [#x0E94-#x0E97] | [#x0E99-#x0E9F] | [#x0EA1-#x0EA3]
//                   | #x0EA5 | #x0EA7 | [#x0EAA-#x0EAB] | [#x0EAD-#x0EAE]
//                   | #x0EB0 | [#x0EB2-#x0EB3] | #x0EBD | [#x0EC0-#x0EC4]
//                   | [#x0F40-#x0F47] | [#x0F49-#x0F69] | [#x10A0-#x10C5]
//                   | [#x10D0-#x10F6] | #x1100 | [#x1102-#x1103]
//                   | [#x1105-#x1107] | #x1109 | [#x110B-#x110C]
//                   | [#x110E-#x1112] | #x113C | #x113E | #x1140 | #x114C
//                   | #x114E | #x1150 | [#x1154-#x1155] | #x1159
//                   | [#x115F-#x1161] | #x1163 | #x1165 | #x1167 | #x1169
//                   | [#x116D-#x116E] | [#x1172-#x1173] | #x1175 | #x119E
//                   | #x11A8 | #x11AB | [#x11AE-#x11AF] | [#x11B7-#x11B8]
//                   | #x11BA | [#x11BC-#x11C2] | #x11EB | #x11F0 | #x11F9
//                   | [#x1E00-#x1E9B] | [#x1EA0-#x1EF9] | [#x1F00-#x1F15]
//                   | [#x1F18-#x1F1D] | [#x1F20-#x1F45] | [#x1F48-#x1F4D]
//                   | [#x1F50-#x1F57] | #x1F59 | #x1F5B | #x1F5D
//                   | [#x1F5F-#x1F7D] | [#x1F80-#x1FB4] | [#x1FB6-#x1FBC]
//                   | #x1FBE | [#x1FC2-#x1FC4] | [#x1FC6-#x1FCC]
//                   | [#x1FD0-#x1FD3] | [#x1FD6-#x1FDB] | [#x1FE0-#x1FEC]
//                   | [#x1FF2-#x1FF4] | [#x1FF6-#x1FFC] | #x2126
//                   | [#x212A-#x212B] | #x212E | [#x2180-#x2182]
//                   | [#x3041-#x3094] | [#x30A1-#x30FA] | [#x3105-#x312C]
//                   | [#xAC00-#xD7A3]
//
// [86] Ideographic   ::= [#x4E00-#x9FA5] | #x3007 | [#x3021-#x3029]
// [87] CombiningChar ::= [#x0300-#x0345] | [#x0360-#x0361] | [#x0483-#x0486]
//                        | [#x0591-#x05A1] | [#x05A3-#x05B9] | [#x05BB-#x05BD]
//                        | #x05BF | [#x05C1-#x05C2] | #x05C4 | [#x064B-#x0652]
//                        | #x0670 | [#x06D6-#x06DC] | [#x06DD-#x06DF]
//                        | [#x06E0-#x06E4] | [#x06E7-#x06E8] | [#x06EA-#x06ED]
//                        | [#x0901-#x0903] | #x093C | [#x093E-#x094C] | #x094D
//                        | [#x0951-#x0954] | [#x0962-#x0963] | [#x0981-#x0983]
//                        | #x09BC | #x09BE | #x09BF | [#x09C0-#x09C4]
//                        | [#x09C7-#x09C8] | [#x09CB-#x09CD] | #x09D7
//                        | [#x09E2-#x09E3] | #x0A02 | #x0A3C | #x0A3E | #x0A3F
//                        | [#x0A40-#x0A42] | [#x0A47-#x0A48] | [#x0A4B-#x0A4D]
//                        | [#x0A70-#x0A71] | [#x0A81-#x0A83] | #x0ABC
//                        | [#x0ABE-#x0AC5] | [#x0AC7-#x0AC9] | [#x0ACB-#x0ACD]
//                        | [#x0B01-#x0B03] | #x0B3C | [#x0B3E-#x0B43]
//                        | [#x0B47-#x0B48] | [#x0B4B-#x0B4D] | [#x0B56-#x0B57]
//                        | [#x0B82-#x0B83] | [#x0BBE-#x0BC2] | [#x0BC6-#x0BC8]
//                        | [#x0BCA-#x0BCD] | #x0BD7 | [#x0C01-#x0C03]
//                        | [#x0C3E-#x0C44] | [#x0C46-#x0C48] | [#x0C4A-#x0C4D]
//                        | [#x0C55-#x0C56] | [#x0C82-#x0C83] | [#x0CBE-#x0CC4]
//                        | [#x0CC6-#x0CC8] | [#x0CCA-#x0CCD] | [#x0CD5-#x0CD6]
//                        | [#x0D02-#x0D03] | [#x0D3E-#x0D43] | [#x0D46-#x0D48]
//                        | [#x0D4A-#x0D4D] | #x0D57 | #x0E31 | [#x0E34-#x0E3A]
//                        | [#x0E47-#x0E4E] | #x0EB1 | [#x0EB4-#x0EB9]
//                        | [#x0EBB-#x0EBC] | [#x0EC8-#x0ECD] | [#x0F18-#x0F19]
//                        | #x0F35 | #x0F37 | #x0F39 | #x0F3E | #x0F3F
//                        | [#x0F71-#x0F84] | [#x0F86-#x0F8B] | [#x0F90-#x0F95]
//                        | #x0F97 | [#x0F99-#x0FAD] | [#x0FB1-#x0FB7] | #x0FB9
//                        | [#x20D0-#x20DC] | #x20E1 | [#x302A-#x302F] | #x3099
//                        | #x309A
//
// Okay, whoever's counting using Roman Numerals:  stop it, stop it, stop it!
// [88] Digit ::= [#x0030-#x0039] | [#x0660-#x0669] | [#x06F0-#x06F9]
//                | [#x0966-#x096F] | [#x09E6-#x09EF] | [#x0A66-#x0A6F]
//                | [#x0AE6-#x0AEF] | [#x0B66-#x0B6F] | [#x0BE7-#x0BEF]
//                | [#x0C66-#x0C6F] | [#x0CE6-#x0CEF] | [#x0D66-#x0D6F]
//                | [#x0E50-#x0E59] | [#x0ED0-#x0ED9] | [#x0F20-#x0F29]
//
// "Oh, yes, I'm the great Extender; woo-oo, woo-oo!"
// [89] Extender ::= #x00B7 | #x02D0 | #x02D1 | #x0387 | #x0640 | #x0E46
//                   | #x0EC6 | #x3005 | [#x3031-#x3035] | [#x309D-#x309E]
//                   | [#x30FC-#x30FE]
//    
// *Ahem* we punt here, allowing anything between quotes.
define collect-value encoding-info(eq, c) () "'", "\"" => { } end;

//-------------------------------------------------------
// assigning parents
define variable *parent* = #f;
define class <add-parents> (<xform-state>) end;

define method before-transform(node :: type-union(<element>, <document>),
                               state :: <add-parents>,
                               rep :: <integer>, str :: <stream>)
  *parent* := node;
end method before-transform;

define method transform(elt :: <element>, tag-name :: <symbol>,
                        state :: <add-parents>, str :: <stream>)
  elt.element-parent := *parent*;
  *parent* := elt;  // is this rebind superfluous?  Seems to work okay
  next-method();
end method transform;
