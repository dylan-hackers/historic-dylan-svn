module: %productions
synopsis: Implements a META parser for XML 1.0
based-on-work-by: Chris Double <chris@double.co.nz>
fleshed-out-by: Andreas Bogk <andreas@andreas.org>
synthesis: Douglas M. Auclair <doug@cotilliongroup.com> -- DMA
copyright: LGPL

/*
 * 
 * This library implements a parser for XML 1.0, using the META parser
 * technique.
 * 
 * Source for the specification was:
 * 
 *   http://www.w3.org/TR/2000/REC-xml-20001006
 * 
 * The parse-foo clauses contain a numeric reference to the associated
 * production in the specification.
 *
 * prerequisit:  the meta library (currently at gd/examples/meta/)
 * 
 * Useful for the understanding of this parser:
 *
 *   http://linux.rice.edu/~rahul/hbaker/Prag-Parse.html
 *
 */

//-------------------------------------------------------
// Macros to simplify the parsing task -- DMA
define macro parse-definer
{ define parse ?:name ( ?meta-vars:* ) => (?results:*) ?meta:* end } 
 => { parse-helper(?name, (?meta-vars), (?results), (?meta)) }
{ define parse ?:name (?meta-vars:*) ?meta:* end }
 => { parse-helper(?name, (?meta-vars), (#t), (?meta)) }
end macro parse-definer;

define macro parse-helper
{ parse-helper(?:name, (?meta-vars:*), (?results:*), (?meta:*)) }
 => { parse-builder(?name, ( meta-builder(?=string, ?=start, 
                                         (?meta-vars), 
                                         (?results), (?meta)))) }
end macro parse-helper;

define macro parse-builder
{ parse-builder(?:name, (?built-meta:*)) }
 => { define method "parse-" ## ?name (?=string, #key ?=start = 0, end: stop)
        ?built-meta
      end; }
end macro parse-builder;

// Doug wrote this macro for collecting strings
define macro collector-definer
{ define collector ?:name (?vars:*) => (?results:*) ?meta:* end }
 => { parse-builder(?name, 
       (with-collector into-vector ?=str, collect: ?=collect;
          meta-builder(?=string, ?=start, (?vars), (?results), (?meta));
        end with-collector)) }
{ define collector ?:name (?vars:*) ?meta:* end }
 => { parse-builder(?name, 
       (with-collector into-vector str, collect: ?=collect;
          meta-builder(?=string, ?=start, (?vars), (as(<string>, str)), (?meta));
        end with-collector)) }
end macro collector-definer;

define macro collect-value-definer
{ define collect-value ?:name (?vars:*)
    (#key ?test:variable = not-in-set?)
    ?single:expression, ?double:expression => ?meta:*
  end }
 => { define collector ?name(c, ?vars)
        {['"',
          loop({[test(rcurry(?test, ?double), c),
                 do(?=collect(c))], ?meta}),
          '"'],
         ["'",
          loop({[test(rcurry(?test, ?single), c), 
                 do(?=collect(c))], ?meta}), 
          "'"]}, []
       end collector; }
end macro collect-value-definer;

define macro meta-builder
{ meta-builder(?str:expression, ?start:expression, (?vars:*), (?results:*), (?meta:*)) }
 => {  with-meta-syntax parse-string (?str, start: ?start, pos: index)
         variables(?vars);
         [ ?meta ];
         values(index, ?results);
       end with-meta-syntax; }
end macro meta-builder;

//-------------------------------------------------------
// entity tables
define variable *entities* = make(<table>);
define variable *pe-refs* = make(<table>);
define variable *substitute-entities?* :: <boolean> = #t;
define variable *defining-entities?* :: <boolean> = #f;

define method entity-value(ent :: <entity-reference>)
 => (val :: <sequence>)
  *entities*[ent.name];
end method entity-value;

define method do-expand(obj :: <xml>) list(obj); end;
define method do-expand(elt :: <element>)
// I'll ignore entities in the element attributes for now
  list(make(<element>, name: elt.name, 
            attributes: elt.element-attributes,
            children: elt.node-children.expand-entity));
end method do-expand;

define method do-expand(ent :: <entity-reference>) 
  expand-entity(ent.entity-value)
end method do-expand;

define function expand-entity(val :: <sequence>)
 => (seq :: <sequence>)
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

//-------------------------------------------------------
// Productions

// Document
// 
//    [1]    document    ::=    prolog element Misc*
//
define function parse-document(doc :: <string>, 
                               #key start = 0, end: stop, 
                               substitute-entities? = #t)
 => (stripped-tree :: <document>)
  *entities* := make(<table>);
  *defining-entities?* := #f;
  *substitute-entities?* := substitute-entities?;
  let (index, document) = parse-document-helper(doc, start: start, end: stop);
  document;
end function parse-document;

define parse document-helper(prolog, elemnt, misc) =>
 (make(<document>, name: elemnt.name, children: vector(elemnt)))
   parse-prolog(prolog), parse-element(elemnt), loop(parse-misc(misc))
end parse document-helper;

// Character Range
//
//    [2]    Char    ::=    #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF] /* any Unicode character, excluding the surrogate blocks,
//    FFFE, and FFFF. */
//    
// FIXME: we're cheating here, by using UFT8 and just allowing anything.
//
define constant <char> = <character>;

// White Space
//
//    [3]    S    ::=    (#x20 | #x9 | #xD | #xA)+
//    
define constant <space> =
  one-of(as(<character>, #x20), as(<character>, #x9), as(<character>, #xd),
         as(<character>, #x0a));

define parse s(c) type(<space>, c), loop(type(<space>, c)) end;
define parse s?(c) loop(parse-s(c)) end;

// Names and Tokens
// 
//    [4]    NameChar    ::=    Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
//
define constant <name-char> = type-union(<version-number>, singleton('-'));

//    [5]    Name        ::=    (Letter | '_' | ':') (NameChar)*
//
define collector name(c) => (as(<symbol>, as(<string>, str)))
  [{type(<letter>, c), '_', ':'}, do(collect(c))],
  loop([type(<name-char>, c), do(collect(c))])
end collector name;

//    [6]    Names       ::=    Name (S Name)*
//
define parse names(name, s)
  parse-name(name), loop([parse-s(s), parse-name(name)])
end parse names;

//    [7]    Nmtoken     ::=    (NameChar)+
//
define parse nmtoken(c)
  type(<name-char>, c), loop(type(<name-char>, c))
end parse nmtoken;

//    [8]    Nmtokens    ::=    Nmtoken (S Nmtoken)*
//    
define parse nmtokens(token, s)
  parse-nmtoken(token), loop([parse-s(s), parse-nmtoken(token)])
end parse nmtokens;

// Literals
//
//    [9]     EntityValue      ::=    '"' ([^%&"] | PEReference | Reference)* '"'
//                                    |  "'" ([^%&'] | PEReference | Reference)* "'"
//
define constant not-in-set? = complement(member?);

define collector entity-value(contents, s) => (str)
  {["\"",
    loop({[parse-reference(contents), do(do(collect, contents))],
          [{[parse-s?(s), parse-element(contents)], 
            parse-double-char-data(contents)}, 
           do(collect(contents))]}), "\""],
   ["'",
    loop({[parse-reference(contents), do(do(collect, contents))],
          [{[parse-s?(s), parse-element(contents)],
            parse-single-char-data(contents)}, 
           do(collect(contents))]}), "'"]}, 
  []
end collector entity-value;


//    [10]    AttValue         ::=    '"' ([^<&"] | Reference)* '"'
//                                    |  "'" ([^<&'] | Reference)* "'"
//
define collect-value att-value(ref) ()
  "<&'", "<&\"" => parse-reference(ref)
end collect-value att-value;

//    [11]    SystemLiteral    ::=    ('"' [^"]* '"') | ("'" [^']* "'")
//
define collect-value system-literal() () "'", "\"" => { } end;

//    [12]    PubidLiteral     ::=    '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
//
define collect-value pubid-literal() 
  (test: method(x, y) subtype?(singleton(x), y) end)
  <pub-id-char-without-quotes>, <pub-id-char> => { }
end;

//    [13]    PubidChar        ::=    #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
//    
// I really wonder if this isn't abuse of the Dylan type system...
//
define constant <pub-id-char-without-quotes> =
  type-union(<letter>, <digit>, 
             one-of(as(<character>, #x20), as(<character>, #xa), as(<character>, #xd),
                    '-', '(', ')', '+', ',', '.', '/', ':', '=',
                    '?', ';', '!', '*', '#', '@', '$', '_', '%'));

define constant <pub-id-char> =
  type-union(<pub-id-char-without-quotes>, singleton('\''));

// Character Data
//
//    [14]    CharData    ::=    [^<&]* - ([^<&]* ']]>' [^<&]*)
//    

// Now this is not very coherent to me. The section in the
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

//    [15]    Comment    ::=    '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
//
define parse comment(c)
  "<!--", loop({["-->", finish()], type(<char>, c)})
end;

// Processing Instructions

//    [16]    PI          ::=    '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
//
define parse pi(c, target, space)
  "<?", parse-pi-target(target), parse-s(space), 
  loop({["?>", finish()], type(<char>, c)})
end parse pi;

//    [17]    PITarget    ::=    Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
//
// Doesn't currently check whether the name is not xml. Shouldn't
// matter (fingers crossed).
//
define constant parse-pi-target = parse-name;

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
 => (make(<char-string>, text: as(<string>, str)))
  "<![CDATA[",
  loop({["]]>", finish()], [type(<char>, c), do(collect(c))]})
end collector cd-sect;

// Prolog
// 
//    [22]    prolog         ::=    XMLDecl? Misc* (doctypedecl Misc*)?
//
define parse prolog(decl, misc, doctype)
  {parse-xml-decl(decl), []}, loop(parse-misc(misc)),
  {[parse-doctypedecl(doctype), loop(parse-misc(misc))], []}
end parse prolog;

//    [23]    XMLDecl        ::=    '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
//
define parse xml-decl(version-info, encoding-decl, sd-decl, c)
  "<?xml", parse-version-info(version-info),     
  {parse-encoding-decl(encoding-decl), []},
  {parse-sd-decl(sd-decl), []},
  parse-s?(c), "?>"
end parse xml-decl;

//    [24]    VersionInfo    ::=    S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')/* */
//
define parse version-info(space, eq, version-num)
  parse-s(space), "version", parse-eq(eq),
  {['\'', parse-version-num(version-num), '\''],
   ['"', parse-version-num(version-num), '"']}
end parse version-info;

//    [25]    Eq             ::=    S? '=' S?
//
define parse eq(sp) parse-s?(sp), '=', parse-s?(sp) end;

//    [26]    VersionNum     ::=    ([a-zA-Z0-9_.:] | '-')+
//
define collector version-num(c)
  loop([type(<version-number>, c), do(collect(c))])
end collector version-num;

//    [27]    Misc           ::=    Comment | PI | S
//
define parse misc(comment, pi, s)
  {parse-comment(comment), parse-pi(pi), parse-s(s)}, []
end parse misc;

//    [28a]    DeclSep        ::=    PEReference | S                                                                 [WFC: PE Between Declarations]
//                                                                                                                   /* */
//
define parse decl-sep(pe-ref, s)
  { parse-pe-reference(pe-ref), parse-s(s)}, []
end parse decl-sep;

// Document Type Definition
// 
//    [28]     doctypedecl    ::=    '<!DOCTYPE' S Name (S ExternalID)? S? ('[' (markupdecl | DeclSep)* ']' S?)? '>' [VC: Root Element Type]
//                                                                                                                   [WFC: External Subset]
//
define parse doctypedecl(s, name, id, markup, decl-sep)
  yes!(*defining-entities?*),
  "<!DOCTYPE", parse-s(s), parse-name(name), 
  {[parse-s(s), parse-external-id(id),
// hokay, we've got an external-ID, now let's parse that document
// and bring in its (I hope only) entities
   do(with-open-file(in = id) parse-dtd-stuff(in.stream-contents) end)], 
   []},
  parse-s?(s), 
  {['[', parse-dtd-stuff(markup) , ']', parse-s?(s)], []}, ">",
  no!(*defining-entities?*)
end parse doctypedecl;

define parse dtd-stuff(markup, decls, misc)
  loop({parse-markupdecl(markup), parse-decl-sep(decls), parse-misc(misc)})
end parse dtd-stuff;

//    [29]     markupdecl     ::=    elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment            [VC: Proper Declaration/PE Nesting]
//                                                                                                                   [WFC: PEs in Internal Subset]
//
define parse markupdecl(decl)
  {parse-elementdecl(decl), parse-attlist-decl(decl),
   parse-entity-decl(decl), parse-notation-decl(decl), 
   parse-pi(decl), parse-comment(decl)}, []
end parse markupdecl;

// External Subset
// 
//    [30]    extSubset        ::=    TextDecl? extSubsetDecl
//
define parse ext-subset(text-decl, subset-decl)
 {parse-text-decl(text-decl), []}, parse-ext-subset-decl(subset-decl)
end parse ext-subset;

//    [31]    extSubsetDecl    ::=    ( markupdecl | conditionalSect | DeclSep)* /* */
//
define parse ext-subset-decl(markup-decl, sect, decl-sep)
  loop({parse-markupdecl(markup-decl), 
        parse-conditional-sect(sect),
        parse-decl-sep(decl-sep)})
end parse ext-subset-decl;

// Standalone Document Declaration
// 
//    [32]    SDDecl    ::=    S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"')) [VC: Standalone Document Declaration]
//
define parse sd-decl(sp, eq)
  parse-s(sp), "standalone", parse-eq(eq),
// DOUG: another '/" example
  {['\'', {"yes", "no"}, '\''], ['"', {"yes", "no"}, '"']}
end parse sd-decl;

//    (Productions 33 through 38 have been removed.)

// This function here parses the attribute value part of the
// attribute -- so this function will parse "bar" for 
// <foo name="bar"/> (quotes are included)
define collect-value xml-attribute(c) () "'", "\"" => { } end;

// Element
// 
//    [39]    element    ::=    EmptyElemTag
//                              | STag content ETag [WFC: Element Type Match]
//                                                  [VC: Element Valid]
//
// here, for a bit more efficiency for most XML docs, we'll
// assume that tags usually have content, and, therefore, look
// for non-empty-element tags first when parsing.
define parse element(name, attribs, content, etag)
 => (make(<element>, children: content, name: name, attributes: attribs))
  {[parse-stag(name, attribs), parse-content(content), parse-etag(etag)],
   [parse-empty-elem-tag(name, attribs), set!(content, "")]}, []
end parse element;

// Start-tag
// 
//    [40]    STag         ::=    '<' Name (S Attribute)* S? '>' [WFC: Unique Att Spec]
//
define parse stag(elt, attribs) => (elt, attribs)
  parse-beginning-of-tag(elt, attribs), ">"
end parse stag;

//    [41]    Attribute    ::=    Name Eq AttValue               [VC: Attribute Value Type]
//                                                               [WFC: No External Entity References]
//                                                               [WFC: No < in Attribute Values]
//
define parse attribute(name, eq, value)
 => (make(<attribute>, name: name, value: value))
  parse-name(name), parse-eq(eq), parse-xml-attribute(value)
end parse attribute;

// End-tag
// 
//    [42]    ETag    ::=    '</' Name S? '>'
//    
define parse etag(name, s) => (name)
  "</", parse-name(name), parse-s?(s), ">"
end parse etag;

// Content of Elements
// 
//    [43]    content    ::=    CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
//
define function empty-string?(str :: <string>) => (b :: <boolean>)
  (str.size = 0) 
  | (every?(method(x) subtype?(singleton(x), <space>) end, str));
end function empty-string?;

define constant has-content? = complement(empty-string?);

define method collapse-strings(str :: <sequence>, b :: <boolean>)
  str;
end method collapse-strings;

// we'll look for adjacent strings and combine them as one
// element in the new sequence
define method collapse-strings(str :: <sequence>, b == #t)
  let ans = make(<deque>);
  let quit :: <integer> = str.size;

  local method find-last-adjacent-char-string(start :: <integer>) => stop :: <integer>;
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
                            copy-sequence(str, start: start + 1, end: stop)));
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

  as(type-for-copy(str), ans);
end method collapse-strings;

define collector content(ignor, contents) 
 => (collapse-strings(str, *substitute-entities?*))
  {[parse-char-data(contents), do(contents.text.has-content? & collect(contents))], []},
  loop({[{parse-element(contents), parse-cd-sect(contents)}, 
         do(collect(contents))],
        [parse-reference(contents), do(do(collect, contents))],
        parse-pi(ignor), parse-comment(ignor),
        [parse-char-data(contents), 
         do(contents.text.has-content? & collect(contents))]})
end collector content;

// helper method for parsing opening tags
define parse beginning-of-tag(elt, attribs, s) => (elt, attribs)
  "<", parse-name(elt), parse-s?(s), parse-xml-attributes(attribs)
end parse beginning-of-tag;

// Tags for Empty Elements
// 
//    [44]    EmptyElemTag    ::=    '<' Name (S Attribute)* S? '/>' [WFC: Unique Att Spec]
//
// This is not optimal because we parse the whole tag data twice if it
// is not an empty-elem-tag. Need to unify this with [39].
define parse empty-elem-tag(elt, attribs) => (elt, attribs)
  parse-beginning-of-tag(elt, attribs), "/>"
end parse empty-elem-tag;

// Element Type Declaration
// 
//    [45]    elementdecl    ::=    '<!ELEMENT' S Name S contentspec S? '>' [VC: Unique Element Type Declaration]
//
define parse elementdecl(s, name, spec)
  "<!ELEMENT", parse-s(s), parse-name(name), parse-s(s),
  parse-contentspec(spec), parse-s?(s), ">"
end parse elementdecl;

//    [46]    contentspec    ::=    'EMPTY' | 'ANY' | Mixed | children
//
define parse contentspec(mixed, children)
  {"EMPTY", "ANY", parse-mixed(mixed), parse-children(children)}, []
end parse contentspec;

define parse opt-regexp(c) {{"?", "*", "+"}, []} end;

// Element-content Models
// 
//    [47]    children    ::=    (choice | seq) ('?' | '*' | '+')?
//
define parse children(choice, seq, c)
  {parse-choice(choice), parse-seq(seq)}, parse-opt-regexp(c)
end parse children;

//    [48]    cp          ::=    (Name | choice | seq) ('?' | '*' | '+')?
//
define parse cp(name, c, kids)
  { [parse-name(name), parse-opt-regexp(c)], parse-children(kids) }
end parse cp;

// the following parse functions all have the form:
// '(' S? meta (S? a-char S? meta)(*|+) S? ')'
define macro pattern-definer
{ define pattern ?:name(?char:expression) ?meta:* end }
 => { define parse ?name(s, ?=expr)
        "(", parse-s?(s), ?meta,
        loop([parse-s?(s), ?char, parse-s?(s), ?meta]),
        parse-s?(s), ")"
      end parse }
{ define pattern repeated ?:name(?char:expression) ?meta:* end }
 => { define parse ?name(s, ?=expr)
        "(", parse-s?(s), ?meta, parse-s?(s), ?char, parse-s?(s), 
        ?meta, loop([parse-s?(s), ?char, parse-s?(s), ?meta]),
        parse-s?(s), ")"
      end parse }
end macro pattern-definer;

//    [49]    choice      ::=   '(' S? cp ( S? '|' S? cp )+ S? ')'
//                                                                       [VC: Proper Group/PE Nesting]
//
define pattern repeated choice("|") parse-cp(expr) end;

//    [50]    seq         ::=    '(' S? cp ( S? ',' S? cp )* S? ')'
//                                                                       [VC: Proper Group/PE Nesting]
//
define pattern seq(",") parse-cp(expr) end;

// Mixed-content Declaration
// 
//    [51]    Mixed    ::=    '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
//                            | '(' S? '#PCDATA' S? ')'                  [VC: Proper Group/PE Nesting]
//                                                                       [VC: No Duplicate Types]
//
define parse mixed(s, name)
  "(", parse-s?(s), "#PCDATA",
  {[parse-s?(s), ")"], 
   [loop([parse-s?(s), "|", parse-s?(s), parse-name(name)]), 
    parse-s?(s), ")*"]}
end parse mixed;

// Attribute-list Declaration
// 
//    [52]    AttlistDecl    ::=    '<!ATTLIST' S Name AttDef* S? '>'
//
define parse attlist-decl(s, name, att-def)
  "<!ATTLIST", parse-s(s), parse-name(name),
    loop(parse-att-def(att-def)), parse-s?(s), ">"
end parse attlist-decl;

//    [53]    AttDef         ::=    S Name S AttType S DefaultDecl
//
define parse att-def(s, name, att-type, def)
  parse-s(s), parse-name(name), parse-s(s), parse-att-type(att-type),
  parse-s(s), parse-default-decl(def)
end parse;

// Attribute Types
// 
//    [54]    AttType          ::=    StringType | TokenizedType | EnumeratedType
//
define parse att-type(str) 
 {parse-string-type(str), parse-tokenized-type(str), 
  parse-enumerated-type(str)}, []
end parse att-type;

//    [55]    StringType       ::=    'CDATA'
//
define parse string-type(c) "CDATA" end;

//    [56]    TokenizedType    ::=    'ID'                                        [VC: ID]
//                                                                                [VC: One ID per Element Type]
//                                                                                [VC: ID Attribute Default]
//                                    | 'IDREF'                                   [VC: IDREF]
//                                    | 'IDREFS'                                  [VC: IDREF]
//                                    | 'ENTITY'                                  [VC: Entity Name]
//                                    | 'ENTITIES'                                [VC: Entity Name]
//                                    | 'NMTOKEN'                                 [VC: Name Token]
//                                    | 'NMTOKENS'                                [VC: Name Token]
//
define parse tokenized-type(c)
  {"ID", "IDREF", "IDREFS", "ENTITY", "ENTITIES", "NMTOKEN", 
   "NMTOKENS"}, []
end parse tokenized-type;

// Enumerated Attribute Types
// 
//    [57]    EnumeratedType    ::=    NotationType | Enumeration
//
define parse enumerated-type(note, enum)
  { parse-notation-type(note), parse-enumeration(enum) }, []
end parse enumerated-type;

//    [58]    NotationType      ::=    'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')' [VC: Notation Attributes]
//                                                                                       [VC: One Notation Per Element Type]
//                                                                                       [VC: No Notation on Empty Element]
//
define pattern notation-helper("|") parse-name(expr) end;
define parse notation-type(s, name)
  "NOTATION", parse-s(s),  parse-notation-helper(s)
end parse notation-type;

//    [59]    Enumeration       ::=    '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'        [VC: Enumeration]
//
define pattern enumeration("|") parse-nmtoken(expr) end;

// Attribute Defaults
// 
//    [60]    DefaultDecl    ::=    '#REQUIRED' | '#IMPLIED'
//                                  | (('#FIXED' S)? AttValue) [VC: Required Attribute]
//                                                             [VC: Attribute Default Legal]
//                                                             [WFC: No < in Attribute Values]
//                                                             [VC: Fixed Attribute Default]
//
define parse default-decl(s, att-value)
  {"#REQUIRED", "#IMPLIED", 
   [{["#FIXED", parse-s(s)], []}, parse-att-value(att-value)]}, []
end parse default-decl;

// Conditional Section
// 
//    [61]    conditionalSect       ::=    includeSect | ignoreSect
//
define parse conditional-sect(include, ignore)
  {parse-include-sect(include), parse-ignore-sect(ignore)}, []
end parse conditional-sect;

//    [62]    includeSect           ::=    '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'      /* */
//                                                                                            [VC: Proper Conditional Section/PE Nesting]
//
define parse include-sect(s, subset-decl)
  "<![", parse-s?(s), "INCLUDE", parse-s?(s), "[", 
  parse-ext-subset-decl(subset-decl), "]]>"
end parse include-sect;

//    [63]    ignoreSect            ::=    '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>' 
//                                                                                            [VC: Proper Conditional Section/PE Nesting]
//
define parse ignore-sect(s, ignore-sect)
  "<![", parse-s?(s), "IGNORE", parse-s?(s), "[", 
  loop(parse-ignore-sect-contents(ignore-sect)), "]]>"
end parse ignore-sect;

//    [64]    ignoreSectContents    ::=    Ignore ('<![' ignoreSectContents ']]>' Ignore)*
// They are doing it again.
//
define parse ignore-sect-contents(ignore, ignore-sect)
  parse-ignore(ignore),
  loop(["<![", loop(parse-ignore-sect-contents(ignore-sect)), "]]>", parse-ignore(ignore)])
end parse ignore-sect-contents;

//    [65]    Ignore                ::=    Char* - (Char* ('<![' | ']]>') Char*)
//  like, wow, what a def ... FIXME!
define method parse-ignore(string, #key start = 0, end: stop)
  values(start, #t);  // DOUG changed index to start
end method parse-ignore;

// Character Reference
// 
//    [66]    CharRef    ::=    '&#' [0-9]+ ';'
//                              | '&#x' [0-9a-fA-F]+ ';' [WFC: Legal Character]
//
define collector int-char-ref(c)
 => (as(<character>, as(<string>, str).string-to-integer), 
        concatenate("#", as(<string>, str)))
  "&#", loop([type(<digit>, c), do(collect(c))]), ";"
end collector int-char-ref;

define collector hex-char-ref(c)
 => (as(<character>, string-to-integer(as(<string>, str), base: 16)),
     concatenate("#x", as(<string>, str)))
  "&#x", loop([type(<hex-digit>, c), do(collect(c))]), ";"
end collector hex-char-ref;

define parse char-ref(char, name)
 => (list(if(*substitute-entities?*)
            make(<char-string>, text: make(<string>, size: 1, fill: char));
          else
            make(<char-reference>, name: as(<symbol>, name), char: char)
          end if))
  { parse-int-char-ref(char, name), parse-hex-char-ref(char, name) }, []
end parse char-ref;

// Entity Reference
// 
//    [67]    Reference      ::=    EntityRef | CharRef
define parse reference(ref) => (ref)
  { parse-entity-ref(ref), parse-char-ref(ref) }, []
end parse reference;

//    [68]    EntityRef      ::=    '&' Name ';'       [WFC: Entity Declared]
//                                                     [VC: Entity Declared]
//                                                     [WFC: Parsed Entity]
//                                                     [WFC: No Recursion]
define parse entity-ref(name)
 => (if(*substitute-entities?* & ~ *defining-entities?*)
      *entities*[name].expand-entity;
     else
      list(make(<entity-reference>, name: name));
     end if)
  "&", parse-name(name), ";"
end parse entity-ref;

//    [69]    PEReference    ::=    '%' Name ';'       [VC: Entity Declared]
//                                                     [WFC: No Recursion]
//                                                     [WFC: In DTD]
//
define parse pe-reference(name) => (*pe-refs*[name])
  "%", parse-name(name), ";"
end parse pe-reference;

// Entity Declaration
// 
//    [70]    EntityDecl    ::=    GEDecl | PEDecl
define parse entity-decl(name) => (name)
  { parse-GE-Decl(name), parse-PE-Decl(name) }, []
end parse entity-decl;

//    [71]    GEDecl        ::=    '<!ENTITY' S Name S EntityDef S? '>'
define parse ge-decl(name, s, def) => (name)
  "<!ENTITY", parse-s(s), parse-name(name), parse-s(s), 
  parse-entity-def(def), parse-s?(s), ">", 
  do(*entities*[name] := def)
end parse ge-decl;

//    [72]    PEDecl        ::=    '<!ENTITY' S '%' S Name S PEDef S? '>'
define parse pe-decl(name, s, def) => (name)
  "<!ENTITY", parse-s(s), "%", parse-s(s), parse-name(name),
  parse-s(s), parse-pe-def(def), parse-s?(s), ">", 
  do(*pe-refs*[name] := def)
end parse pe-decl;

//    [73]    EntityDef     ::=    EntityValue | (ExternalID NDataDecl?)
define parse entity-def(def, id) => (def)
  {parse-entity-value(def),
   [parse-external-id(id), {parse-n-data-decl(def), []}]}, []
end parse entity-def;

//    [74]    PEDef         ::=    EntityValue | ExternalID
//
define parse pe-def(def) => (def)
  { parse-entity-value(def), parse-external-id(def) }, []
end parse pe-def;

// External Entity Declaration
// 
//    [75]    ExternalID    ::=    'SYSTEM' S SystemLiteral
//                                 | 'PUBLIC' S PubidLiteral S SystemLiteral
//
define parse external-id(s, sys, pub) => (sys)
  { "SYSTEM", parse-public-id(pub) }, parse-s(s),
  parse-system-literal(sys)
end parse external-id;

//    [76]    NDataDecl     ::=    S 'NDATA' S Name                          [VC: Notation Declared]
//
define parse n-data-decl(s, name) => (name)
  parse-s(s), "NDATA", parse-s(s), parse-name(name)
end parse n-data-decl;

// Text Declaration
// 
//    [77]    TextDecl    ::=    '<?xml' VersionInfo? EncodingDecl S? '?>'
//    
define parse text-decl(vers, s, decl)
  "<?xml", {parse-version-info(vers), []}, parse-encoding-decl(decl), 
  parse-s?(s), "?>"
end parse text-decl;

// Well-Formed External Parsed Entity
// 
//    [78]    extParsedEnt    ::=    TextDecl? content
//
define parse ext-parsed-ent(decl, content) => (content)
  {parse-text-decl(decl), []}, parse-content(content)
end parse ext-parsed-ent;

// Encoding Declaration
// 
//    [80]    EncodingDecl    ::=    S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
//    [81]    EncName         ::=    [A-Za-z] ([A-Za-z0-9._] | '-')*                      /* Encoding name contains only Latin characters */
//
define parse encoding-decl(s, eq, name) => (name)
  parse-s(s), "encoding", parse-eq(eq),
  {['\'', parse-encoding-name(name), '\''],
   ['"', parse-encoding-name(name), '"']}
end parse encoding-decl;

// fudging it here -- I say that encname can start with graphics, but
// that's wrong (esp since enc-name is a subset of version-num
define constant parse-encoding-name = parse-version-num;

// Notation Declarations
// 
//    [82]    NotationDecl    ::=    '<!NOTATION' S Name S (ExternalID | PublicID) S? '>' [VC: Unique Notation Name]
//
define parse notation-decl(s, name, ex, pub)
  "<!NOTATION", parse-s(s), parse-name(name), parse-s(s),
  { parse-external-id(ex), parse-public-id(pub) }, parse-s?(s), ">"
end parse notation-decl;

//    [83]    PublicID        ::=    'PUBLIC' S PubidLiteral
//
define parse public-id(s, pub) => (pub)
  "PUBLIC", parse-s(s), parse-pubid-literal(pub)
end parse public-id;

// Characters
// 
//    [84]    Letter    ::=    BaseChar | Ideographic
//    [85]    BaseChar    ::=    [#x0041-#x005A] | [#x0061-#x007A] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x00FF] | [#x0100-#x0131] | [#x0134-#x013E]
//    | [#x0141-#x0148] | [#x014A-#x017E] | [#x0180-#x01C3] | [#x01CD-#x01F0] | [#x01F4-#x01F5] | [#x01FA-#x0217] | [#x0250-#x02A8] | [#x02BB-#x02C1]
//    | #x0386 | [#x0388-#x038A] | #x038C | [#x038E-#x03A1] | [#x03A3-#x03CE] | [#x03D0-#x03D6] | #x03DA | #x03DC | #x03DE | #x03E0 | [#x03E2-#x03F3]
//    | [#x0401-#x040C] | [#x040E-#x044F] | [#x0451-#x045C] | [#x045E-#x0481] | [#x0490-#x04C4] | [#x04C7-#x04C8] | [#x04CB-#x04CC] | [#x04D0-#x04EB]
//    | [#x04EE-#x04F5] | [#x04F8-#x04F9] | [#x0531-#x0556] | #x0559 | [#x0561-#x0586] | [#x05D0-#x05EA] | [#x05F0-#x05F2] | [#x0621-#x063A]
//    | [#x0641-#x064A] | [#x0671-#x06B7] | [#x06BA-#x06BE] | [#x06C0-#x06CE] | [#x06D0-#x06D3] | #x06D5 | [#x06E5-#x06E6] | [#x0905-#x0939] | #x093D
//    | [#x0958-#x0961] | [#x0985-#x098C] | [#x098F-#x0990] | [#x0993-#x09A8] | [#x09AA-#x09B0] | #x09B2 | [#x09B6-#x09B9] | [#x09DC-#x09DD]
//    | [#x09DF-#x09E1] | [#x09F0-#x09F1] | [#x0A05-#x0A0A] | [#x0A0F-#x0A10] | [#x0A13-#x0A28] | [#x0A2A-#x0A30] | [#x0A32-#x0A33] | [#x0A35-#x0A36]
//    | [#x0A38-#x0A39] | [#x0A59-#x0A5C] | #x0A5E | [#x0A72-#x0A74] | [#x0A85-#x0A8B] | #x0A8D | [#x0A8F-#x0A91] | [#x0A93-#x0AA8] | [#x0AAA-#x0AB0]
//    | [#x0AB2-#x0AB3] | [#x0AB5-#x0AB9] | #x0ABD | #x0AE0 | [#x0B05-#x0B0C] | [#x0B0F-#x0B10] | [#x0B13-#x0B28] | [#x0B2A-#x0B30] | [#x0B32-#x0B33]
//    | [#x0B36-#x0B39] | #x0B3D | [#x0B5C-#x0B5D] | [#x0B5F-#x0B61] | [#x0B85-#x0B8A] | [#x0B8E-#x0B90] | [#x0B92-#x0B95] | [#x0B99-#x0B9A] | #x0B9C
//    | [#x0B9E-#x0B9F] | [#x0BA3-#x0BA4] | [#x0BA8-#x0BAA] | [#x0BAE-#x0BB5] | [#x0BB7-#x0BB9] | [#x0C05-#x0C0C] | [#x0C0E-#x0C10] | [#x0C12-#x0C28]
//    | [#x0C2A-#x0C33] | [#x0C35-#x0C39] | [#x0C60-#x0C61] | [#x0C85-#x0C8C] | [#x0C8E-#x0C90] | [#x0C92-#x0CA8] | [#x0CAA-#x0CB3] | [#x0CB5-#x0CB9]
//    | #x0CDE | [#x0CE0-#x0CE1] | [#x0D05-#x0D0C] | [#x0D0E-#x0D10] | [#x0D12-#x0D28] | [#x0D2A-#x0D39] | [#x0D60-#x0D61] | [#x0E01-#x0E2E] | #x0E30
//    | [#x0E32-#x0E33] | [#x0E40-#x0E45] | [#x0E81-#x0E82] | #x0E84 | [#x0E87-#x0E88] | #x0E8A | #x0E8D | [#x0E94-#x0E97] | [#x0E99-#x0E9F]
//    | [#x0EA1-#x0EA3] | #x0EA5 | #x0EA7 | [#x0EAA-#x0EAB] | [#x0EAD-#x0EAE] | #x0EB0 | [#x0EB2-#x0EB3] | #x0EBD | [#x0EC0-#x0EC4] | [#x0F40-#x0F47]
//    | [#x0F49-#x0F69] | [#x10A0-#x10C5] | [#x10D0-#x10F6] | #x1100 | [#x1102-#x1103] | [#x1105-#x1107] | #x1109 | [#x110B-#x110C] | [#x110E-#x1112]
//    | #x113C | #x113E | #x1140 | #x114C | #x114E | #x1150 | [#x1154-#x1155] | #x1159 | [#x115F-#x1161] | #x1163 | #x1165 | #x1167 | #x1169
//    | [#x116D-#x116E] | [#x1172-#x1173] | #x1175 | #x119E | #x11A8 | #x11AB | [#x11AE-#x11AF] | [#x11B7-#x11B8] | #x11BA | [#x11BC-#x11C2] | #x11EB
//    | #x11F0 | #x11F9 | [#x1E00-#x1E9B] | [#x1EA0-#x1EF9] | [#x1F00-#x1F15] | [#x1F18-#x1F1D] | [#x1F20-#x1F45] | [#x1F48-#x1F4D] | [#x1F50-#x1F57]
//    | #x1F59 | #x1F5B | #x1F5D | [#x1F5F-#x1F7D] | [#x1F80-#x1FB4] | [#x1FB6-#x1FBC] | #x1FBE | [#x1FC2-#x1FC4] | [#x1FC6-#x1FCC] | [#x1FD0-#x1FD3]
//    | [#x1FD6-#x1FDB] | [#x1FE0-#x1FEC] | [#x1FF2-#x1FF4] | [#x1FF6-#x1FFC] | #x2126 | [#x212A-#x212B] | #x212E | [#x2180-#x2182] | [#x3041-#x3094]
//    | [#x30A1-#x30FA] | [#x3105-#x312C] | [#xAC00-#xD7A3]
//    [86]    Ideographic    ::=    [#x4E00-#x9FA5] | #x3007 | [#x3021-#x3029]
//    [87]    CombiningChar    ::=    [#x0300-#x0345] | [#x0360-#x0361] | [#x0483-#x0486] | [#x0591-#x05A1] | [#x05A3-#x05B9] | [#x05BB-#x05BD] | #x05BF
//    | [#x05C1-#x05C2] | #x05C4 | [#x064B-#x0652] | #x0670 | [#x06D6-#x06DC] | [#x06DD-#x06DF] | [#x06E0-#x06E4] | [#x06E7-#x06E8] | [#x06EA-#x06ED]
//    | [#x0901-#x0903] | #x093C | [#x093E-#x094C] | #x094D | [#x0951-#x0954] | [#x0962-#x0963] | [#x0981-#x0983] | #x09BC | #x09BE | #x09BF
//    | [#x09C0-#x09C4] | [#x09C7-#x09C8] | [#x09CB-#x09CD] | #x09D7 | [#x09E2-#x09E3] | #x0A02 | #x0A3C | #x0A3E | #x0A3F | [#x0A40-#x0A42]
//    | [#x0A47-#x0A48] | [#x0A4B-#x0A4D] | [#x0A70-#x0A71] | [#x0A81-#x0A83] | #x0ABC | [#x0ABE-#x0AC5] | [#x0AC7-#x0AC9] | [#x0ACB-#x0ACD]
//    | [#x0B01-#x0B03] | #x0B3C | [#x0B3E-#x0B43] | [#x0B47-#x0B48] | [#x0B4B-#x0B4D] | [#x0B56-#x0B57] | [#x0B82-#x0B83] | [#x0BBE-#x0BC2]
//    | [#x0BC6-#x0BC8] | [#x0BCA-#x0BCD] | #x0BD7 | [#x0C01-#x0C03] | [#x0C3E-#x0C44] | [#x0C46-#x0C48] | [#x0C4A-#x0C4D] | [#x0C55-#x0C56]
//    | [#x0C82-#x0C83] | [#x0CBE-#x0CC4] | [#x0CC6-#x0CC8] | [#x0CCA-#x0CCD] | [#x0CD5-#x0CD6] | [#x0D02-#x0D03] | [#x0D3E-#x0D43] | [#x0D46-#x0D48]
//    | [#x0D4A-#x0D4D] | #x0D57 | #x0E31 | [#x0E34-#x0E3A] | [#x0E47-#x0E4E] | #x0EB1 | [#x0EB4-#x0EB9] | [#x0EBB-#x0EBC] | [#x0EC8-#x0ECD]
//    | [#x0F18-#x0F19] | #x0F35 | #x0F37 | #x0F39 | #x0F3E | #x0F3F | [#x0F71-#x0F84] | [#x0F86-#x0F8B] | [#x0F90-#x0F95] | #x0F97 | [#x0F99-#x0FAD]
//    | [#x0FB1-#x0FB7] | #x0FB9 | [#x20D0-#x20DC] | #x20E1 | [#x302A-#x302F] | #x3099 | #x309A
//    [88]    Digit    ::=    [#x0030-#x0039] | [#x0660-#x0669] | [#x06F0-#x06F9] | [#x0966-#x096F] | [#x09E6-#x09EF] | [#x0A66-#x0A6F] | [#x0AE6-#x0AEF]
//    | [#x0B66-#x0B6F] | [#x0BE7-#x0BEF] | [#x0C66-#x0C6F] | [#x0CE6-#x0CEF] | [#x0D66-#x0D6F] | [#x0E50-#x0E59] | [#x0ED0-#x0ED9] | [#x0F20-#x0F29]
//    [89]    Extender    ::=    #x00B7 | #x02D0 | #x02D1 | #x0387 | #x0640 | #x0E46 | #x0EC6 | #x3005 | [#x3031-#x3035] | [#x309D-#x309E] | [#x30FC-#x30FE]
//    
//
define collect-value encoding-info(eq, c) () "'", "\"" => { } end;

define collector xml-attributes(attr-name, eq, attr-val, sp) => (str)
  loop([parse-name(attr-name), parse-eq(eq),
        parse-xml-attribute(attr-val), parse-s?(sp),
        do(collect(make(<attribute>, name: attr-name, value: attr-val)))])
end collector xml-attributes;

