Module: latin1-entities
synopsis: embeds the latin1 entities into the parser
author: Douglas M. Auclair
copyright: (c) 2002, GD license

define macro entities-definer
{ define entities ?:name ?entities:* end } =>
 { define function initialize-latin1-entities() => ()
     ?name := make(<table>);
     do(method(x) ?name[x.head] := x.tail end, list(?entities));
   end function initialize-latin1-entities }

 entities:
   { } => { }
   { ?data; ... } => { ?data, ... }

 data:
   { <!entity ?ent:name cdata ?charref:expression ?comment-body:* ?:token }
     => { list(?#"ent", make(<char-reference>, char: ?charref.as-char,
			     name: copy-sequence(?charref, start: 1,
						 end: ?charref.size - 1))) }
/*
 comment-body:
  { } => { }
  { ?:name ... } => { ?name, ... }
*/
end macro entities-definer;

define function as-char(s :: <string>) => (c :: <character>)
  as(<character>, copy-sequence(s, start: 2, end: s.size - 1).string-to-integer)
end function as-char;

// we did the above macro so we can do this:
define entities *entities*
<!ENTITY nbsp   CDATA "&#160;"  no-break space >;
<!ENTITY iexcl  CDATA "&#161;"  inverted exclamation mark >;
<!ENTITY cent   CDATA "&#162;"  cent sign >;
<!ENTITY pound  CDATA "&#163;"  pound sterling sign >;
<!ENTITY curren CDATA "&#164;"  general currency sign >;
<!ENTITY yen    CDATA "&#165;"  yen sign >;
<!ENTITY brvbar CDATA "&#166;"  broken (vertical) bar >;
<!ENTITY sect   CDATA "&#167;"  section sign >;
<!ENTITY uml    CDATA "&#168;"  umlaut (dieresis) >;
<!ENTITY copy   CDATA "&#169;"  copyright sign >;
<!ENTITY ordf   CDATA "&#170;"  ordinal indicator, feminine >;
<!ENTITY laquo  CDATA "&#171;"  angle quotation mark, left >;
<!ENTITY not    CDATA "&#172;"  not sign >;
<!ENTITY shy    CDATA "&#173;"  soft hyphen >;
<!ENTITY reg    CDATA "&#174;"  registered sign >;
<!ENTITY macr   CDATA "&#175;"  macron >;
<!ENTITY deg    CDATA "&#176;"  degree sign >;
<!ENTITY plusmn CDATA "&#177;"  plus-or-minus sign >;
<!ENTITY sup2   CDATA "&#178;"  superscript two >;
<!ENTITY sup3   CDATA "&#179;"  superscript three >;
<!ENTITY acute  CDATA "&#180;"  acute accent >;
<!ENTITY micro  CDATA "&#181;"  micro sign >;
<!ENTITY para   CDATA "&#182;"  pilcrow (paragraph sign) >;
<!ENTITY middot CDATA "&#183;"  middle dot >;
<!ENTITY cedil  CDATA "&#184;"  cedilla >;
<!ENTITY sup1   CDATA "&#185;"  superscript one >;
<!ENTITY ordm   CDATA "&#186;"  ordinal indicator, masculine >;
<!ENTITY raquo  CDATA "&#187;"  angle quotation mark, right >;
<!ENTITY frac14 CDATA "&#188;"  fraction one-quarter >;
<!ENTITY frac12 CDATA "&#189;"  fraction one-half >;
<!ENTITY frac34 CDATA "&#190;"  fraction three-quarters >;
<!ENTITY iquest CDATA "&#191;"  inverted question mark >;
<!ENTITY Agrave CDATA "&#192;"  capital A, grave accent >;
<!ENTITY Aacute CDATA "&#193;"  capital A, acute accent >;
<!ENTITY Acirc  CDATA "&#194;"  capital A, circumflex accent >;
<!ENTITY Atilde CDATA "&#195;"  capital A, tilde >;
<!ENTITY Auml   CDATA "&#196;"  capital A, dieresis or umlaut mark >;
<!ENTITY Aring  CDATA "&#197;"  capital A, ring >;
<!ENTITY AElig  CDATA "&#198;"  capital AE diphthong (ligature) >;
<!ENTITY Ccedil CDATA "&#199;"  capital C, cedilla >;
<!ENTITY Egrave CDATA "&#200;"  capital E, grave accent >;
<!ENTITY Eacute CDATA "&#201;"  capital E, acute accent >;
<!ENTITY Ecirc  CDATA "&#202;"  capital E, circumflex accent >;
<!ENTITY Euml   CDATA "&#203;"  capital E, dieresis or umlaut mark >;
<!ENTITY Igrave CDATA "&#204;"  capital I, grave accent >;
<!ENTITY Iacute CDATA "&#205;"  capital I, acute accent >;
<!ENTITY Icirc  CDATA "&#206;"  capital I, circumflex accent >;
<!ENTITY Iuml   CDATA "&#207;"  capital I, dieresis or umlaut mark >;
<!ENTITY ETH    CDATA "&#208;"  capital Eth, Icelandic >;
<!ENTITY Ntilde CDATA "&#209;"  capital N, tilde >;
<!ENTITY Ograve CDATA "&#210;"  capital O, grave accent >;
<!ENTITY Oacute CDATA "&#211;"  capital O, acute accent >;
<!ENTITY Ocirc  CDATA "&#212;"  capital O, circumflex accent >;
<!ENTITY Otilde CDATA "&#213;"  capital O, tilde >;
<!ENTITY Ouml   CDATA "&#214;"  capital O, dieresis or umlaut mark >;
<!ENTITY times  CDATA "&#215;"  multiply sign >;
<!ENTITY Oslash CDATA "&#216;"  capital O, slash >;
<!ENTITY Ugrave CDATA "&#217;"  capital U, grave accent >;
<!ENTITY Uacute CDATA "&#218;"  capital U, acute accent >;
<!ENTITY Ucirc  CDATA "&#219;"  capital U, circumflex accent >;
<!ENTITY Uuml   CDATA "&#220;"  capital U, dieresis or umlaut mark >;
<!ENTITY Yacute CDATA "&#221;"  capital Y, acute accent >;
<!ENTITY THORN  CDATA "&#222;"  capital THORN, Icelandic >;
<!ENTITY szlig  CDATA "&#223;"  small sharp s, German (sz ligature) >;
<!ENTITY agrave CDATA "&#224;"  small a, grave accent >;
<!ENTITY aacute CDATA "&#225;"  small a, acute accent >;
<!ENTITY acirc  CDATA "&#226;"  small a, circumflex accent >;
<!ENTITY atilde CDATA "&#227;"  small a, tilde >;
<!ENTITY auml   CDATA "&#228;"  small a, dieresis or umlaut mark >;
<!ENTITY aring  CDATA "&#229;"  small a, ring >;
<!ENTITY aelig  CDATA "&#230;"  small ae diphthong (ligature) >;
<!ENTITY ccedil CDATA "&#231;"  small c, cedilla >;
<!ENTITY egrave CDATA "&#232;"  small e, grave accent >;
<!ENTITY eacute CDATA "&#233;"  small e, acute accent >;
<!ENTITY ecirc  CDATA "&#234;"  small e, circumflex accent >;
<!ENTITY euml   CDATA "&#235;"  small e, dieresis or umlaut mark >;
<!ENTITY igrave CDATA "&#236;"  small i, grave accent >;
<!ENTITY iacute CDATA "&#237;"  small i, acute accent >;
<!ENTITY icirc  CDATA "&#238;"  small i, circumflex accent >;
<!ENTITY iuml   CDATA "&#239;"  small i, dieresis or umlaut mark >;
<!ENTITY eth    CDATA "&#240;"  small eth, Icelandic >;
<!ENTITY ntilde CDATA "&#241;"  small n, tilde >;
<!ENTITY ograve CDATA "&#242;"  small o, grave accent >;
<!ENTITY oacute CDATA "&#243;"  small o, acute accent >;
<!ENTITY ocirc  CDATA "&#244;"  small o, circumflex accent >;
<!ENTITY otilde CDATA "&#245;"  small o, tilde >;
<!ENTITY ouml   CDATA "&#246;"  small o, dieresis or umlaut mark >;
<!ENTITY divide CDATA "&#247;"  divide sign >;
<!ENTITY oslash CDATA "&#248;"  small o, slash >;
<!ENTITY ugrave CDATA "&#249;"  small u, grave accent >;
<!ENTITY uacute CDATA "&#250;"  small u, acute accent >;
<!ENTITY ucirc  CDATA "&#251;"  small u, circumflex accent >;
<!ENTITY uuml   CDATA "&#252;"  small u, dieresis or umlaut mark >;
<!ENTITY yacute CDATA "&#253;"  small y, acute accent >;
<!ENTITY thorn  CDATA "&#254;"  small thorn, Icelandic >;
<!ENTITY yuml   CDATA "&#255;"  small y, dieresis or umlaut mark >;

<!ENTITY quot    CDATA "&#34;"     quotation mark, =apl quote, u+0022 ISOnum >;
<!ENTITY amp     CDATA "&#38;"     ampersand, u+0026 ISOnum >;
<!ENTITY lt      CDATA "&#60;"     less-than sign, u+003C ISOnum >;
<!ENTITY gt      CDATA "&#62;"     greater-than sign, u+003E ISOnum >;
end entities;

// The above list was obtained from:
// <!-- http://www.w3.org/TR/WD-html40-970708/sgml/entities.html -->;
// (actually I got it second-hand and formatted as XML from
// http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/clocc/clocc/src/cllib/)
// and modified with the following command:
// $ perl -n -e 's/\-\-//g; chomp; print "$_;\n"' entities.xml > out.txt
// it also was truncated to the ascii set (no char-references above 255)

