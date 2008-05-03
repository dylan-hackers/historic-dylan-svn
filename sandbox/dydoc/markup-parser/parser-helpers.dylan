module: markup-parser
synopsis: Parser simplification and print-out functions.


/// SYNOPSIS: Simplifies bullet-list etc. product.
define function combined-list-items (items :: <sequence>)
=> (simplified-items :: <sequence>)
   let opt-many-items = items[1];
   if (opt-many-items)
      // Each element of opt-many-items will be #(#f, item).
      concatenate(vector(items[0]),
                  map(method (seq-items) seq-items.last end, opt-many-items))
   else
      vector(items[0])
   end if
end function;


/// SYNOPSIS: Integrates the contents of a sequence.
///
/// For example, `#[a, #[b, c], #d]` would become `#[a, b, c, d]`.
define function integrate-sequences (items :: <sequence>)
=> (integrated-items :: <sequence>)
   let new-items = make(<deque>);
   for (item in items)
      if (instance?(item, <sequence>))
         new-items := concatenate!(new-items, item);
      else
         push-last(new-items, item);
      end if;
   end for;
   new-items
end function;


/// SYNOPSIS: Prepends a line to flush-content etc. product.
///
/// In the following example of an argument list section, the line
/// "Of course..." needs to be prepended to the paragraph following.
///
/// [EXAMPLE]
///  [1]: Of course, the translation here 
///  needs some work. Refer to these sources:
///    - Pratchett, Terry. "Dwarven Dieties"
/// [END]
///
/// ARGUMENTS:
///   words -  A sequence of markup words, or #f.
///   body -   A <token>, a sequence of <token>s, or #f.
///
/// VALUES:
///   new-body -  A sequence of <token>s or #f. The first element of 'new-body'
///               will have 'words', either merged with the first element of
///               'body', or in a separate <paragraph-token> with elements of
///               'body' following.
define generic prepend-words
   (words :: false-or(<sequence>),
    body :: false-or(type-union(<sequence>, <token>)))
=> (new-body);

define method prepend-words (words == #f, body :: false-or(<sequence>))
=> (new-body :: false-or(<sequence>))
   body
end method;

define method prepend-words (words == #f, body :: <token>)
=> (new-body :: <sequence>)
   vector(body)
end method;

define method prepend-words (words :: <sequence>, body == #f)
=> (new-body :: <sequence>)
   let para = make(<paragraph-token>, start: #f, end: #f);
   para.content := words;
   vector(para)
end method;

define method prepend-words (words :: <sequence>, body :: <sequence>)
=> (new-body :: <sequence>)
   replace-subsequence!(body,
         prepend-words(words, element(body, 0, default: #f)), start: 0, end: 1)
end method;

define method prepend-words (words :: <sequence>, body :: <paragraph-token>)
=> (new-body :: <sequence>)
   body.content := concatenate(words, body.content);
   vector(body)
end method;

define method prepend-words (words :: <sequence>, body :: <token>)
=> (new-body :: <sequence>)
   concatenate(prepend-words(words, #f), vector(body))
end method;


//// print-object

define method print-object (o :: <topic-directive-token>, s :: <stream>) => ()
   format(s, "{topic (%s) %=: %=}",
      o.topic-type, o.topic-title, o.content)
end method;

define method print-object (o :: <titled-topic-token>, s :: <stream>) => ()
   format(s, "{topic %= id %s: %=}",
      o.topic-title, o.topic-nickname, o.content)
end method;

/*
define method print-object (o :: <footnote-token>, s :: <stream>) => ()
end method;
*/

define method print-object
   (o :: type-union(<paragraph-token>, <paragraph-til-null-directive-token>),
    s :: <stream>)
=> ()
   format(s, "{para %=}", o.content)
end method;

define method print-object (o :: <marginal-code-block-token>, s :: <stream>) => ()
   format(s, "{code block %=}", o.content)
end method;

define method print-object (o :: <marginal-verbatim-block-token>, s :: <stream>) => ()
   format(s, "{verbatim block %=}", o.content)
end method;

define method print-object (o :: <bullet-list-token>, s :: <stream>) => ()
   format(s, "{bull list %=}", o.content)
end method;

define method print-object (o :: <numeric-list-token>, s :: <stream>) => ()
   format(s, "{num list from %=: %=}", o.list-start, o.content)
end method;

define method print-object (o :: <phrase-list-token>, s :: <stream>) => ()
   format(s, "{phrase list: %=}", o.content)
end method;

define method print-object (o :: <hyphenated-list-token>, s :: <stream>) => ()
   format(s, "{hyph list: %=}", o.content)
end method;

/*
define method print-object (o :: <figure-ref-line-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <content-ref-line-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <ditto-ref-line-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <api-list-ref-line-token>, s :: <stream>) => ()
end method;
*/

define method print-object (o :: <bracketed-raw-block-token>, s :: <stream>) => ()
   format(s, "{[%s] block: %=}", o.block-type, o.content)
end method;

define method print-object
   (o :: type-union(<bullet-list-first-item-token>, <bullet-list-item-token>,
                    <numeric-list-first-item-token>, <numeric-list-item-token>),
    s :: <stream>)
=> ()
   format(s, "{item: %=}", o.content)
end method;

define method print-object
   (o :: type-union(<hyphenated-list-item-token>, <phrase-list-item-token>),
    s :: <stream>)
=> ()
   format(s, "{item %=: %=}", o.item-label, o.content)
end method;

define method print-object (o :: <raw-line-token>, s :: <stream>) => ()
   if (o.index)
      format(s, "{line #%s %=}", o.index, o.content)
   else
      format(s, "{line %=}", o.content)
   end if
end method;

/*
define method print-object (o :: <perc-scale-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <mult-scale-token>, s :: <stream>) => ()
end method;
*/

define method print-object (o :: <paragraph-directive-token>, s :: <stream>) => ()
   format(s, "{%s %=}", o.directive-type, o.content)
end method;

define method print-object (o :: <link-directive-token>, s :: <stream>) => ()
   format(s, "{%s %=}", o.directive-type, o.link)
end method;

define method print-object (o :: <links-directive-token>, s :: <stream>) => ()
   format(s, "{%s %=}", o.directive-type, o.links)
end method;

define method print-object (o :: <indented-directive-token>, s :: <stream>) => ()
   format(s, "{%s %=}", o.directive-type, o.content)
end method;

/*
define method print-object (o :: <image-ref-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <marker-ref-token>, s :: <stream>) => ()
end method;
*/

define method print-object (o :: <bracketed-render-block-token>, s :: <stream>) => ()
   format(s, "{render %s %=}", o.block-type, o.content)
end method;

/*
define method print-object (o :: <synopsis-ref-token>, s :: <stream>) => ()
end method;
*/

define method print-object (o :: <quote-token>, s :: <stream>) => ()
   format(s, "{quote %s%s%s%s%s %s}",
      o.prequoted-content | "", o.open-quote, o.quoted-content | "",
      o.close-quote, o.postquoted-content | "", o.quote-spec | "(def)")
end method;

/*
define method print-object (o :: <ascii-line-token>, s :: <stream>) => ()
end method;
*/
