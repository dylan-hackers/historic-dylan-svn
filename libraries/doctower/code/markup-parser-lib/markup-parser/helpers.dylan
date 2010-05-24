module: markup-parser
synopsis: Parser simplification and print-out functions.


/// Synopsis: Records the characteristics of a topic level style.
define class <topic-level-style> (<object>)
   slot line-character :: <character>, init-keyword: #"char";
   slot underline? :: <boolean>, init-keyword: #"under";
   slot midline? :: <boolean>, init-keyword: #"mid";
   slot overline? :: <boolean>, init-keyword: #"over";
end class;

define method \= (style1 :: <topic-level-style>, style2 :: <topic-level-style>)
=> (equal? :: <boolean>)
   style1.line-character = style2.line-character &
   style1.underline? = style2.underline? &
   style1.midline? = style2.midline? &
   style1.overline? = style2.overline?
end method;


/// Synopsis: The underline/overline style of a section (as opposed to topic).
/// TODO: Get this from configs.
define constant $section-style =
      make(<topic-level-style>, char: '-', under: #f, mid: #t, over: #f);


/** Synopsis: Create <topic-level-style> from title attributes. */
define function topic-level-style-from-attributes ()
=> (style :: <topic-level-style>)
   make(<topic-level-style>,
        char: attr(title-line-char),
        over: attr(title-overline?),
        mid: attr(title-midline?),
        under: attr(title-underline?));
end function;


/**
Synopsis: Simplifies products of the form (item, #f | ((..., item), ...))
such as bullet lists.
*/
define function first-item-and-last-subelements (items :: <sequence>)
=> (simplified-items :: <sequence>)
   let opt-many-items = items[1];
   apply(vector, items[0], map(last, opt-many-items))
end function;


/**
Synopsis: Flatten nested sequences. For example, `#[a, #[b, c, #[d]]]` would
become `#[a, b, c, d]`.
*/
define function integrate-sequences (items :: <sequence>)
=> (integrated-items :: <sequence>)
   let new-items = make(<deque>);
   for (item in items)
      if (instance?(item, <sequence>) & ~instance?(item, <string>))
         new-items := concatenate!(new-items, integrate-sequences(item));
      else
         push-last(new-items, item);
      end if;
   end for;
   new-items
end function;


/** Synopsis: Ensures presence or absence of indent/dedent tokens. */
define function check-paired-indentation (ind, ded, fail)
   unless (~ind & ~ded | ind | ded)
      fail(make(<parse-failure>, expected: "matching indentation and dedentation"))
   end unless;
end function;


/** Synopsis: Ensures "[end x]" text is correct. */
define function check-end-spec-text (text :: false-or(<symbol>), fail)
   let spec-text = attr(bracketed-spec-text, default: #f);
   unless (text = #f | text = spec-text)
      fail(make(<parse-failure>, expected:
                format-to-string("\"[end %s]\"", spec-text)))
   end unless
end function;


/** Synopsis: Ensures consistent bullet list items. */
define function check-bullet-char (bullet :: <character>, fail)
   let bullet-char = attr(bullet-char, default: ' ');
   case
      bullet-char = ' ' =>
         attr(bullet-char) := bullet;
      bullet-char ~= bullet =>
         fail(make(<parse-failure>, expected:
                   format-to-string("bullet \"%c\"", bullet-char)));
   end case
end function;


/** Synopsis: Ensures consistent numeric list item style. */
define function check-numeric-list-marker (tokens, fail)
   let ordinal-separator :: false-or(<symbol>) =
         attr(ordinal-separator, default: #f);
   let ordinal-type :: false-or(<class>) =
         attr(ordinal-type, default: #f);
   
   when (ordinal-separator & ordinal-type)
      let matching-ordinal? =
            tokens[0] = #"hash" | tokens[0].object-class = ordinal-type;
      let matching-separator? = (tokens[1] = ordinal-separator);

      unless (matching-ordinal? & matching-separator?)
         let fail-desc = format-to-string("list item like \"%c%c\"",
               select (ordinal-type)
                  <integer> => '1';
                  <character> => 'a';
               end select,
               select (ordinal-separator)
                  #"colon" => ':';
                  #"right-paren" => ')';
                  #"period" => '.';
               end select);
         fail(make(<parse-failure>, expected: fail-desc));
      end unless;
   end when;
end function;


/** Synopsis: Ensures all characters in an ASCII line are identical. */
define function check-ascii-line-chars (line :: <string>, start-pos, fail)
   let line-char = line[0];
   for (char in line, i from 0)
      unless (char = line-char)
         fail(make(<parse-failure>, position: start-pos + i, expected:
                   format-to-string("\"%c\"", char)))
      end unless
   end for
end function;


/** Synopsis: Ensures ASCII lines in a title all use the same character. */
define function check-title-line-char (line :: <string>, fail)
   let line-char = attr(title-line-char, default: ' ');
   case
      line-char = ' ' =>
         attr(title-line-char) := line-char;
      line-char ~= line[0] =>
         fail(make(<parse-failure>, expected:
                   format-to-string("\"%c\"", line-char)));
   end case;
end function;


/**
Synopsis: Get topic title from topic- or section-title-bare-style or
-midline-style.

'Tokens' will be one of:
   - #[ #[title-line-x-style,...], title-nickname-line-x-style ]
   - #[ #f,                        title-nickname-line-x-style ]
   - #[ #[title-line-x-style,...], #f ]
*/
define function extract-title (tokens) => (title :: <sequence>)
   let flattened-tokens = choose(true?, integrate-sequences(tokens));
   apply(concatenate, #[], map(content, flattened-tokens));
end function;


/** Synopsis: Removes multiple spaces in a row, and leading/trailing spaces. */
define function remove-multiple-spaces (string :: <string>)
=> (new-string :: <string>)
   let trimmed :: <string> = regexp-replace(string, "^ +| +$", "");
   regexp-replace(trimmed, " {2,}", " ");
end function;


/** Synopsis: Replace '\n' with a space. */
define function replace-ls-with-spc (string :: <string>)
=> (string :: <string>)
   replace-elements!(string, curry(\=, '\n'), always(' '))
end function;


/**
Synopsis: Prepends a line to division content.

In the following example of an argument list section, the line "Of course..."
needs to be prepended to the paragraph following.

[example]
 [1]: Of course, the translation here 
 needs some work. Refer to these sources:
   - Pratchett, Terry. "Dwarven Dieties"
[end]

ARGUMENTS:
  words -  A sequence of markup word tokens, or #f.
  body -   A <token>, a sequence of <token>s, or #f.

VALUES:
  new-body -  A sequence of <token>s. The first element of 'new-body' will
              have 'words', either merged with the first element of 'body', or
              in a separate <paragraph-token> with elements of 'body'
              following.
*/

define generic prepend-words
   (words :: false-or(<markup-word-sequence>),
    body :: false-or(type-union(<division-content-sequence>, <token>)))
=> (new-body :: <division-content-sequence>);

/// Synopsis: Nothing to prepend.
define method prepend-words
   (words == #f, body :: false-or(<division-content-sequence>))
=> (new-body :: <division-content-sequence>)
   body | as(<division-content-sequence>, #[])
end method;

/// Synopsis: Nothing to prepend, but need to return sequence.
define method prepend-words (words == #f, body :: <token>)
=> (new-body :: <division-content-sequence>)
   as(<division-content-sequence>, vector(body))
end method;

/// Synopsis: Nothing to prepend onto, so make paragraph.
define method prepend-words (words :: <markup-word-sequence>, body == #f)
=> (new-body :: <division-content-sequence>)
   let para = make(<paragraph-token>,
                   start: words.first.parse-start, end: words.last.parse-end);
   para.content := words;
   as(<division-content-sequence>, vector(para))
end method;

/// Synopsis: Prepend to first element of sequence.
define method prepend-words
   (words :: <markup-word-sequence>, body :: <division-content-sequence>)
=> (new-body :: <division-content-sequence>)
   let old-first = element(body, 0, default: #f);
   let new-first = prepend-words(words, old-first);
   replace-subsequence!(body, new-first, start: 0, end: 1)
end method;

/// Synopsis: Prepend to paragraph.
define method prepend-words (words :: <markup-word-sequence>, body :: <paragraph-token>)
=> (new-body :: <division-content-sequence>)
   let para = make(<paragraph-token>,
                   start: words.first.parse-start, end: body.parse-end);
   para.content := concatenate(words, body.content);
   as(<division-content-sequence>, vector(para))
end method;

/// Synopsis: Prepend to anything else by creating paragraph.
define method prepend-words (words :: <markup-word-sequence>, body :: <token>)
=> (new-body :: <division-content-sequence>)
   concatenate(prepend-words(words, #f), vector(body))
end method;


//
// print-object
//


define method print-object (o :: <topic-level-style>, s :: <stream>) => ()
   format(s, "{topic-style '%c'%s%s%s}", o.line-character,
          (o.overline? & " over") | "", (o.midline? & " mid") | "",
          (o.underline? & " under") | "");
end method;

define method print-object (o :: <markup-content-token>, s :: <stream>) => ()
   format(s, "{markup %= %=}", o.default-topic-content, o.topics)
end method;

define method print-object (o :: <directive-topic-token>, s :: <stream>) => ()
   format(s, "{topic (%s) %= id %s: %=}",
      o.topic-type, o.topic-title.title-text,
      o.topic-nickname & o.topic-nickname.text,
      o.content)
end method;

define method print-object (o :: <titled-topic-token>, s :: <stream>) => ()
   format(s, "{topic %= id %s: %=}",
      o.topic-title.title-content,
      o.topic-nickname & o.topic-nickname.text,
      o.content)
end method;

define method print-object (o :: <titled-section-token>, s :: <stream>) => ()
   format(s, "{section %= id %s: %=}",
      o.section-title.title-content,
      o.section-nickname & o.section-nickname.text,
      o.content)
end method;

define method print-object (o :: <footnote-token>, s :: <stream>) => ()
   format(s, "{footnote id %s: %=}", o.index, o.content)
end method;

define method print-object (o :: <topic-or-section-title-token>, s :: <stream>) => ()
   format(s, "{title %=}", o.title-content)
end method;

define method print-object (o :: <title-nickname-token>, s :: <stream>) => ()
   format(s, "{id %s}", o.text)
end method;

define method print-object (o :: <directive-topic-title-token>, s :: <stream>) => ()
   format(s, "{title (%s) %=}", o.title-type, o.title-text)
end method;

define method print-object
   (o :: type-union(<paragraph-directive-token>, <division-directive-token>,
                    <indented-content-directive-token>),
    s :: <stream>) => ()
   format(s, "{%s direc %=}", o.directive-type, o.content)
end method;

define method print-object (o :: <link-directive-token>, s :: <stream>) => ()
   format(s, "{%s direc %=}", o.directive-type, o.link)
end method;

define method print-object (o :: <links-directive-token>, s :: <stream>) => ()
   format(s, "{%s direc %=}", o.directive-type, o.links)
end method;

define method print-object (o :: <marginal-code-block-token>, s :: <stream>) => ()
   format(s, "{code block %=}", o.content)
end method;

define method print-object (o :: <marginal-verbatim-block-token>, s :: <stream>) => ()
   format(s, "{verbatim block %=}", o.content)
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

define method print-object (o :: <bullet-list-token>, s :: <stream>) => ()
   format(s, "{bull list %=}", o.content)
end method;

define method print-object (o :: <numeric-list-token>, s :: <stream>) => ()
   format(s, "{num list from %=: %=}", o.list-start, o.content)
end method;

define method print-object (o :: <hyphenated-list-token>, s :: <stream>) => ()
   format(s, "{hyph list: %=}", o.content)
end method;

define method print-object (o :: <phrase-list-token>, s :: <stream>) => ()
   format(s, "{phrase list: %=}", o.content)
end method;

define method print-object
   (o :: type-union(<bullet-list-item-token>, <numeric-list-item-token>),
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

define method print-object
   (o :: type-union(<paragraph-token>), s :: <stream>)
=> ()
   format(s, "{para %=}", o.content)
end method;

define method print-object (o :: <raw-line-token>, s :: <stream>) => ()
   if (o.index)
      format(s, "{line #%s %=}", o.index, o.text)
   else
      format(s, "{line %=}", o.text)
   end if
end method;

/*
define method print-object (o :: <image-ref-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <line-marker-ref-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <footnote-ref-token>, s :: <stream>) => ()
end method;

define method print-object (o :: <synopsis-ref-token>, s :: <stream>) => ()
end method;
*/

define method print-object (o :: <quote-token>, s :: <stream>) => ()
   format(s, "{quote %s%s%s%s%s %=}",
      o.prequoted-text | "", o.open-quote, o.quoted-text | "", o.close-quote,
      o.postquoted-text | "", o.quote-spec | "(default)")
end method;

define method print-object (o :: <quote-spec-token>, s :: <stream>) => ()
   if (o.link)
      format(s, "%= %=", o.quote-options, o.link)
   else
      format(s, "%=", o.quote-options)
   end if;
end method;

define method print-object (o :: <text-word-token>, s :: <stream>) => ()
   format(s, "%=", o.text)
end method;

define method print-object (o :: <link-word-token>, s :: <stream>) => ()
   format(s, "{link %s}", o.text)
end method;

/*
define method print-object (o :: <api-ref-token>, s :: <stream>) => ()
end method;
*/

define method print-object (o :: <bracketed-render-span-token>, s :: <stream>)
=> ()
   format(s, "{render %s %=}", o.block-type, o.text)
end method;
