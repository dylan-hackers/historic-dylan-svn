module: markup-translator


//
// Processing for <markup-seq> and <title-seq>
//


define method process-tokens
   (seq :: type-union(<markup-seq>, <title-seq>),
    word :: <text-word-token>)
=> ()
   add-spacer(seq);
   add!(seq, word.token-text);
end method;


define method process-tokens
   (seq :: type-union(<markup-seq>, <title-seq>),
    image-ref :: <image-ref-token>)
=> ()
   let img = make(<inline-image>, image: image-ref.filename,
                  alt-text: image-ref.caption | "",
                  source-location: image-ref.token-src-loc);
   add-spacer(seq);
   add!(seq, img);
end method;


define method process-tokens
   (seq :: type-union(<markup-seq>, <title-seq>),
    span :: <bracketed-render-span-token>)
=> ()
   let render-content =
         make(if (span.block-type = #"html") <html-content> else <dita-content> end,
              content: span.token-text, source-location: span.token-src-loc);
   add-spacer(seq);
   add!(seq, render-content);
end method;


//
// Processing for <markup-seq>
//


define method process-tokens
   (seq :: <markup-seq>, ref :: <synopsis-ref-token>)
=> ()
   let target = make(<target-placeholder>, link: ref.link.token-text,
                     source-location: ref.link.token-src-loc);
   add-spacer(seq);
   add!(seq, make(<conref>, type: #"shortdesc", target: target,
                  source-location: ref.token-src-loc));
end method;


define method process-tokens
   (seq :: <markup-seq>, ref :: <line-marker-ref-token>)
=> ()
   let marker = make(<line-marker-placeholder>, index: ref.token-index,
                     source-location: ref.token-src-loc);
   add-spacer(seq);
   add!(seq, make(<xref>, target: marker, source-location: ref.token-src-loc));
end method;


define method process-tokens
   (seq :: <markup-seq>, ref :: <footnote-ref-token>)
=> ()
   let marker = make(<footnote-placeholder>, index: ref.token-index,
                     source-location: ref.token-src-loc);
   add-spacer(seq);
   add!(seq, make(<xref>, target: marker, source-location: ref.token-src-loc));
end method;


//
// Quotes
//


define method process-tokens
   (seq :: type-union(<markup-seq>, <title-seq>),
    quote :: <quote-token>)
=> ()
   let elements = quote-elements(quote);
   add-spacer(seq);
   if (quote.prequoted-text) add!(seq, quote.prequoted-text) end;
   seq := concatenate!(seq, elements);
   if (quote.postquoted-text) add!(seq, quote.postquoted-text) end;
end method;


/**
Synopsis: Returns the intermediate elements that make up a quote.

Style changes affect the entire render. Link and monospace affect everything
inside typographical quotes. The actual link tag is innermost so its text can
be easily replaced by a topic title.

To support that, from innermost to outermost, the quoted text is wrapped in:
link-placeholder, api, term, code, typographical quotes, bib, b, i, u, term formatting, em
**/
define method quote-elements (quote :: <quote-token>) => (elements :: <sequence>)
   let specs = quote-specs(quote);
   let interior =
         if (member?(#"sic", specs))
            concatenate(quote.open-quote, quote.quoted-text, quote.close-quote)
         else
            quote.quoted-text
         end if;

   let link-token = quote.quote-spec & quote.quote-spec.link;
   let link-target =
         if (link-token)
            // Has qv or vi with an explicit link location. Use it as target.
            make(<target-placeholder>, source-location: link-token.token-src-loc,
                 link: link-token.token-text)
         else
            // May have qv or vi without an explicit link location. Use quoted
            // text as target if needed.
            make(<target-placeholder>, source-location: quote.token-src-loc,
                 link: quote.quoted-text)
         end if;

   // The term option is used twice: once to make a term tag and again to wrap
   // it in term formatting. Use flag to distinguish the two uses.
   let first-term = #t;

   // This is a list of quote options included in the quote spec in the correct
   // order for wrapping and processing.
   let used-specs-in-order =
         choose(rcurry(member?, specs),
                #[ #"qv", #"vi", #"api", #"term", #"code", #"q", #"qq", #"bib",
                   #"b", #"i", #"u", #"term", #"em" ]);

   for (spec in used-specs-in-order)
      let new-interior = 
            select (spec)
               #"qv" =>
                  // TODO: If link-target is an URL, make a new URL for it
                  // instead of a <target-placeholder>.
                  make(<xref>, source-location: quote.token-src-loc,
                       text: interior, target: link-target,
                       target-from-text: ~link-token);
               #"vi" =>
                  make(<vi-xref>, source-location: quote.token-src-loc,
                       text: interior, target: link-target,
                       target-from-text: ~link-token);
               #"api" =>
                  make(<api/parm-name>, source-location: quote.token-src-loc,
                       text: interior);
               #"term" =>
                  let term = make(if (first-term) <term> else <term-style> end,
                                  source-location: quote.token-src-loc, text: interior);
                  first-term := #f;
                  term;
               #"code" =>
                  make(<code-phrase>, source-location: quote.token-src-loc,
                       text: interior);
               #"q" =>
                  if (~instance?(interior, <sequence>))
                     interior := vector(interior);
                  end if;
                  // left and right single curly quotes
                  let left-quote = vector(make(<entity>, code: #x2018));
                  let right-quote = vector(make(<entity>, code: #x2019));
                  concatenate-as(<markup-seq>, left-quote, interior, right-quote);
               #"qq" =>
                  if (~instance?(interior, <sequence>))
                     interior := vector(interior);
                  end if;
                  // left and right double curly quotes
                  let left-quote = vector(make(<entity>, code: #x201C));
                  let right-quote = vector(make(<entity>, code: #x201D));
                  concatenate-as(<markup-seq>, left-quote, interior, right-quote);
               #"bib" =>
                  make(<cite>, source-location: quote.token-src-loc,
                       text: interior);
               #"b" =>
                  make(<bold>, source-location: quote.token-src-loc,
                       text: interior);
               #"i" =>
                  make(<italic>, source-location: quote.token-src-loc,
                       text: interior);
               #"u" =>
                  make(<underline>, source-location: quote.token-src-loc,
                       text: interior);
               #"em" =>
                  make(<emphasis>, source-location: quote.token-src-loc,
                       text: interior);
            end select;
      interior := new-interior;
   end for;
   
   // Ensure we are returning a sequence.
   if (instance?(interior, <sequence>))
      interior
   else
      vector(interior)
   end if
end method;


define method quote-specs (quote :: <quote-token>) => (specs :: <sequence>)
   let default-specs = dynamic-binding(*default-quote-specs*,
                                       default: $default-markup-quote-specs);
   let (specs, spec-loc) =
         if (~quote.quote-spec)
            values(default-specs[quote.open-quote], $unknown-source-location)
         else
            values(quote.quote-spec.quote-options, quote.quote-spec.token-src-loc)
         end if;

   when (member?(#"q", specs) & member?(#"qq", specs))
      q-and-qq-in-spec(location: spec-loc);
   end when;

   // Titles cannot have links. This excludes qv, vi. Can have code if we don't
   // link to APIs. In DITA, they cannot have <cite>, but we can fake that.
   if (dynamic-binding(*title-markup*, default: #f))
      when (member?(#"qv", specs) | member?(#"vi", specs))
         qv-or-vi-in-title(location: spec-loc);
         specs := remove!(specs, #"qv");
         specs := remove!(specs, #"vi");
      end when;
   end if;
   
   // Cannot have a link without qv or vi.
   unless (member?(#"qv", specs) | member?(#"vi", specs))
      if (quote.quote-spec & quote.quote-spec.link)
         link-without-qv-or-vi-in-spec(location: spec-loc);
      end if
   end unless;

   // A quote can basically be code, api, term, bib, or plain text.
   // Former trumps latter.
   case
      member?(#"code", specs) =>
         specs := remove!(specs, #"api");
         specs := remove!(specs, #"term");
         specs := remove!(specs, #"bib");
      member?(#"api", specs) =>
         specs := remove!(specs, #"term");
         specs := remove!(specs, #"bib");
      member?(#"term", specs) =>
         specs := remove!(specs, #"bib");
   end case;
   specs
end method;


/** Synopsis: Insert a space character into sequence, if needed. **/
define function add-spacer (seq :: type-union(<title-seq>, <markup-seq>)) => ()
   unless (seq.empty?)
      add!(seq, ' ');
   end unless;
end function;
