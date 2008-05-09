module: markup-translator

//
// process-tokens for <topic-content-seq> or <content-seq>
//

define method process-tokens
   (seq :: type-union(<topic-content-seq>, <content-seq>),
    token :: <marginal-code-block-token>)
=> ()
   let code-block = make(<code-block>);
   process-tokens(code-block, token.token-content);
   add!(seq, code-block);
end method;


define method process-tokens
   (seq :: type-union(<topic-content-seq>, <content-seq>),
    token :: <marginal-verbatim-block-token>)
=> ()
   let verb-block = make(<pre>);
   process-tokens(verb-block, token.token-content);
   add!(seq, verb-block);
end method;


define method process-tokens
   (seq :: type-union(<topic-content-seq>, <content-seq>),
    token :: <figure-ref-line-token>)
=> ()
   let fig = make(<fig>);
   fig.image-name := token.filename;
   fig.title := token.caption;
   select (token.scale-factor.object-class)
      <perc-scale-token> => fig.rel-size := token.scale-factor.factor;
      <mult-scale-token> => fig.abs-size := token.scale-factor.factor;
   end select;
   add!(seq, fig);
end method;


define method process-tokens
   (seq :: type-union(<topic-content-seq>, <content-seq>),
    token :: <content-ref-line-token>)
=> ()
   let ref = if (token.link)
                let targ = make(<target-placeholder>, link: token.link);
                make(<conref>, style: #"toc", target: targ);
             else
                make(<toc-placeholder>);
             end if;
   add!(seq, ref);
end method;


define method process-tokens
   (seq :: type-union(<topic-content-seq>, <content-seq>),
    token :: <ditto-ref-line-token>)
=> ()
   add!(seq, make(<ditto-placeholder>, target: token.link));
end method;


define method process-tokens
   (seq :: type-union(<topic-content-seq>, <content-seq>),
    token :: <api-list-ref-line-token>)
=> ()
   // TODO
end method;


define method process-tokens
   (seq :: type-union(<topic-content-seq>, <content-seq>),
    token :: <bracketed-raw-block-token>)
=> ()
   let raw-block =
         make(if (token.block-type = #"verbatim") <pre> else <code-block> end);
   process-tokens(raw-block, token.token-content);
   add!(seq, raw-block);
end method;


// define method process-tokens
//    (seq :: type-union(<topic-content-seq>, <content-seq>),
//     token :: <table-token>)
// => ()
//    let simp-tbl = make(<simple-table>);
//    // TODO
// end method;


define method process-tokens
   (seq :: type-union(<topic-content-seq>, <content-seq>),
    token :: type-union(<numeric-list-token>, <bullet-list-token>))
=> ()
   // <x-list-token> contains
   //   <x-list-first-item-token> or <x-list-item-token> contains
   //     <content-seq>
   let token-items-content = map(token-content, token.token-content);
   let list =
         select (token.object-class)
            <numeric-list-token> => make(<ordered-list>, start: token.list-start);
            <bullet-list-token> => make(<unordered-list>);
         end select;
   list.items = make(<vector>, size: token-items-content.size);
   for (i from 0, token-item-content in token-items-content)
      let list-item-content = make(<content-seq>);
      process-tokens(list-item-content, token-item-content);
      list.items[i] := list-item-content;
   end for;
   add!(seq, list);
end method;


define method process-tokens
   (seq :: type-union(<topic-content-seq>, <content-seq>),
    token :: type-union(<hyphenated-list-token>, <phrase-list-token>))
=> ()
   // <x-list-token> contains
   //   <x-list-item-token> contains
   //     <markup-seq> in item-label and <content-seq> in content
   let token-items = token.token-content;
   let list =
         select (token.object-class)
            <hyphenated-list-token> => make(<one-line-defn-list>);
            <phrase-list-token> => make(<two-line-defn-list>);
         end select;
   list.items = make(<array>, dimensions: vector(2, token-items.size));
   for (i from 0, token-item in token-items)
      let list-item-label = make(<markup-seq>);
      let list-item-content = make(<content-seq>);
      process-tokens(list-item-label, token-item.item-label);
      process-tokens(list-item-content, token-item.token-content);
      list.items[0, i] := list-item-label;
      list.items[1, i] := list-item-content;
   end for;
   add!(seq, list);
end method;


define method process-tokens
   (seq :: type-union(<topic-content-seq>, <content-seq>),
    token :: <paragraph-token>)
=> ()
   let para = make(<paragraph>);
   process-tokens(para.content, token.token-content);
   add!(seq, para);
end method;


//
// process-tokens for <code-block> and <pre>
//

define method process-tokens
   (blk :: <pre>, seq :: <sequence>)
=> ()
   do(curry(process-tokens, blk), seq);
end method;


define method process-tokens
   (blk :: <pre>, line-token :: <raw-line-token>)
=> ()
   when (line-token.token-index)
      add!(blk.content, make(<ph-marker>, index: line-token.token-index));
   end when;
   add!(blk.content, concatenate(line-token.token-content, "\n"));
end method;


//
// process-tokens for <markup-seq> and <title-seq>
//

define method process-tokens
   (seq :: type-union(<markup-seq>, <title-seq>),
    word :: <string>)
=> ()
   if (~seq.empty?) add!(seq, ' ') end;
   add!(seq, word);
end method;


define method process-tokens
   (seq :: type-union(<markup-seq>, <title-seq>),
    quote :: <quote-token>)
=> ()
   let elements = quote-elements(seq, quote, $default-quote-spec);
   if (~instance?(elements, <sequence>))
      elements := vector(elements);
   end if;
   if (~seq.empty?) add!(seq, ' ') end;
   if (quote.prequoted-content) add!(seq, quote.prequoted-content) end;
   seq := concatenate!(seq, elements);
   if (quote.postquoted-content) add!(seq, quote.postquoted-content) end;
end method;


define method process-tokens
   (seq :: type-union(<markup-seq>, <title-seq>),
    image-ref :: <image-ref-token>)
=> ()
   let img = make(<inline-image>, image: image-ref.filename,
                  alt-text: image-ref.caption | "");
   add!(seq, img);
end method;


define method process-tokens
   (seq :: type-union(<markup-seq>, <title-seq>),
    block-tok :: <bracketed-render-block-token>)
=> ()
   let render-content =
         make(if (block-tok.block-type = #"html") <html-content> else <dita-content> end,
              content: block-tok.token-content);
   add!(seq, render-content);
end method;


define method process-tokens
   (seq :: <markup-seq>, ref :: <marker-ref-token>)
=> ()
   let marker = make(<marker-placeholder>, index: ref.token-index);
   add!(seq, make(<xref>, target: marker));
end method;


define method process-tokens
   (seq :: <markup-seq>, ref :: <synopsis-ref-token>)
=> ()
   let target = make(<target-placeholder>, link: ref.link);
   add!(seq, make(<conref>, type: #"shortdesc", target: target));
end method;


/// Synopsis: Returns the intermediate elements that make up a quote.
///
/// Style changes affect the entire render. Link and monospace affect everything
/// inside typographical quotes. The actual link tag is innermost so its text can
/// be replaced easily by a topic title or an <api-name> or <parameter-name>
/// object (perhaps surrounded by an <xref>).
///
/// To support that, from innermost to outermost, the quoted text is wrapped in:
/// link-placeholder, term, code, typographical quotes, bib, b, i, u, term formatting, em
///
define method quote-elements
   (quote-owner :: type-union(<title-seq>, <markup-seq>),
    quote :: <quote-token>, default-specs :: <table>)
=> (outer-quote-elem :: <interm-element>)
   let specs = check-quote-specifiers(quote, default-specs,
         for-title: instance?(quote-owner, <title-seq>));
   let interior =
         if (member?(#"sic", specs))
            concatenate(quote.open-quote, quote.quoted-content, quote.close-quote)
         else
            quote.quoted-content
         end if;
   let (link-target, link-text) =
         if (instance?(specs.last, <string>))
            values(make(<target-placeholder>, link: specs.last),
                   interior)
         else
            values(make(<target-placeholder>, link: interior),
                   make(<conref>, style: #"title", target: interior))
         end if;
   let first-term = #t;

   for (spec in choose(rcurry(member?, specs),
         #[ #"qv", #"toc", #"term", #"code", #"q", #"qq", #"bib", #"b", #"i", #"u", #"term", #"em" ]))
      let new-interior = 
            select (spec)
               #"qv", #"toc" =>
                  let link = make( if (spec = #"toc") <toc-xref> else <xref> end );
                  link.text := link-text;
                  link.target := link-target;
                  link;
               #"term" =>
                  let term = make( if (first-term) <term> else <term-style> end );
                  term.text := interior;
                  first-term := #f;
                  term;
               #"code" =>
                  let phrase = make(<code-phrase>);
                  phrase.text := interior;
                  phrase;
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
                  let cite = make(<cite>);
                  cite.text := interior;
                  cite;
               #"b" =>
                  let bold = make(<bold>);
                  bold.text := interior;
                  bold;
               #"i" =>
                  let ital = make(<italic>);
                  ital.text := interior;
                  ital;
               #"u" =>
                  let ul = make(<underline>);
                  ul.text := interior;
                  ul;
               #"em" =>
                  let em = make(<emphasis>);
                  em.text := interior;
                  em;
            end select;
      interior := new-interior;
   end for;
   interior
end method;
