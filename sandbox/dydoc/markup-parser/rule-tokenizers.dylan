module: markup-parser

define parser markup-block
   rule seq(many(choice(topic, flush-content)), opt-many-spc-or-ls, eos) => tokens;
   yield integrate-sequences(tokens[0]);
end;

define parser topic
   rule choice(topic-directive, titled-topic) => token;
   yield token;
end;

// exported
define parser topic-directive (<token-source>)
   rule seq(topic-directive-spec, opt(topic-content)) => tokens;
   slot topic-type :: <symbol> =
      tokens[0].topic-type;
   slot topic-title :: <string> =
      tokens[0].topic-title;
   slot content :: false-or(<sequence>) =
      tokens[1];
end;

// exported
define parser titled-topic (<token-source>)
   rule seq(topic-title, opt(topic-content)) => tokens;
   slot ascii-line-char :: <character> = tokens[0].ascii-line-char;
   slot ascii-overline? :: <boolean> = tokens[0].ascii-overline?;
   slot ascii-midline? :: <boolean> = tokens[0].ascii-midline?;
   slot ascii-underline? :: <boolean> = tokens[0].ascii-underline?;
   slot topic-title :: <sequence> = tokens[0].topic-title;
   slot topic-nickname  :: false-or(<string>) = tokens[0].topic-nickname;
   slot content :: false-or(<sequence>) = tokens[1];
end;

// contents promoted to <topic-directive-token>
define parser topic-directive-spec ()
   rule seq(opt(ascii-full-line), sol-ind, opt(ascii-line),
            topic-directive-spec-text, colon, opt-spaces, text-til-spc-ascii-ls,
            opt(ascii-line), spc-ls, opt(ascii-full-line))
      => tokens;
   slot topic-type :: <symbol> =
      tokens[3];
   slot topic-title :: <string> =
      tokens[6];
end;

define parser topic-title
   rule choice(topic-title-midline-style, topic-title-bare-style)
      => token;
   yield token;
cleanup (context)
   context.ascii-line-char := #f
end;

// contents promoted to <titled-topic-token>
define parser topic-title-midline-style ()
   rule seq(opt(ascii-full-line),
            choice(seq(opt-many(title-midline-line), title-midline-nickname-line),
                   seq(title-midline-line)),
            opt(ascii-full-line))
      => tokens;
   slot ascii-line-char :: <character> =
      // last retrieves title-midline-nickname-line or title-midline-line
      tokens[1].last.ascii-line-char;
   slot ascii-overline? :: <boolean> = true?(tokens[0]);
   slot ascii-midline? :: <boolean> = #t;
   slot ascii-underline? :: <boolean> = true?(tokens[2]);
   slot topic-title :: <sequence> = 
      begin
         // tokens[1] will be one of:
         //   #[ #[title-midline-line,...], title-midline-nickname-line ]
         //   #[ #f, title-midline-nickname-line ]
         //   #[ title-midline-line ]
         // We want contents of each -line (optional for -nickname-line).
         let flat-lines = integrate-sequences(choose(true?, tokens[1]));
         apply(concatenate, choose(true?, map(content, flat-lines)))
      end;
   slot topic-nickname :: false-or(<string>) =
      instance?(tokens[1].last, <title-midline-nickname-line-token>) &
      tokens[1].last.topic-nickname;
end;

// contents promoted to <titled-topic-token>
define parser topic-title-bare-style ()
   rule seq(opt(ascii-full-line),
            choice(seq(opt-many(title-bare-line), title-bare-nickname-line),
                   seq(title-bare-line)),
            ascii-full-line)
      => tokens;
   slot ascii-line-char :: <character> = tokens[2].content[0];
   slot ascii-overline? :: <boolean> = true?(tokens[0]);
   slot ascii-midline? :: <boolean> = #f;
   slot ascii-underline? :: <boolean> = #t;
   slot topic-title :: <sequence> = 
      begin
         // tokens[1] will be one of:
         //   #[ #[title-bare-line,...], title-bare-nickname-line ]
         //   #[ #f, title-bare-nickname-line ]
         //   #[ title-bare-line ]
         // We want contents of each -line (optional for -nickname-line).
         let flat-lines = integrate-sequences(choose(true?, tokens[1]));
         apply(concatenate, choose(true?, map(content, flat-lines)))
      end;
   slot topic-nickname :: false-or(<string>) =
      instance?(tokens[1].last, <title-bare-nickname-line-token>) &
      tokens[1].last.topic-nickname;
end;

// contents promoted to <topic-title-midline-style-token>
define parser title-midline-line ()
   rule seq(not-next(title-midline-nickname-line),
            sol-ind, ascii-line, spaces,
            many(seq(not-next(ascii-line), not-next(spc-ls), title-word, opt-spaces)),
            opt(ascii-line), spc-ls)
      => tokens;
   slot ascii-line-char :: <character> = tokens[2].content[0];
   slot content :: <sequence> = collect-subelements(tokens[4], 2);
end;

// contents promoted to <topic-title-bare-style-token>
define parser title-bare-line ()
   rule seq(not-next(title-bare-nickname-line),
            sol-ind, many(seq(not-next(spc-ls), title-word, opt-spaces)), spc-ls)
      => tokens;
   slot content :: <sequence> = collect-subelements(tokens[2], 1);
end;

// contents promoted to <topic-title-midline-style-token>
define parser title-midline-nickname-line ()
   rule seq(sol-ind, ascii-line, spaces,
            opt-many(seq(not-next(ascii-line), not-next(open-bracket), title-word, opt-spaces)),
            opt-seq(ascii-line, spaces),
            opn-brack-spc, nickname-word, spc-cls-brack, spc-ls)
      => tokens;
   slot ascii-line-char :: <character> =
      tokens[1].content[0];
   slot content :: false-or(<sequence>) =
      tokens[3] & collect-subelements(tokens[3], 2);
   slot topic-nickname :: <string> =
      tokens[6];
end;

// contents promoted to <topic-title-bare-style-token>
define parser title-bare-nickname-line ()
   rule seq(sol-ind,
            opt-many(seq(not-next(open-bracket), title-word, opt-spaces)),
            opn-brack-spc, nickname-word, spc-cls-brack, spc-ls)
      => tokens;
   slot content :: false-or(<sequence>) =
      tokens[1] & collect-subelements(tokens[1], 1);
   slot topic-nickname :: <string> =
      tokens[3];
end;

define parser topic-content
   rule many(choice(section-directive, flush-content, footnote))
      => items;
   yield integrate-sequences(choose(true?, items));
end;

// null-directive yields flush-content
define parser section-directive
   rule choice(paragraph-directive, link-directive, links-directive,
               indented-directive, null-directive)
      => token;
   yield token;
end;

// exported
define parser footnote (<token-source>)
   rule seq(sol-ind, opn-brack-spc, choice(number, ordinal),
            choice(seq(spc-cls-brack, colon), seq(colon, spc-cls-brack)),
            spaces, opt(markup-words), ls, opt(flush-content))
      => tokens;
   slot index :: type-union(<integer>, <character>) =
      tokens[2];
   slot content :: false-or(<sequence>) =
      prepend-words(tokens[5], tokens[7]);
end;

define parser flush-content
   rule many(flush-content-at-level) => tokens;
   yield (apply(concatenate, tokens));
end;

define parser lines-content
   rule many(line-content)
      => tokens;
   yield choose(true?, tokens);
end;

// #"blank-lines" counts as #f
define parser line-content :: false-or(<token>)
   rule seq(not-next(eos), not-next(topic-directive-spec), not-next(topic-title),
            not-next(section-directive), not-next(footnote),
            choice(blank-lines, marginal-code-block, marginal-verbatim-block,
                   figure-ref-line, content-ref-line, ditto-ref-line,
                   api-list-ref-line, bracketed-raw-block, table,
                   bullet-list, numeric-list, hyphenated-list, phrase-list,
                   paragraph))
      => tokens;
   yield (tokens[5] ~= #"blank-lines") & tokens[5];
end;

// exported -- contents are those of paragraph-line tokens combined
define parser paragraph (<token-source>)
   rule many(paragraph-line)
      => items;
   slot content :: <sequence> =
      apply(concatenate, items);
end;

// exported as paragraph -- contents are those of paragraph-line tokens combined
define parser paragraph-til-null-directive (<paragraph-token>)
   rule many(seq(not-next(null-directive-spec), paragraph-line))
      => items;
   inherited slot content /* :: <sequence> */ =
      apply(concatenate, collect-subelements(items, 1));
end;

// only used for look-ahead
define parser bracketed-line
   rule seq(req-next(open-bracket),
            choice(figure-ref-line, content-ref-line, ditto-ref-line,
                   api-list-ref-line, bracketed-raw-block-start-line))
end;

// exported
define parser marginal-code-block (<token-source>)
   rule many(marginal-code-block-line) => lines;
   slot content :: <sequence> = lines;
end;

// exported
define parser marginal-verbatim-block (<token-source>)
   rule many(marginal-verbatim-block-line) => lines;
   slot content :: <sequence> = lines;
end;

// exported -- content is sequence of items
define parser bullet-list (<token-source>)
   rule seq(bullet-list-first-item, opt-many(seq(opt(blank-lines), bullet-list-item)))
      => items;
   slot content :: <sequence> = combined-list-items(items);
afterwards (context, tokens)
   pop(context.list-stack);
end;

// exported -- content is sequence of items
define parser numeric-list (<token-source>)
   rule seq(numeric-list-first-item, opt-many(seq(opt(blank-lines), numeric-list-item)))
      => items;
   slot list-start :: type-union(<integer>, <character>) =
      items[0].ordinal;
   slot content :: <sequence> =
      combined-list-items(items);
afterwards (context, tokens)
   pop(context.list-stack);
end;

// exported -- content is sequence of items
define parser phrase-list (<token-source>)
   rule seq(phrase-list-item, opt-many(seq(opt(blank-lines), phrase-list-item)))
      => items;
   slot content :: <sequence> = combined-list-items(items);
end;

// exported -- content is sequence of items
define parser hyphenated-list (<token-source>)
   rule seq(hyphenated-list-item, opt-many(seq(opt(blank-lines), hyphenated-list-item)))
      => items;
   slot content :: <sequence> = combined-list-items(items);
end;

define parser table
   rule seq(table-header, opt-many(table-row), table-footer)
end;

// exported
define parser figure-ref-line (<token-source>)
   rule seq(sol-ind, opn-brack-spc, fig-lit, many-spc-or-ls, filename,
            opt-seq(many-spc-or-ls, choice(perc-scale, mult-scale)),
            spc-cls-brack, opt(text-til-spc-ls), spc-ls)
      => tokens;
   slot filename :: <string> =
      tokens[4];
   slot scale-factor :: false-or(type-union(<perc-scale-token>, <mult-scale-token>)) =
      tokens[5] & tokens[5][1];
   slot caption :: false-or(<string>) =
      tokens[7];
end;

// exported -- link can be false if referring to sub-topics of current topic
define parser content-ref-line (<token-source>)
   rule seq(sol-ind, opn-brack-spc, contents-lit,
            opt-seq(many-spc-or-ls, of-lit, many-spc-or-ls,
                    link-til-end-brack),
            spc-cls-brack, spc-ls)
      => tokens;
   slot link :: false-or(<string>) = tokens[3] & tokens[3][3];
end;

// exported
define parser ditto-ref-line (<token-source>)
   rule seq(sol-ind, opn-brack-spc, ditto-lit, many-spc-or-ls,
            link-til-end-brack, spc-cls-brack, spc-ls)
      => tokens;
   slot link :: <string> = tokens[4];
end;

// exported
define parser api-list-ref-line (<token-source>)
   rule seq(sol-ind, opn-brack-spc, list-lit, many-spc-or-ls, of-lit,
            many-spc-or-ls, api-list-spec-text, spc-cls-brack, spc-ls)
      => tokens;
   slot list-type :: <symbol> = tokens[6];
end;

// exported
define parser bracketed-raw-block (<token-source>)
   rule seq(bracketed-raw-block-start-line,
            opt-many(seq(sol-ind, not-next(bracketed-raw-block-end-line),
                         raw-line)),
            bracketed-raw-block-end-line)
      => tokens;
   slot block-type :: <symbol> =
      tokens[0];
   slot content :: false-or(<sequence>) =
      tokens[1] & collect-subelements(tokens[1], 2);
cleanup (context)
   context.end-bracket-spec := #f
end;

define parser blank-lines
   rule many(seq(sol, spc-ls))
end;

// exported
define parser bullet-list-first-item (<token-source>)
   rule seq(bullet-list-line, opt(indented-content)) => tokens;
   slot content :: <sequence> =
      prepend-words(tokens[0].content, tokens[1]);
afterwards (context, tokens)
   push(context.list-stack, tokens[0].bullet);
end;

// exported
define parser bullet-list-item (<token-source>)
   rule seq(bullet-list-line, opt(indented-content)) => tokens;
   slot content :: <sequence> =
      prepend-words(tokens[0].content, tokens[1]);
afterwards (context, tokens)
   unless (tokens[0].bullet = context.list-stack.first)
      error(make(<parse-failure>, expected:
                 format-to-string("bullet \"%s\"", context.list-stack.first)))
   end unless;
end;

// exported
define parser numeric-list-first-item (<token-source>)
   rule seq(numeric-list-first-line, opt(indented-content)) => tokens;
   slot ordinal :: type-union(<integer>, <character>) =
      tokens[0].ordinal;
   slot content :: <sequence> =
      prepend-words(tokens[0].content, tokens[1]);
afterwards (context, tokens)
   let ord-type =
         if (instance?(tokens[0].ordinal, <integer>)) #"number" else #"alpha" end;
   let list-pair = pair(ord-type, tokens[0].separator);
   push(context.list-stack, list-pair);
end;

// exported
define parser numeric-list-item (<token-source>)
   rule seq(numeric-list-line, opt(indented-content)) => tokens;
   slot content :: <sequence> =
      prepend-words(tokens[0].content, tokens[1]);
afterwards (context, tokens)
   let list-pair = pair(tokens[0].ordinal-type, tokens[0].separator);
   let stack-pair = context.list-stack.first;
   let type-matches? = 
         list-pair.head = #"hash" | (list-pair.head = stack-pair.head);
   let sep-matches? = (list-pair.tail = stack-pair.tail);
   unless (type-matches? & sep-matches?)
      error(make(<parse-failure>, expected: format-to-string("%s or hash followed by %s",
            as(<string>, stack-pair.head), as(<string>, stack-pair.tail))))
   end unless;
end;

// exported
define parser hyphenated-list-item (<token-source>)
   rule seq(hyphenated-list-line, opt(indented-content)) => tokens;
   slot item-label :: <sequence> =
      tokens[0].item-label;
   slot content :: <sequence> =
      prepend-words(tokens[0].content, tokens[1]);
end;

// exported
define parser phrase-list-item (<token-source>)
   rule seq(phrase-list-label, indented-content) => tokens;
   slot item-label :: <sequence> =
      tokens[0];
   slot content :: <sequence> =
      tokens[1];
end;

define parser paragraph-line
   rule seq(not-next(blank-lines), not-next(bracketed-line),
            not-next(marginal-code-block-line),
            not-next(marginal-verbatim-block-line), not-next(bullet-list-line),
            not-next(numeric-list-first-line), not-next(hyphenated-list-line),
            sol-ind, markup-words, spc-ls)
      => tokens;
   yield tokens[8];
end;

define parser paragraph-line-til-hyphen-spc-ls
   rule seq(not-next(blank-lines), not-next(bracketed-line),
            not-next(marginal-code-block-line),
            not-next(marginal-verbatim-block-line), not-next(bullet-list-line),
            not-next(numeric-list-first-line), not-next(hyphenated-list-line),
            sol-ind,
            many(seq(not-next(seq(hyphen, spc-ls)), markup-word, spaces)),
            choice(req-next(hyphen), ls))
      => tokens;
   yield collect-subelements(tokens[8], 1);
end;

define parser bracketed-raw-block-start-line
   rule seq(sol-ind, opn-brack-spc, bracketed-raw-block-spec-text,
            spc-cls-brack, spc-ls)
      => tokens;
   yield tokens[2];
afterwards (context, tokens)
   context.end-bracket-spec := tokens[2]
end;

define parser bracketed-raw-block-end-line
   rule seq(sol-ind, opn-brack-spc, end-lit,
            opt-seq(many(spc-or-ls), bracketed-raw-block-spec-text),
            spc-cls-brack, spc-ls);
afterwards (context, tokens)
   when (tokens[3] & tokens[3][1] ~= context.end-bracket-spec)
      error(make(<parse-failure>, expected:
                 format-to-string("\"%s\"", context.end-bracket-spec)))
   end when
end;

define parser marginal-code-block-line
   rule seq(sol-ind, colon, spc, raw-line) => tokens;
   yield tokens[3];
end;

define parser marginal-verbatim-block-line
   rule seq(sol-ind, choice(gt, bar), spc, raw-line) => tokens;
   yield tokens[3];
end;

// contents promoted to <bullet-list-item> or <bullet-list-first-item>
define parser bullet-list-line ()
   rule seq(sol-ind, bullet-char, spaces, markup-words, ls)
      => tokens;
   slot bullet :: <string> = tokens[1];
   slot content :: <sequence> = tokens[3];
end;

// contents promoted to <numeric-list-first-item>
define parser numeric-list-first-line ()
   rule seq(sol-ind, choice(number, ordinal), choice(colon, right-paren, period),
            spaces, markup-words, ls)
      => tokens;
   slot ordinal :: type-union(<integer>, <character>) =
      tokens[1];
   slot separator :: <symbol> =
      tokens[2];
   slot content :: <sequence> =
      tokens[4];
end;

// contents promoted to <numeric-list-item>
define parser numeric-list-line ()
   rule seq(sol-ind, choice(number, ordinal, hash), choice(colon, right-paren, period),
            spaces, markup-words, ls)
      => tokens;
   slot ordinal-type :: <symbol> =
      select (tokens[1] by instance?)
         <integer>   => #"number";
         <character> => #"alpha";
         <symbol>    => #"hash";
      end select;
   slot separator :: <symbol> =
      tokens[2];
   slot content :: <sequence> =
      tokens[4];
end;

// contents promoted to <hyphenated-list-item>
define parser hyphenated-list-line ()
   rule seq(sol-ind, markup-word, spaces, hyphen, spaces, markup-words, ls)
      => tokens;
   slot item-label :: <sequence> =
      tokens[1];
   slot content :: <sequence> =
      tokens[5];
end;

define parser phrase-list-label
   rule seq(not-next(topic-directive-spec), not-next(topic-title),
            not-next(section-directive), not-next(footnote),
            many(paragraph-line-til-hyphen-spc-ls), hyphen, spc-ls)
      => tokens;
   yield tokens[4];
end;

// exported
define parser raw-line (<token-source>)
   rule seq(opt-many(seq(not-next(raw-line-end), char)), raw-line-end)
      => tokens;
   slot content :: <string> =
      begin
         let first-part =
               if (tokens[0])
                  map-as(<string>, method (seq-items) seq-items.last end, tokens[0]);
               else
                  ""
               end if;
         concatenate(first-part, tokens[1].content)
      end;
   slot index :: false-or(type-union(<integer>, <character>)) =
      tokens[1].index;
end;

// content promoted to <raw-line-token> -- includes literal string representation
define parser raw-line-end ()
   rule seq(opt(line-marker), spc-ls) => tokens;
   slot content :: <string> =
      (tokens[0] & tokens[0].content) | "";
   slot index :: false-or(type-union(<integer>, <character>)) =
      tokens[0] & tokens[0].index
end;

// content promoted to <raw-line-end-token> -- includes literal string representation
define parser line-marker ()
   rule seq(open-bracket, colon, choice(number, ordinal), close-bracket)
      => tokens;
   slot content :: <string> =
      format-to-string("[:%s]", tokens[2]);
   slot index :: type-union(<integer>, <character>) =
      tokens[2];
end;

// exported
define parser perc-scale (<token-source>)
   rule seq(number, percent) => tokens;
   slot factor :: <integer> = tokens[0];
end;

// exported
define parser mult-scale (<token-source>)
   rule seq(number, x-lit) => tokens;
   slot factor :: <integer> = tokens[0];
end;

// exported
define parser paragraph-directive (<token-source>)
   rule seq(paragraph-directive-spec, opt(markup-words), spc-ls,
            opt(paragraph-til-null-directive))
      => tokens;
   slot directive-type :: <symbol> =
      tokens[0];
   slot content :: false-or(<paragraph-token>) =
      begin
         let body = prepend-words(tokens[1], tokens[3]);
         body & body.first
      end;
end;

// exported
define parser link-directive (<token-source>)
   rule seq(link-directive-spec, link-til-spc-ls, spc-ls) => tokens;
   slot directive-type :: <symbol> = tokens[0];
   slot link :: <string> = tokens[1];
end;

// exported
define parser links-directive (<token-source>)
   rule seq(links-directive-spec, opt-many(link-words), spc-ls,
            opt(indented-link-words))
      => tokens;
   slot directive-type :: <symbol> =
      tokens[0];
   slot links :: <sequence> =
      concatenate(tokens[1], tokens[3]);
end;

// exported
define parser indented-directive (<token-source>)
   rule seq(indented-directive-spec, opt(markup-words), spc-ls,
            opt(indented-content))
      => tokens;
   slot directive-type :: <symbol> =
      tokens[0];
   slot content :: false-or(<sequence>) =
      prepend-words(tokens[1], tokens[3]);
end;

define parser null-directive
   rule seq(null-directive-spec, opt(markup-words), spc-ls,
            opt(flush-content))
      => tokens;
   yield prepend-words(tokens[1], tokens[3]);
end;

define parser paragraph-directive-spec
   rule seq(opt(ascii-full-line), sol-ind, opt-seq(ascii-line, spaces),
            paragraph-directive-spec-text, colon, opt-spaces, 
            opt-seq(ascii-line, spaces), opt-seq(ls, ascii-full-line))
      => tokens;
   yield tokens[3];
cleanup (context)
   context.ascii-line-char := #f
end;

define parser link-directive-spec
   rule seq(opt(ascii-full-line), sol-ind, opt-seq(ascii-line, spaces),
            link-directive-spec-text, colon, opt-spaces,
            opt-seq(ascii-line, spaces), opt-seq(ls, ascii-full-line))
      => tokens;
   yield tokens[3];
cleanup (context)
   context.ascii-line-char := #f
end;

define parser links-directive-spec
   rule seq(opt(ascii-full-line), sol-ind, opt-seq(ascii-line, spaces),
            links-directive-spec-text, colon, opt-spaces,
            opt-seq(ascii-line, spaces), opt-seq(ls, ascii-full-line))
      => tokens;
   yield tokens[3];
cleanup (context)
   context.ascii-line-char := #f
end;

define parser indented-directive-spec
   rule seq(opt(ascii-full-line), sol-ind, opt-seq(ascii-line, spaces),
            indented-directive-spec-text, colon, opt-spaces,
            opt-seq(ascii-line, spaces), opt-seq(ls, ascii-full-line))
      => tokens;
   yield tokens[3];
cleanup (context)
   context.ascii-line-char := #f
end;

define parser null-directive-spec
   rule seq(opt(ascii-full-line), sol-ind, opt-seq(ascii-line, spaces),
            null-directive-spec-text, colon, opt-spaces,
            opt-seq(ascii-line, spaces), opt-seq(ls, ascii-full-line));
cleanup (context)
   context.ascii-line-char := #f
end;

define parser title-words
   rule many(seq(opt-spaces, opt-seq(ls, sol, opt-spaces, not-next(ls)),
                 not-next(open-bracket), not-next(ascii-line), title-word))
      => items;
   yield collect-subelements(items, 4);
end;

define parser markup-words
   rule many(seq(markup-word, opt-spaces)) => items;
   yield collect-subelements(items, 0);
end;

define parser title-word
   rule choice(image-ref, quote, bracketed-render-block, text-til-spc-or-ls)
      => token;
   yield token;
end;

define parser markup-word
   rule choice(title-word, api-ref, marker-ref, synopsis-ref)
      => token;
   yield token;
end;

define parser ascii-full-line
   rule seq(sol-ind, ascii-line, spc-ls) => tokens;
   yield tokens[1];
end;

// exported
define parser image-ref (<token-source>)
   rule seq(opn-brack-spc, img-lit, many-spc-or-ls, filename, 
            opt-seq(many-spc-or-ls, text-til-spc-cls-brack), spc-cls-brack)
      => tokens;
   slot filename :: <string> = tokens[3];
   slot caption :: false-or(<string>) = tokens[4] & tokens[4][1];
end;

// exported
define parser marker-ref (<token-source>)
   rule seq(opn-brack-spc, choice(number, ordinal), spc-cls-brack) => tokens;
   slot index :: type-union(<integer>, <character>) = tokens[1];
end;

// exported
define parser bracketed-render-block (<token-source>)
   rule seq(bracketed-render-block-start,
            opt-many(seq(not-next(bracketed-render-block-end), char)),
            bracketed-render-block-end)
      => tokens;
   slot block-type :: <symbol> =
      tokens[0];
   slot content :: <string> =
      tokens[1] & map-as(<string>, method (seq-items) seq-items.last end, tokens[1]);
cleanup (context)
   context.end-bracket-spec := #f
end;

define parser bracketed-render-block-start
   rule seq(opn-brack-spc, bracketed-render-block-spec-text, spc-cls-brack)
      => tokens;
   yield tokens[1];
afterwards (context, tokens)
   context.end-bracket-spec := tokens[1]
end;

define parser bracketed-render-block-end
   rule seq(opn-brack-spc, end-lit,
            opt-seq(many(spc-or-ls), bracketed-render-block-spec-text),
            spc-cls-brack);
afterwards (context, tokens)
   when (tokens[2] & tokens[2][1] ~= context.end-bracket-spec)
      error(make(<parse-failure>, expected:
                 format-to-string("\"%s\"", context.end-bracket-spec)))
   end when
end;

// exported
define parser synopsis-ref (<token-source>)
   rule seq(opn-brack-spc, synopsis-lit, many-spc-or-ls, of-lit,
            many-spc-or-ls, link-til-end-brack, spc-cls-brack)
      => tokens;
   slot link :: <string> = tokens[5];
end;

define parser topic-directive-spec-text
   rule choice(function-lit, variable-lit, seq(generic-lit, spaces, function-lit),
               library-lit, module-lit, class-lit, macro-lit)
      => token;
   yield (instance?(token, <symbol>) & token) | #"generic-function";
end;

define parser paragraph-directive-spec-text
   rule choice(synopsis-lit, syn-lit) => token;
   yield
      select (token)
         #"synopsis", #"syn" => #"synopsis";
      end select;
end;

define parser link-directive-spec-text
   rule section-lit => token;
   yield token;
end;

define parser links-directive-spec-text
   rule choice(seq(relevant-lit, spaces, to-lit),
               seq(see-lit, spaces, also-lit))
      => token;
   yield
      select (token[0])
         #"relevant" => #"relevant-to";
         #"see" => #"see-also";
      end select;
end;

define parser indented-directive-spec-text
   rule choice(init-keywords-lit, conditions-lit, exceptions-lit, arguments-lit,
               keywords-lit, signals-lit, warning-lit, errors-lit, values-lit, 
               args-lit, seq(make-lit, spaces, keywords-lit), note-lit)
      => token;
   yield 
      select (token)
         #"arguments", #"args" => #"arguments";
         #"values" => #"values";
         #"note" => #"note";
         #"warning" => #"warning";
         #"conditions", #"signals", #"errors", #"exceptions" => #"conditions";
         otherwise => #"keywords";
      end select;
end;

define parser null-directive-spec-text
   rule discussion-lit;
end;

define parser api-list-spec-text
   rule choice(functions-lit, libraries-lit, variables-lit, bindings-lit,
               classes-lit, modules-lit, macros-lit) => token;
   yield token;
end;

define parser bracketed-raw-block-spec-text
   rule choice(verbatim-lit, diagram-lit, example-lit, code-lit) => token;
   yield token;
end;

define parser bracketed-render-block-spec-text
   rule choice(dita-lit, html-lit) => token;
   yield token;
end;

// yields a list of link-word elements
define parser link-word-lines
   rule many(seq(sol-ind, link-words, spc-ls))
      => tokens;
   yield collect-subelements(tokens, 2);
end;

// yields a list of link-word elements
define parser link-words
   rule seq(link-word, opt-many(seq(spaces, link-word)))
      => tokens;
   yield
      begin
         let first-items = vector(tokens[0]);
         let many-items = tokens[1];
         if (many-items)
            apply(concatenate, first-items,
                  map(method (seq-items) seq-items.last end, many-items))
         else
            first-items
         end if;
      end;
end;

define parser link-word
   rule choice(seq(start-quote, text-til-end-quote, end-quote), text-til-spc-or-ls)
      => token;
   yield (instance?(token, <string>) & token) | token[1];
end;

define parser link-til-spc-ls
   rule text-til-spc-ls => token;
   yield token;
end;

define parser link-til-end-brack
   rule text-til-spc-cls-brack => token;
   yield token;
end;

define parser filename
   rule seq(start-quote, text-til-end-quote, end-quote) => tokens;
   yield tokens[1];
end;

define parser nickname-word
   rule many(seq(not-next(spc), not-next(close-bracket), char)) => items;
   yield map-as(<string>, method (seq-items) seq-items.last end, items);
end;

// exported
define parser quote (<token-source>)
   rule seq(quoted-words, opt-seq(many-spc-or-ls, quote-spec))
      => tokens;
   slot prequoted-content :: false-or(<string>) =
      tokens[0].prequoted-content;
   slot open-quote :: <string> = 
      tokens[0].open-quote;
   slot quoted-content :: false-or(<string>) =
      tokens[0].quoted-content;
   slot close-quote :: <string> =
      tokens[0].close-quote;
   slot postquoted-content :: false-or(<string>) =
      tokens[0].postquoted-content;
   slot quote-spec :: false-or(<sequence>) =
      tokens[1] & tokens[1][1];
end;

// content promoted to <quote-token>
define parser quoted-words ()
   rule seq(opt-many(choice(left-paren, open-bracket, left-brace, lt)),
            start-quote, opt(text-til-end-quote), end-quote,
            opt(text-til-spc-or-ls))
      => tokens;
   slot prequoted-content :: false-or(<string>) =
      tokens[0] & apply(concatenate,
                        map(method (sym) as(<string>, sym) end, tokens[0]));
   slot open-quote :: <string> =
      tokens[1];
   slot quoted-content :: false-or(<string>) =
      tokens[2];
   slot close-quote :: <string> =
      tokens[3];
   slot postquoted-content :: false-or(<string>) =
      tokens[4];
end;

define parser quote-spec
   rule seq(opn-brack-spc, opt-many(seq(quote-spec-option, opt-many-spc-or-ls)),
            opt(quote-spec-link-option), spc-cls-brack)
      => tokens;
   // tokens[1] will be false-or( #( #(option, #f), ...))
   yield
      begin
         let simple-options = collect-subelements(tokens[1], 0);
         let link-options = (tokens[2] & vector(tokens[2].option, tokens[2].link)) | #();
         concatenate(simple-options, link-options)
      end;
end;

define parser quote-spec-option
   rule choice(code-lit, term-lit, bib-lit, sic-lit, toc-lit, unq-lit,
               em-lit, qq-lit, qv-lit, b-lit, i-lit, u-lit, q-lit)
      => token;
   yield token;
end;

// content promoted to quote spec list
define parser quote-spec-link-option ()
   rule seq(choice(toc-lit, qv-lit), many-spc-or-ls, text-til-spc-cls-brack)
      => tokens;
   slot option = tokens[0];
   slot link = tokens[2];
end;

define parser text-til-spc-ls
   rule many(seq(not-next(spc-ls), char)) => items;
   yield map-as(<string>, method (seq-items) seq-items.last end, items);
end;

define parser text-til-spc-ascii-ls
   rule many(text-til-spc-ascii-ls-3) => items;
   // yield map-as(<string>, method (seq-items) seq-items.last end, items);
   yield concatenate-as(<string>, items);
end;

define parser text-til-spc-ascii-ls-3
   rule seq(text-til-spc-ascii-ls-2, char) => items;
   yield items[1];
end;

define parser text-til-spc-ascii-ls-2
   rule not-next(text-til-spc-ascii-ls-1);
end;

define parser text-til-spc-ascii-ls-1
   rule seq(opt-spaces, choice(ls, ascii-line, spc-ls));
end;

define parser text-til-spc-cls-brack
   rule many(seq(not-next(spc-cls-brack), char)) => items;
   yield map-as(<string>, method (seq-items) seq-items.last end, items);
end;

define parser text-til-end
   rule many(seq(not-next(seq(opn-brack-spc, end-lit)), char)) => items;
   yield map-as(<string>, method (seq-items) seq-items.last end, items);
end;

define parser opn-brack-spc
   rule seq(open-bracket, opt-many-spc-or-ls)
end;

define parser spc-cls-brack
   rule seq(opt-many-spc-or-ls, close-bracket)
end;

define parser spc-ls
   rule seq(opt-spaces, ls)
end;
