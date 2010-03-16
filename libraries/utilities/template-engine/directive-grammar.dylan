module: template-engine


//
// Comments
//


define parser templ-space
   rule many(choice(whitespace, templ-comment))
end;


define caching parser opt-templ-space
   rule opt-many(choice(whitespace, templ-comment))
end;


define parser templ-comment
   rule seq(lex-lf-comment,
            opt-many(seq(not-next(lex-rt-comment), choice(templ-comment, char))),
            lex-rt-comment)
end;


//
// Directive groups and processing
//


define class <directive-group> (<token>)
end class;


define parser-method directive (stream, context :: <template-context>)
=> (r1, r2, r3)
   label "directive";
   context.directive-catalog-parser(stream, context);
end parser-method;


define parser case-directive-block (<directive-group>)
   rule seq(many(seq(case-directive, opt(case-content))),
            opt-seq(else-directive, opt(block-content)),
            end-directive)
      => tokens;
   slot case-pairs :: <sequence> = tokens[0];
   slot else-pair :: false-or(<sequence>) = tokens[1];
   slot end-directive :: <end-directive-token> = tokens[2];
end;


define parser if-directive-block (<directive-group>)
   rule seq(if-directive, opt(branch-content),
            opt-seq(else-directive, opt(block-content)),
            end-directive)
      => tokens;
   slot if-pair :: <sequence> = vector(tokens[0], tokens[1]);
   slot else-pair :: false-or(<sequence>) = tokens[2];
   slot end-directive :: <end-directive-token> = tokens[3];
end;


define parser repeat-directive-block (<directive-group>)
   rule seq(repeat-directive, opt(block-content), end-directive) => tokens;
   slot block-pair :: <sequence> = vector(tokens[0], tokens[1]);
   slot end-directive :: <end-directive-token> = tokens[2];
end;


define parser with-directive-block (<directive-group>)
   rule seq(with-directive, opt(block-content), end-directive) => tokens;
   slot block-pair :: <sequence> = vector(tokens[0], tokens[1]);
   slot end-directive :: <end-directive-token> = tokens[2];
end;


//
// Content
//


define parser case-content :: <sequence>
   rule many(seq(not-next(case-directive), not-next(else-directive),
                 not-next(end-directive), content-element))
      => tokens;
   yield concat-adj-strings(collect-subelements(tokens, 3))
end;

define parser branch-content :: <sequence>
   rule many(seq(not-next(else-directive), not-next(end-directive), content-element))
      => tokens;
   yield concat-adj-strings(collect-subelements(tokens, 2))
end;
   
define parser block-content :: <sequence>
   rule many(seq(not-next(end-directive), content-element)) => tokens;
   yield concat-adj-strings(collect-subelements(tokens, 1))
end;
   

//
// Directive types
//


define class <delimited-directive-token> (<token>)
   slot left-delimiter :: <directive-prolog-token>;
   slot right-delimiter :: <directive-epilog-token>;
   slot directive-specifier :: false-or(<dirspec-token>);
end class;


define caching parser case-directive (<delimited-directive-token>)
   rule seq(lf-directive, dirspec-content, rt-directive) => tokens;
   inherited slot left-delimiter = tokens[0];
   inherited slot right-delimiter = tokens[2];
afterwards (context, tokens, result, start-pos, end-pos, fail: fail)
   result.directive-specifier := reparse-dirspec(context, tokens[1], tokens[0].parse-end,
         parse-case-dirspec, fail)
end;

define caching parser if-directive (<delimited-directive-token>)
   rule seq(lf-directive, dirspec-content, rt-directive) => tokens;
   inherited slot left-delimiter = tokens[0];
   inherited slot right-delimiter = tokens[2];
afterwards (context, tokens, result, start-pos, end-pos, fail: fail)
   result.directive-specifier := reparse-dirspec(context, tokens[1], tokens[0].parse-end,
         parse-if-dirspec, fail)
end;

define caching parser repeat-directive (<delimited-directive-token>)
   rule seq(lf-directive, dirspec-content, rt-directive) => tokens;
   inherited slot left-delimiter = tokens[0];
   inherited slot right-delimiter = tokens[2];
afterwards (context, tokens, result, start-pos, end-pos, fail: fail)
   result.directive-specifier := reparse-dirspec(context, tokens[1], tokens[0].parse-end,
         parse-repeat-dirspec, fail)
end;   

define caching parser with-directive (<delimited-directive-token>)
   rule seq(lf-directive, dirspec-content, rt-directive) => tokens;
   inherited slot left-delimiter = tokens[0];
   inherited slot right-delimiter = tokens[2];
afterwards (context, tokens, result, start-pos, end-pos, fail: fail)
   result.directive-specifier := reparse-dirspec(context, tokens[1], tokens[0].parse-end,
         parse-with-dirspec, fail)
end;   

define caching parser simple-directive (<delimited-directive-token>, <directive-group>)
   rule seq(lf-directive, dirspec-content, rt-directive) => tokens;
   inherited slot left-delimiter = tokens[0];
   inherited slot right-delimiter = tokens[2];
afterwards (context, tokens, result, start-pos, end-pos, fail: fail)
   result.directive-specifier := reparse-dirspec(context, tokens[1], tokens[0].parse-end,
         parse-simple-dirspec, fail)
end;

define caching parser else-directive (<delimited-directive-token>)
   rule seq(lf-directive, dirspec-content, rt-directive) => tokens;
   inherited slot left-delimiter = tokens[0];
   inherited slot right-delimiter = tokens[2];
afterwards (context, tokens, result, start-pos, end-pos, fail: fail)
   result.directive-specifier := reparse-dirspec(context, tokens[1], tokens[0].parse-end,
         parse-else-dirspec, fail)
end;

define caching parser end-directive (<delimited-directive-token>)
   label "end directive";
   rule seq(lf-directive, dirspec-content, rt-directive) => tokens;
   inherited slot left-delimiter = tokens[0];
   inherited slot right-delimiter = tokens[2];
afterwards (context, tokens, result, start-pos, end-pos, fail: fail)
   result.directive-specifier := reparse-dirspec(context, tokens[1], tokens[0].parse-end,
         parse-end-dirspec, fail)
end;

define caching parser empty-directive (<delimited-directive-token>, <directive-group>)
   rule seq(lf-directive, rt-directive) => tokens;
   inherited slot left-delimiter = tokens[0];
   inherited slot right-delimiter = tokens[1];
   inherited slot directive-specifier = #f;
end;


//
// Directive specifier reparsing
//


define caching parser dirspec-content :: <string>
   rule opt-many(seq(not-next(rt-directive), choice(paren-dirspec-text, char)))
      => tokens;
   yield subelements-string(tokens, 1)
end;


define parser paren-dirspec-text :: <string>
   rule seq(lit-lf-paren,
            opt-many(seq(not-next(lit-rt-paren), choice(paren-dirspec-text, char))),
            lit-rt-paren)
      => tokens;
   yield concat-strings(as(<string>, tokens[0]), subelements-string(tokens[1], 1),
                        as(<string>, tokens[2]))
end;


define method reparse-dirspec
   (context :: <template-context>, dirspec-text :: <string>,
    start-pos :: type-union(<integer>, <stream-position>), parser :: <function>,
    failure-exit :: <function>)
=> (dirspec :: <dirspec-token>)
   let start-pos = as(<integer>, start-pos);
   let substream = make(<string-stream>, contents: dirspec-text,
                        position-offset: start-pos);
   invalidate-parser-cache(context, from: start-pos + dirspec-text.size);
   let (dirspec, success?, failure) = parser(substream, context);
   if (success?)
      dirspec
   else
      failure-exit(failure)
   end if
end method;
