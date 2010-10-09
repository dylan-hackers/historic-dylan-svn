module: template-engine


//
// Overall template and template specifier
//


define parser template (<token>)
   rule seq(opt(template-header), opt-many(content-element), eof) => tokens;
   slot contents :: <sequence>
      = concat-adj-strings(integrate-sequences(copy-sequence(tokens, start: 1)));
end;


define parser content-element :: type-union(<sequence>, <directive-group>, <string>)
   rule choice(escaped-directive, directive, text, space) => token;
   yield token
end;


define parser content-element-til-escape-end
      :: type-union(<sequence>, <directive-group>, <string>)
   rule choice(escaped-directive, directive, text-til-escape-end, space) => token;
   yield token
end;


define parser-method template-header (stream, context :: <template-context>)
=> (result, success?, failure)
   label "template header";
   if (context.allow-header?)
      parse-template-style-directive(stream, context)
   else
      values(#f, #f, #f)
   end if
end parser-method;


define parser template-style-directive
   rule choice(paren-directive, angle-directive, brace-directive, brack-directive)
end;


define parser paren-directive
   rule seq(lf-paren-directive, lit-template, rt-paren-directive);
afterwards (context :: <template-context>, tokens, result, start-pos, end-pos)
   context.lf-directive-character := '(';
   context.rt-directive-character := ')';
end;

define parser angle-directive
   rule seq(lf-angle-directive, lit-template, rt-angle-directive);
afterwards (context :: <template-context>, tokens, result, start-pos, end-pos)
   context.lf-directive-character := '<';
   context.rt-directive-character := '>';
end;

define parser brace-directive
   rule seq(lf-brace-directive, lit-template, rt-brace-directive);
afterwards (context :: <template-context>, tokens, result, start-pos, end-pos)
   context.lf-directive-character := '{';
   context.rt-directive-character := '}';
end;

define parser brack-directive
   rule seq(lf-brack-directive, lit-template, rt-brack-directive);
afterwards (context :: <template-context>, tokens, result, start-pos, end-pos)
   context.lf-directive-character := '[';
   context.rt-directive-character := ']';
end;


//
// Directive prolog and epilog
//


define class <directive-prolog-token> (<token>)
end class;

define class <directive-epilog-token> (<token>)
   slot raw-output? :: <boolean>
end class;


define caching parser lf-paren-directive (<directive-prolog-token>)
   rule seq(choice(seq(opt-space, lit-lf-parens, lit-plus), lit-lf-parens),
            opt-templ-space) => tokens;
end;

define caching parser lf-angle-directive (<directive-prolog-token>)
   rule seq(choice(seq(opt-space, lit-lf-angles, lit-plus), lit-lf-angles),
            opt-templ-space) => tokens;
end;

define caching parser lf-brace-directive (<directive-prolog-token>)
   rule seq(choice(seq(opt-space, lit-lf-braces, lit-plus), lit-lf-braces),
            opt-templ-space) => tokens;
end;

define caching parser lf-brack-directive (<directive-prolog-token>)
   rule seq(choice(seq(opt-space, lit-lf-bracks, lit-plus), lit-lf-bracks),
            opt-templ-space) => tokens;
end;


define caching parser rt-paren-directive (<directive-epilog-token>)
   rule seq(opt-templ-space, opt(lit-exclamation),
            choice(lit-rt-parens, seq(lit-plus, lit-rt-parens, opt-space)))
      => tokens;
   inherited slot raw-output? = tokens[1].true?;
end;

define caching parser rt-angle-directive (<directive-epilog-token>)
   rule seq(opt-templ-space, opt(lit-exclamation),
            choice(lit-rt-angles, seq(lit-plus, lit-rt-angles, opt-space)))
      => tokens;
   inherited slot raw-output? = tokens[1].true?;
end;

define caching parser rt-brace-directive (<directive-epilog-token>)
   rule seq(opt-templ-space, opt(lit-exclamation),             
            choice(lit-rt-braces, seq(lit-plus, lit-rt-braces, opt-space)))
      => tokens;
   inherited slot raw-output? = tokens[1].true?;
end;

define caching parser rt-brack-directive (<directive-epilog-token>)
   rule seq(opt-templ-space, opt(lit-exclamation),             
            choice(lit-rt-bracks, seq(lit-plus, lit-rt-bracks, opt-space)))
      => tokens;
   inherited slot raw-output? = tokens[1].true?;
end;


//
// Prolog and epilog character wrappers via context
//


define parser-method lf-directive (stream, context :: <template-context>)
=> (r1, r2, r3)
   label "start of directive";
   context.lf-directive-parser(stream, context)
end parser-method;

define parser-method rt-directive (stream, context :: <template-context>)
=> (r1, r2, r3)
   label "end of directive";
   context.rt-directive-parser(stream, context)
end parser-method;


define parser-method lf-directive-char (stream, context :: <template-context>)
=> (r1, r2, r3)
   label as(<string>, context.lf-directive-character);
   context.lf-directive-char-parser(stream, context)
end parser-method;

define parser-method rt-directive-char (stream, context :: <template-context>)
=> (r1, r2, r3)
   label as(<string>, context.rt-directive-character);
   context.rt-directive-char-parser(stream, context)
end parser-method;


define parser-method lf-directive-mark (stream, context :: <template-context>)
=> (r1, r2, r3)
   label format-to-string("%c%c",
         context.lf-directive-character, context.lf-directive-character);
   context.lf-directive-mark-parser(stream, context)
end parser-method;

define parser-method rt-directive-mark (stream, context :: <template-context>)
=> (r1, r2, r3)
   label format-to-string("%c%c",
         context.rt-directive-character, context.rt-directive-character);
   context.rt-directive-mark-parser(stream, context)
end parser-method;
