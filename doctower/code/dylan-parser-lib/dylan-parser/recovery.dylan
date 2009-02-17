module: dylan-parser
synopsis: Grammars that recover from parse failures.

define class <skipped-token> (<token>)
end class;


define parser checked-type :: <token>
   rule choice(seq(type, req-next(checked-type-followers)),
               seq(checked-type-recovery))
   => tokens;
   yield tokens[0];
end;

define parser checked-expression :: <token>
   rule choice(seq(expression, req-next(checked-expression-followers)),
               seq(checked-expression-recovery))
   => tokens;
   yield tokens[0];
end;


/**
Generic Function: checked-followers
Synopsis: Checks if the current parse point has expected syntax.
**/

define method checked-followers (follows :: <sequence>, stream, context)
=> (result, succ?, err)
   let parser = apply(choice, follows);
   parser(stream, context);
end method;

define method checked-followers (follows, stream, context) => (result, succ?, err)
   values(#f, #t, #f);
end method;

define parser-method checked-type-followers (stream, context)
=> (result, succ? :: <boolean>, err :: false-or(<parse-failure>))
   label "valid input";
   checked-followers(attr(type-followers, default: #f), stream, context)
end;

define parser-method checked-expression-followers (stream, context)
=> (result, succ? :: <boolean>, err :: false-or(<parse-failure>))
   label "valid input";
   checked-followers(attr(expression-followers, default: #f), stream, context)
end;


/**
Generic Function: checked-recovery
Synopsis: Advances the parser to the next recovery point (using a skipping
parser) and signals a warning to the user.
**/

define method checked-recovery
   (skipper :: <function>, stream :: <canonical-text-stream>, context)
=> (result, succ?, err)
   // Skip to next graphic character before starting recovery, so that the range
   // of skipped input does not include end-of-line characters.
   for (c = peek(stream, on-end-of-stream: #f) then peek(stream, on-end-of-stream: #f),
        while: c & ~c.graphic?)
      read-element(stream)
   end for;

   let parser = skip(skipper);
   let start-pos = stream.stream-position;
   let (result :: false-or(<token>), succ? :: <boolean>, err) = parser(stream, context);
   let start-pos = (succ? & result.parse-start) | start-pos;
   let end-pos = (succ? & result.parse-end) | stream.stream-position;
   let source-location =
         source-location-from-stream-positions(context, start-pos, end-pos);
   unparsable-expression-in-code(location: source-location);
   values(result, succ?, err);
end method;

define method checked-recovery (skipper, stream, context) => (result, succ?, err)
   values(#f, #t, #f)
end method;

define parser-method checked-type-recovery (stream, context)
=> (result, succ? :: <boolean>, err :: false-or(<parse-failure>))
   label "unparsable input";
   checked-recovery(attr(type-skipper, default: #f), stream, context)
end;

define parser-method checked-expression-recovery (stream, context)
=> (result, succ? :: <boolean>, err :: false-or(<parse-failure>))
   label "unparsable input";
   checked-recovery(attr(expression-skipper, default: #f), stream, context)
end;


//
// Skipping parsers
//

define parser til-parsable (<skipped-token>)
   rule lines-til-parsable => tokens;
end;

define parser til-rt-paren (<skipped-token>)
   rule many(seq(not-next(lex-RT-PAREN),
                 choice(seq(lex-LF-PAREN, til-rt-paren, lex-RT-PAREN),
                        char)))
   => tokens;
end;

define parser til-class-clause (<skipped-token>)
   rule many(seq(not-next(seq(eol, lex-END)),
                 not-next(seq(lex-SEMICOLON, class-clause)),
                 char))
   => tokens;
end;
