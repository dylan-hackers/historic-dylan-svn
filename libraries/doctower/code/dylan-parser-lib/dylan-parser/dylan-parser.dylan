module: dylan-parser
synopsis: Parser manager.


define class <dylan-parse-context> (<file-parse-context>)
   slot last-whitespace-doc :: false-or(<markup-content-token>) = #f;
   slot source-stream :: <positionable-stream>,
      required-init-keyword: #"source-stream";
end class;


/**
Synopsis: Entry point into parsing.

--- Conditions: --- 
Signals error if stream has syntax error.

--- Arguments: ---
text -
   A <positionable-stream>. The stream may be one or more wrapper streams over
   the base disk file.
text-line-col-position -
   A function on <stream-position> or <integer>. The function should return the
   line and column of the base disk file at the given 'text' stream position.
locator -
   The <file-locator> of the base disk file.
**/
define method parse-dylan-file
   (text :: <positionable-stream>, text-line-col-position :: <function>,
    locator :: <file-locator>)
=> (file-token :: <interchange-file-token>)
   let context = make(<dylan-parse-context>,
         cache-stream: text,
         source-stream: text,
         file-locator: locator,
         line-col-position-method: text-line-col-position);
            
   // *parser-trace* := *standard-output*;
   // *parser-cache-hits* := #t;

   let (file-token, success?, failure) = parse-interchange-file(text, context);

   // *parser-trace* := #f;
   // for (e keyed-by k in context.parser-cache-hits)
   //    if (e > 1) log("%5d %s", e, k) end;
   // end for;

   if (success?)
      file-token
   else
      let loc = source-location-from-stream-positions
            (context, failure.failure-position, failure.failure-position);
      parse-error-in-dylan(location: loc, expected: failure.parse-expected);
   end if;
end method;
