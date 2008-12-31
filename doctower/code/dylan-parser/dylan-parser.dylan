module: dylan-parser
synopsis: Parser manager.


/// Synopsis: Entry point into parsing.
/// Conditions: Throws <parse-failure> if stream has syntax error.
define method parse-dylan-file
   (text :: <canonical-text-stream>, locator :: <file-locator>)
=> (source-token :: <source-record-token>)
   // *parser-trace* := *standard-output*;
   // *parser-cache-hits* := #t;

   let context = make(<file-parse-context>,
         cache-stream: text,
         file-locator: locator,
         line-col-position-method:
            method (pos) => (line, col)
               line-col-position(text, at: pos)
            end);
            
   let (source-token, success?, failure) = parse-source-file(text, context);

   // for (e keyed-by k in context.parser-cache-hits)
   //    if (e > 1) log("%5d %s", e, k) end;
   // end for;

   if (success?)
      log-object("Source Record", source-token);
      source-token;
   else
      error(failure);
   end if;
end method;
