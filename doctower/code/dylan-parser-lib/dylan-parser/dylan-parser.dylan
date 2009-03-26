module: dylan-parser
synopsis: Parser manager.


define class <dylan-parse-context> (<file-parse-context>)
   slot last-whitespace-doc :: false-or(<doc-comment-token>) = #f;
end class;


/// Synopsis: Entry point into parsing.
/// Conditions: Throws <parse-failure> if stream has syntax error.
define method parse-dylan-file
   (text :: <canonical-text-stream>, locator :: <file-locator>)
=> (file-token :: <interchange-file-token>)
   // *parser-trace* := *standard-output*;
   // *parser-cache-hits* := #t;

   let context = make(<dylan-parse-context>,
         cache-stream: text,
         file-locator: locator,
         line-col-position-method:
            method (pos) => (line, col)
               line-col-position(text, at: pos)
            end);
            
   let (file-token, success?, failure) = parse-interchange-file(text, context);

   // for (e keyed-by k in context.parser-cache-hits)
   //    if (e > 1) log("%5d %s", e, k) end;
   // end for;

   if (success?)
      // log-object("Interchange file", file-token);
      file-token;
   else
      error(failure);
   end if;
end method;
