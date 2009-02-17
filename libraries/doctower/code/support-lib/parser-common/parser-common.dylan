module: parser-common
synopsis: Common code for dylan-parser and markup-parser.


/// Synopsis: A token that has a user-identifiable origin.
define open class <source-location-token> (<token>)
   slot source-location :: <source-location> = make(<unknown-source-location>);
end class;


/// Synopsis: Parse context that includes information required to generate a
/// source location.
define open class <file-parse-context> (<parse-context>)
   slot file-locator :: <file-locator>,
      required-init-keyword: #"file-locator";
   slot line-col-position :: <function>,
      required-init-keyword: #"line-col-position-method";
end class;


/// Synopsis: Generate source location from stream positions.
define method source-location-from-stream-positions
   (context :: <file-parse-context>, start-pos :: <integer>, end-pos :: <integer>)
=> (source-location :: <source-location>)
   let end-pos = max(start-pos, end-pos - 1);
   let (start-line, start-col) = context.line-col-position(start-pos);
   let (end-line, end-col) = context.line-col-position(end-pos);
   make(<file-source-location>, file: context.file-locator,
        start-line: start-line, start-column: start-col,
        end-line: end-line, end-column: end-col);
end method;


/// Synopsis: Saves source location of a parsed token.
define inline method note-source-location
   (context :: <file-parse-context>, value :: <source-location-token>)
=> ()
   value.source-location :=
         source-location-from-stream-positions(context, value.parse-start, value.parse-end);
end method;


/// Synopsis: Saves source location encompassing several child tokens.
define method note-combined-source-location
   (context :: <file-parse-context>, value :: <source-location-token>, tokens)
=> ()
   let location = combined-source-locations(context, tokens);
   if (location) value.source-location := location end;
end method;

define method combined-source-locations
   (context :: <file-parse-context>, seq :: <sequence>)
=> (source-location :: false-or(<file-source-location>))
   let locations = map(curry(combined-source-locations, context), seq);
   if (locations.empty?)
      #f
   else
      reduce1(merge-file-source-locations, choose(true?, locations))
   end if;
end method;

define method combined-source-locations
   (context :: <file-parse-context>, obj :: <object>)
=> (source-location :: singleton(#f))
   #f
end method;

define method combined-source-locations
   (context :: <file-parse-context>, token :: <token>)
=> (source-location :: <file-source-location>)
   source-location-from-stream-positions(context, token.parse-start, token.parse-end)
end method;

define method combined-source-locations
   (context :: <file-parse-context>, token :: <source-location-token>)
=> (source-location :: <file-source-location>)
   token.source-location
end method;

