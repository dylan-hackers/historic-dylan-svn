module: parser-common
synopsis: Common code for dylan-parser and markup-parser.


/** Tokens are equal if they are of the same class and cover the same range. **/
define method \= (tok1 :: <token>, tok2 :: <token>) => (equal? :: <boolean>)
   tok1 == tok2
      | (tok1.object-class == tok2.object-class
            & tok1.parse-start = tok2.parse-start
            & tok1.parse-end = tok2.parse-end)
end method;


define open abstract class <updatable-source-location-mixin> (<object>)
   slot source-location :: <source-location> = $unknown-source-location,
      init-keyword: #"source-location";
end class;


/** Synopsis: A token that has a user-identifiable origin. **/
define open class <source-location-token> (<token>, <updatable-source-location-mixin>)
end class;


/**
Synopsis: Parse context that includes information required to generate a source
location.

There are one or more wrapper streams between what the parser sees and the
underlying file on disk. This would normally screw up source location reporting
to the end user, but with the information in this class, a source location can
accurately reflect the file on disk.

--- Make Keywords: ---
file-locator               - The <file-locator> of the base file.
line-col-position-method   - A function. Given a stream position in the
                             parser-visible stream, the function should return
                             the corresponding line and column in the base file.
**/
define open class <file-parse-context> (<parse-context>)
   slot file-locator :: <file-locator>,
      required-init-keyword: #"file-locator";
   slot line-col-position :: <function>,
      required-init-keyword: #"line-col-position-method";
end class;


/** Synopsis: Parse context used for internal parsing. **/
define open class <internal-parse-context> (<parse-context>)
   slot locator :: <source-location>, required-init-keyword: #"locator";
end class;


/** Synopsis: Generate source location from stream positions. **/
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


define method source-location-from-stream-positions
   (context :: <internal-parse-context>, start-pos :: <integer>, end-pos :: <integer>)
=> (source-location :: <source-location>)
   context.locator
end method;


/** Synopsis: Saves source location of a parsed token. **/
define inline method note-source-location
   (context :: <file-parse-context>, value :: <source-location-token>)
=> ()
   value.source-location := source-location-from-stream-positions
         (context, value.parse-start, value.parse-end);
end method;


define inline method note-source-location
   (context :: <internal-parse-context>, value :: <source-location-token>)
=> ()
   value.source-location := context.locator;
end method;


/**
Synopsis: Saves source location encompassing one or more child tokens that are
subclasses of <token>.
**/
define inline method note-combined-source-location
   (context :: <file-parse-context>, value :: <source-location-token>, tokens)
=> ()
   span-token-positions(value, tokens);
   note-source-location(context, value);
end method;


define inline method note-combined-source-location
   (context :: <internal-parse-context>, value :: <source-location-token>, tokens)
=> ()
   note-source-location(context, value);
end method;


/**
Synopsis: Sets token parse span encompassing one or more child tokens that are
subclasses of <token>.
**/
define method span-token-positions
   (value :: <token>, tokens)
=> ()
   let (start-pos, end-pos) = combined-stream-positions(tokens);
   if (start-pos)
      value.parse-start := start-pos;
      value.parse-end := end-pos;
   end if;
end method;


define method combined-stream-positions (seq :: <sequence>)
=> (start-pos :: false-or(<integer>), end-pos :: false-or(<integer>))
   let pos-seq = make(<vector>, size: seq.size);
   for (item in seq, i from 0)
      let (start-pos, end-pos) = combined-stream-positions(item);
      pos-seq[i] := start-pos & pair(start-pos, end-pos);
   end for;
   let pos-seq = choose(true?, pos-seq);
   if (~pos-seq.empty?)
      let start-pos = apply(min, map(head, pos-seq));
      let end-pos = apply(max, map(tail, pos-seq));
      values(start-pos, end-pos)
   end if;
end method;

define method combined-stream-positions (obj :: <object>)
=> (start-pos :: singleton(#f), end-pos :: singleton(#f))
   values(#f, #f)
end method;

define method combined-stream-positions (token :: <token>)
=> (start-pos :: <integer>, end-pos :: <integer>)
   values(token.parse-start, token.parse-end)
end method;
