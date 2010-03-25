module: markup-parser
synopsis: Parser initialization, pre-processing, and overall control.


/**
Synopsis: Entry point into parsing a user-supplied file.

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
define method parse-markup
   (text :: <positionable-stream>, text-line-col-position :: <function>,
    locator :: <file-locator>)
=> (markup-token :: <markup-content-token>)
   let (indented-stream, close-indented-stream, text-stream-position) =
         text.stream-with-indentation;

   block ()
      let context = make(<file-parse-context>,
            cache-stream: indented-stream,
            file-locator: locator,
            line-col-position-method:
               compose(text-line-col-position, text-stream-position));

      // *parser-trace* := *standard-output*;
      // *parser-cache-hits* := #t;
   
      let (markup-token, success?, extent) =
            parse-markup-block(indented-stream, context);

      // *parser-trace* := #f;
      // for (e keyed-by k in context.parser-cache-hits)
      //    if (e > 1) log("%5d %s", e, k) end;
      // end for;

      if (success?)
         markup-token;
      else
         let loc = source-location-from-stream-positions
               (context, extent.parse-position, extent.parse-position);
         parse-error-in-markup(location: loc, expected: extent.parse-expected);
      end if;
   cleanup
      close-indented-stream();
   end block;
end method;


/**
Synopsis: Entry point into parsing a filled-out topic template stream.

--- Conditions: --- 
Signals error if stream has syntax error.

--- Arguments: ---
text -
   A <positionable-stream>. The stream may be one or more wrapper streams over
   the base disk file.
locator -
   The <source-location> to use in tokens.
**/
define method parse-internal-markup
   (text :: <positionable-stream>, locator :: <source-location>)
=> (markup-token :: <markup-content-token>)
   let (indented-stream, close-indented-stream, text-stream-position) =
         text.stream-with-indentation;

   block ()
      let context = make(<internal-parse-context>, cache-stream: text,
                         locator: locator);

      // *parser-trace* := *standard-output*;
   
      let (markup-token, success?, extent) =
            parse-markup-block(indented-stream, context);

      // *parser-trace* := #f;
   
      if (success?)
         markup-token;
      else
         error("Failed to parse internal markup at stream position %d: %=",
               extent.parse-position, extent.parse-expected);
      end if;
   cleanup
      close-indented-stream();
   end block;
end method;


/// Synopsis: Creates a stream with INDENT or DEDENT tokens and without trailing
/// spaces.
define method stream-with-indentation (text :: <stream>)
=> (new-stream :: <stream>, close-stream :: <function>, stream-position :: <function>)
   let stream = make(<replacing-stream>, inner-stream: text);
   let levels = make(<deque>);
   push(levels, 0);
   until (stream.stream-at-end?)

      // Add INDENTs or DEDENTs before leading spaces. The INDENT and DEDENT logic
      // is similar to the Python parser. Ignore completely blank lines.
      let sol = stream.stream-position;
      let spaces = count-spaces(stream);
      unless (stream.peek = '\n')
         let spaces-replacement =
            case
               spaces > levels.first =>
                  push(levels, spaces);
                  as(<string>, $indent);
               spaces = levels.first =>
                  "";
               spaces < levels.first =>
                  // Dedent until we clear the missing levels. Then, if there are
                  // spaces remaining, add an indent to take care them. That last
                  // part differs from Python logic.
                  let reps = make(<stretchy-vector>);
                  while (spaces < levels.first)
                     pop(levels);
                     reps := add!(reps, $dedent);
                  end while;
                  if (spaces ~= levels.first)
                     push(levels, spaces);
                     reps := add!(reps, $indent);
                  end if;
                  as(<string>, reps);
            end case;
         add-replacement-contents(stream, spaces-replacement, start: sol, end: sol);
      end unless;

      // Remove trailing spaces. Save starting position of a run of space
      // characters, remove them if still in run when reaching eol.
      let spaces-start = #f;
      for (char-pos = stream.stream-position then stream.stream-position,
           char = read-element(stream) then read-element(stream),
           until: char = '\n')
         // Save starting position of a run of space characters.
         if (char ~= ' ')
            spaces-start := #f
         elseif (~spaces-start)
            spaces-start := char-pos
         end if;
      end for;
      if (spaces-start)
         // Remove spaces up to '\n.'
         adjust-stream-position(stream, -1);
         add-replacement-contents(stream, "", start: spaces-start,
                                  end: stream.stream-position);
         adjust-stream-position(stream, +1);
      end if;
   end until;

   // Add dedents until stack is returned to initial state. Ensure stream still
   // ends with '\n.'
   let remaining-dedents = make(<string>, size: levels.size - 1, fill: $dedent);
   if (remaining-dedents.size > 0)
      let eos = stream.stream-size;
      let (foo, eos) = add-replacement-contents(stream, remaining-dedents,
                                                start: eos, end: eos);
      let (foo, foo) = add-replacement-contents(stream, "\n",
                                                start: eos, end: eos);
   end if;
   
   stream.stream-position := #"start";
   values(stream,
          method ()
             stream.close;
          end,
          method (pos) => (pos)
             inner-stream-position(stream, at: pos)
          end);
end method;


define method count-spaces (stream)
=> (count :: <integer>)
   let (elems, found?) = read-to(stream, ' ', test: \~=, on-end-of-stream: #f);
   adjust-stream-position(stream, -1); // because read-to reads non-space delimiter
   (found? & elems.size) | 0;
end method;
