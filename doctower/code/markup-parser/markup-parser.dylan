module: markup-parser
synopsis: Parser initialization, pre-processing, and overall control.


/// Synopsis: Entry point into parsing.
/// Conditions: Throws <parse-failure> if stream has syntax error.
define method parse-markup
   (text :: <canonical-text-stream>, locator :: <file-locator>)
=> (markup-token :: <markup-content-token>)
   let (indented-stream, close-indented-stream, text-stream-position) =
         text.stream-with-indentation;
   block ()
      // *parser-trace* := *standard-output*;
      // *parser-cache-hits* := #t;
   
      let context = make(<file-parse-context>,
            cache-stream: indented-stream,
            file-locator: locator,
            line-col-position-method:
               method (pos) => (line, col)
                  let text-pos = text-stream-position(pos);
                  line-col-position(text, at: text-pos);
               end);

      let (markup-token, success?, failure) =
            parse-markup-block(indented-stream, context);

      // for (e keyed-by k in context.parser-cache-hits)
      //    if (e > 1) log("%5d %s", e, k) end;
      // end for;

      if (success?)
         log-object("Markup", markup-token);
         markup-token;
      else
         error(failure);
      end if;
   cleanup
      close-indented-stream();
   end block;
end method;


/// Synopsis: Creates a stream with trailing and leading spaces removed or
/// replaced with INDENT or DEDENT tokens.
define method stream-with-indentation (text :: <canonical-text-stream>)
=> (new-stream :: <stream>, close-stream :: <function>, stream-position :: <function>)
   let inner-stream = make(<sequence-stream>,
                           contents: stream-contents(text, clear-contents?: #f),
                           element-type: <object>);
   let stream = make(<replacing-stream>, inner-stream: inner-stream);
   let levels = make(<deque>);
   push(levels, 0);
   until (stream.stream-at-end?)

      // Replace leading spaces with nothing, INDENTs or DEDENTs, similar to
      // the Python parser. Ignore completely blank lines.
      let sol = stream.stream-position;
      let spaces = count-spaces(stream);
      unless (stream.peek = '\n')
         let spaces-replacement =
            case
               spaces > levels.first =>
                  push(levels, spaces);
                  #[#"indent"];
               spaces = levels.first =>
                  #[];
               spaces < levels.first =>
                  // Dedent until we clear the missing levels. Then, if there are
                  // spaces remaining, add an indent to take care them. That last
                  // part differs from Python logic.
                  let reps = make(<stretchy-vector>);
                  while (spaces < levels.first)
                     pop(levels);
                     reps := add!(reps, #"dedent");
                  end while;
                  if (spaces ~= levels.first)
                     push(levels, spaces);
                     reps := add!(reps, #"indent");
                  end if;
                  as(<simple-vector>, reps);
            end case;
         add-replacement-contents(stream, spaces-replacement,
                                  start: sol, end: stream.stream-position);
      end unless;

      // Remove trailing spaces. Save starting position of a run of space
      // characters, remove them if still in run when reaching eol.
      let in-spaces = #f;
      for (char-pos = stream.stream-position then stream.stream-position,
           char = read-element(stream) then read-element(stream),
           until: char = '\n')
         // Save starting position of a run of space characters.
         in-spaces := (char = ' ' & (in-spaces | char-pos));
      end for;
      if (in-spaces)
         // Remove spaces up to '\n.'
         adjust-stream-position(stream, -1);
         add-replacement-contents(stream, "", start: in-spaces,
                                  end: stream.stream-position);
         adjust-stream-position(stream, +1);
      end if;
   end until;

   // Add dedents until stack is returned to initial state. Ensure stream still
   // ends with '\n.'
   let remaining-dedents = make(<vector>, size: levels.size - 1, fill: #"dedent");
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
             inner-stream.close;
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
