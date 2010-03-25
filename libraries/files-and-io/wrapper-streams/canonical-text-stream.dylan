module: canonical-text-stream
author: Dustin Voss


// TODO: Not sure, but I think rather than inheriting from <basic-wrapper-stream>,
// this should inherit from <wrapper-stream> and the inner stream should be wrapped
// in <basic-wrapper-stream> if necessary.


/**
Class: <canonical-text-stream>
------------------------------

A text stream that detabs, removes or replaces control characters, standardizes
line endings, and tracks row and column.

NOTE: 'inner-stream' will return a <replacing-stream>. That stream's inner stream
will be the 'inner-stream'.

Make keywords:
   tabstop-size  - Size of a tab stop. Defaults to 8.
   end-of-line   - Canonical end-of-line sequence. Defaults to "\n".
   control-chars - Replacement for control characters (0-31 and 127), or #f to
                   leave control characters alone. Defaults to "".
*/

define class <canonical-text-stream> (<basic-wrapper-stream>, <positionable-stream>)
   constant slot tabstop-size :: <integer> = 8, init-keyword: #"tabstop-size";
   constant slot eol :: <string> = "\n", init-keyword: #"end-of-line";
   constant slot control :: false-or(<string>) = "", init-keyword: #"control-chars";
   slot line-positions = make(<stretchy-vector>);
   slot tabstop-fillers :: <vector>;
   slot unchecked-position :: <integer> = 0;
   slot checked-final-eol? :: <boolean> = #f;
   keyword inner-stream:, type: <positionable-stream>;
end class;


define method make (cts-class == <canonical-text-stream>, #rest keys, 
                    #key inner-stream)
=> (object :: <object>)
   // TODO: I shouldn't really do this. The inner-stream returned by <c-t-s>
   // won't be what the user expects.
   let replacing-stream = make(<replacing-stream>, inner-stream: inner-stream);
   apply(next-method, cts-class, inner-stream:, replacing-stream, keys);
end method;


define method initialize (cts :: <canonical-text-stream>, #key) => ()
   next-method();
   assert(subtype?(cts.inner-stream.stream-element-type, <character>),
          "Element type of inner stream must be <character>");
   cts.line-positions[0] := 0;
   cts.tabstop-fillers := make(<vector>, size: cts.tabstop-size);
   for (i from 0 below cts.tabstop-size)
      let filler-size = cts.tabstop-size - i;
      cts.tabstop-fillers[i] := make(<string>, size: filler-size, fill: ' ');
   end for;
end method;


define method read-element
   (cts :: <canonical-text-stream>, #rest keys, #key on-end-of-stream)
=> (elem :: <object>)
   check-elements-to-stream-position(cts);
   apply(read-element, cts.inner-stream, keys);
end method;


define method peek
   (cts :: <canonical-text-stream>, #rest keys, #key on-end-of-stream)
=> (elem :: <object>)
   check-elements-to-stream-position(cts);
   apply(peek, cts.inner-stream, keys);
end method;


define method write-element (cts :: <canonical-text-stream>, elem :: <object>)
=> ()
   check-elements-to-stream-position(cts);
   write-element(cts.inner-stream, elem);
end method;


define method stream-position-setter
   (position :: <integer>, cts :: <canonical-text-stream>)
=> (position :: <integer>)
   cts.inner-stream.stream-position := position;
   let new-pos = check-elements-to-stream-position(cts);
   if (position ~= new-pos)
      cts.inner-stream.stream-position := position;
   else
      position;
   end if;
end method;


define method stream-position-setter
   (position == #"start", cts :: <canonical-text-stream>)
=> (position :: <integer>)
   adjust-stream-position(cts, 0, from: #"start");
end method;


define method stream-position-setter
   (position == #"end", cts :: <canonical-text-stream>)
=> (position :: <integer>)
   adjust-stream-position(cts, 0, from: #"end");
end method;


define method adjust-stream-position
   (cts :: <canonical-text-stream>, delta :: <integer>,
    #key from :: one-of(#"current", #"start", #"end") = #"current")
=> (new-position :: <integer>)
   adjust-stream-position(cts.inner-stream, delta, from: from);
   check-elements-to-stream-position(cts);
end method;


define method stream-size (cts :: <canonical-text-stream>)
=> (sz :: <integer>)
   let saved = cts.stream-position;
   cts.stream-position := #"end";
   let sz = next-method();
   cts.stream-position := saved;
   sz
end method;


define method stream-contents
   (cts :: <canonical-text-stream>, #key clear-contents?)
=> (contents :: <sequence>)
   let saved = cts.stream-position;
   cts.stream-position := #"end";
   let cont = next-method();
   cts.stream-position := saved;
   cont
end method;


define method stream-contents-as
   (type :: <type>, cts :: <canonical-text-stream>, #key clear-contents?)
=> (contents :: <sequence>)
   let saved = cts.stream-position;
   cts.stream-position := #"end";
   let cont = next-method();
   cts.stream-position := saved;
   cont
end method;


/// Synopsis: Returns current line and column position.
/// Arguments:
///   cts   - An instance of <canonical-text-stream>.
///   at:   - An instance of false or <integer>. The line and column of the
///           current (if false) or given (if <integer>) stream position will
///           be returned. Defaults to #f.
/// Values:
///   line  - An instance of <integer>. Line number, starting with 1.
///   col   - An instance of <integer>. Column number, starting with 1.
define method line-col-position
   (cts :: <canonical-text-stream>, #key at :: false-or(<integer>) = #f)
=> (line :: <integer>, col :: <integer>)

   local method current-line-col-position (cts :: <canonical-text-stream>)
         => (line :: <integer>, col :: <integer>)
            let pos = check-elements-to-stream-position(cts);
            let line = find-last-key(cts.line-positions, rcurry(\<=, pos));
            values(line + 1, pos - cts.line-positions[line] + 1);
         end method;

   if (at)
      let saved-pos = cts.stream-position;
      block ()
         cts.stream-position := at;
         cts.current-line-col-position;
      cleanup
         cts.stream-position := saved-pos;
      end block;
   else
      cts.current-line-col-position;
   end if;
end method;


define function check-elements-to-stream-position (cts :: <canonical-text-stream>)
=> (new-pos :: <integer>)
   let inner = cts.inner-stream;
   let position = inner.stream-position;
   when (position >= cts.unchecked-position)

      // We make canonical replacements of \t, \r, \n and control characters up to
      // and including current stream position. If current stream position is also
      // eos, we ensure the stream is terminated by eol. In scanning the stream, we
      // must change the stream position. We save the original stream position,
      // scan the stream but do not actually perform any replacements, restore the
      // original stream position, and then do all the replacements at once. The
      // replacing automatically adjusts the stream position, saving us from having
      // to figure out the new position ourselves.
      
      let replacements = make(<stretchy-vector>);
      local method add-replacement (start-pos, end-pos, str, orig)
               let new-rep = make(<vector>, size: 4);
               new-rep[0] := start-pos;
               new-rep[1] := end-pos;
               new-rep[2] := str;
               new-rep[3] := (str ~= orig);  // #t if replacement actually needed
               replacements := add!(replacements, new-rep);
            end method;
            
      let cur-col = cts.unchecked-position - cts.line-positions.last;
      let cur-pos = cts.unchecked-position;
      inner.stream-position := cts.unchecked-position;
      
      // Plan replacements.
      while (cur-pos <= position & ~inner.stream-at-end?)
         let elem = read-element(inner);
         case
            elem = '\t' =>
               let filler = cts.tabstop-fillers[modulo(cur-col, cts.tabstop-size)];
               add-replacement(cur-pos, cur-pos + 1, filler, #f);
               cur-pos := cur-pos + 1;
               cur-col := cur-col + filler.size;
            elem = '\n' =>
               add-replacement(cur-pos, cur-pos + 1, cts.eol, "\n");
               cur-pos := cur-pos + 1;
               cur-col := 0;
            elem = '\r' =>
               let new-pos = cur-pos + 1;
               let orig-eol = 
                     if (peek(inner, on-end-of-stream: #f) == '\n')
                        read-element(inner);
                        new-pos := new-pos + 1;
                        "\r\n";
                     else
                        "\r";
                     end if;
               add-replacement(cur-pos, new-pos, cts.eol, orig-eol);
               cur-pos := new-pos;
               cur-col := 0;
            cts.control & (elem < '\<20>' | elem = '\<7F>') =>
               add-replacement(cur-pos, cur-pos + 1, cts.control,
                               make(<string>, size: 1, fill: elem));
               cur-pos := cur-pos + 1;
               cur-col := cur-col + cts.control.size;
            otherwise =>
               cur-pos := cur-pos + 1;
               cur-col := cur-col + 1;
         end case;
      end while;
      
      // If we scanned to end of stream, ensure that it has a preceding eol.
      // We cannot reasonably check the last few bytes of the stream, and we
      // cannot rely on unchecked-position because eos always qualifies as a
      // position to check and thus eol would always be added, so we just use
      // a flag to indicate we already checked it and it is good.
      when (~cts.checked-final-eol? & inner.stream-at-end?)
         let needs-eol = replacements.empty? |
               ~(replacements.last[2] == cts.eol &
                 replacements.last[1] == inner.stream-limit);
         when (needs-eol)
            add-replacement(inner.stream-limit, inner.stream-limit, cts.eol, #f);
         end when;
         cts.checked-final-eol? := #t;
      end when;
      
      // Do replacements in a batch. Update line positions and track cumulative
      // offsets (so we can adjust replacement start positions) as we go.
      inner.stream-position := position;
      let offset = 0;
      for (rep in replacements)
         let start-pos = rep[0] + offset;
         let end-pos = rep[1] + offset;
         let rep-str = rep[2];
         let rep-end =
               if (rep[3])
                  let (new-start, new-end) = add-replacement-contents
                        (inner, rep-str, start: start-pos, end: end-pos);
                  offset := offset + (rep-str.size - (end-pos - start-pos));
                  new-end
               else
                  end-pos
               end if;

         cts.unchecked-position := rep-end;
         
         // Was it a new line?
         if (rep-str == cts.eol)
            cts.line-positions := add!(cts.line-positions, rep-end);
         end if;
      end for;
      
      position := inner.stream-position;
   end when;
   position
end function;
