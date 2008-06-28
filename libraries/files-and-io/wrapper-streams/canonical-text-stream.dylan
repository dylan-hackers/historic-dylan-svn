module: canonical-text-stream
author: Dustin Voss

/**
Class: <canonical-text-stream>
------------------------------

A text stream that detabs, standardizes line endings, and tracks row and column.

Make keywords:
   tabstop-size - Size of a tab stop. Defaults to 8.
   end-of-line  - Canonical end-of-line sequence. Defaults to "\n".
*/

define class <canonical-text-stream> (<basic-wrapper-stream>)
   constant slot tabstop-size :: <integer> = 8, init-keyword: #"tabstop-size";
   constant slot eol :: <string> = "\n", init-keyword: #"end-of-line";
   constant slot line-positions = make(<stretchy-vector>);
   slot tabstop-fillers :: <vector>;
   slot unchecked-position :: <integer> = 0;
end class;


define method initialize (cts :: <canonical-text-stream>, #key) => ()
   next-method();
   assert(subtype?(cts.inner-stream.stream-element-type, <character>),
          "Element type of inner stream must be <character>");
   cts.inner-stream := make(<replacing-stream>, inner-stream: cts.inner-stream);
   cts.line-positions[0] := 0;
   cts.tabstop-fillers := make(<vector>, size: cts.tabstop-size);
   for (i from 0 below cts.tabstop-size)
      let filler-size = cts.tabstop-size - i;
      cts.tabstop-fillers[i] := make(<string>, size: filler-size, fill: ' ');
   end for;
end method;


define method read-element
   (cts :: <canonical-text-stream>, #rest keys, #key on-end-of-stream)
=> (elem :: <character>)
   check-elements-to-stream-position(cts);
   next-method();
end method;


define method peek
   (cts :: <canonical-text-stream>, #rest keys, #key on-end-of-stream)
=> (elem :: <character>)
   check-elements-to-stream-position(cts);
   next-method();
end method;


define method write-element (cts :: <canonical-text-stream>, elem :: <object>)
=> ()
   check-elements-to-stream-position(cts);
   next-method();
end method;


define method stream-position-setter
   (position :: <integer>, cts :: <canonical-text-stream>)
=> (position :: <integer>)
   next-method();
   let new-pos = check-elements-to-stream-position(cts);
   if (position ~= new-pos)
      next-method();
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
   next-method();
   check-elements-to-stream-position(cts);
end method;


/// Synopsis: Returns current row and column position, counting from 1.
define method row-col-position (cts :: <canonical-text-stream>)
=> (row :: <integer>, col :: <integer>)
   let pos = check-elements-to-stream-position(cts);
   let row = find-last-key(cts.line-positions, rcurry(\<=, pos));
   values(row + 1, pos - cts.line-positions[row] + 1)
end method;


define function check-elements-to-stream-position (cts :: <canonical-text-stream>)
=> (new-pos :: <integer>)
   let inner = cts.inner-stream;
   let position = inner.stream-position;
   when (position >= cts.unchecked-position)

      // We make canonical replacements of \t, \r, \n up to and including
      // current stream position. If current stream position is also eos,
      // we ensure the stream is terminated by eol. In scanning the stream, we
      // must change the stream position. We save the original stream position,
      // scan the stream but do not actually perform any replacements, restore
      // the original stream position, and then do all the replacements at once.
      // The replacing automatically adjusts the stream position, saving us
      // from having to figure out the new position ourselves.
      
      let replacements = make(<stretchy-vector>);
      local method add-replacement (start-pos, end-pos, str)
               let new-rep = make(<vector>, size: 3);
               new-rep[0] := start-pos;
               new-rep[1] := end-pos;
               new-rep[2] := str;
               replacements := add!(replacements, new-rep);
            end method;
            
      let cur-col = cts.unchecked-position - cts.line-positions.last;
      let cur-pos = cts.unchecked-position;
      inner.stream-position := cts.unchecked-position;
      
      // Plan replacements.
      while (cur-pos <= position & ~inner.stream-at-end?)
         let elem = read-element(inner);
         select (elem)
            '\t' =>
               let filler = cts.tabstop-fillers[modulo(cur-col, cts.tabstop-size)];
               add-replacement(cur-pos, cur-pos + 1, filler);
               cur-pos := cur-pos + 1;
               cur-col := cur-col + filler.size;
            '\n' =>
               add-replacement(cur-pos, cur-pos + 1, cts.eol);
               cur-pos := cur-pos + 1;
               cur-col := 0;
            '\r' =>
               let new-pos = cur-pos + 1;
               if (peek(inner, on-end-of-stream: #f) == '\n')
                  read-element(inner);
                  new-pos := new-pos + 1;
               end if;
               add-replacement(cur-pos, new-pos, cts.eol);
               cur-pos := new-pos;
               cur-col := 0;
            otherwise =>
               cur-pos := cur-pos + 1;
               cur-col := cur-col + 1;
         end select;
      end while;
      
      // If we scanned to end of stream, ensure that it has a preceding eol.
      when (inner.stream-at-end?)
         let needs-eol = replacements.empty? |
               ~(replacements.last[2] == cts.eol &
                 replacements.last[1] == inner.stream-limit);
         when (needs-eol)
            add-replacement(inner.stream-limit, inner.stream-limit, cts.eol)
         end when;
      end when;
      
      // Do replacements in a batch. Update line positions and track cumulative
      // offsets (so we can adjust replacement start positions) as we go.
      inner.stream-position := position;
      let offset = 0;
      for (rep in replacements)
         let start-pos = rep[0] + offset;
         let end-pos = rep[1] + offset;
         let rep-str = rep[2];

         let (new-start, new-end) =
               add-replacement-contents(inner, rep-str, start: start-pos, end: end-pos);
         offset := offset + (rep-str.size - (end-pos - start-pos));
         cts.unchecked-position := new-end;
         
         // Was it a new line?
         if (rep-str == cts.eol)
            add!(cts.line-positions, new-end);
         end if;
      end for;
      
      position := inner.stream-position;
   end when;
   position
end function;
