module: parser-common


define constant <string-vector> = limited(<vector>, of: <string>);
define variable *tabstop-fillers* :: <string-vector> = make(<string-vector>, size: 0);
         

define method canonical-text-stream (file :: <positionable-stream>)
=> (text-stream :: <string-stream>)
   unless (*tabstop-fillers*.size > 0)
      *tabstop-fillers* := make(<string-vector>, size: $tab-size, fill: "");
      for (tab-pos from 0 below $tab-size)
         let filler-size = $tab-size - tab-pos;
         *tabstop-fillers*[tab-pos] := make(<string>, size: filler-size, fill: ' ');
      end for
   end unless;

   local method next-line () => (line :: false-or(<string>))
            read-line(file, on-end-of-stream: #f)
         end method,

         method next-tab (line :: <string>) => (index :: false-or(<integer>))
            find-key(line,
                  method (c :: <character>) => (found? :: <boolean>)
                     c = '\t'
                  end)
         end method;

   let canonical-contents = make(<stretchy-vector>);
   for (line = next-line() then next-line(), while: line)
      // Detab
      for (tab-index = next-tab(line) then next-tab(line), while: tab-index)
         let filler = *tabstop-fillers*[modulo(tab-index, $tab-size)];
         line := replace-subsequence!(line, filler, start: tab-index, end: tab-index + 1)
      end for;
      
      // Remove ctrl chars and add eol
      local method valid-char? (c :: <character>) => (valid? :: <boolean>)
               c >= '\<20>' & c ~= '\<7f>'
            end;
      let clean-line = choose(valid-char?, line);
      canonical-contents := concatenate!(canonical-contents, clean-line, "\n");
   end for;
   make(<sequence-stream>, contents: as(<string>, canonical-contents))
end method;


/// Synopsis: Returns function that computes line and column for a particular
/// stream position.
define method line-col-position-func (stream :: <positionable-stream>)
=> (line-col-position :: <function>)
   let line-ends = make(<stretchy-vector>, of: <integer>);
   let current-pos = stream.stream-position;
   stream.stream-position := #"start";
   while (skip-through(stream, '\n'))
      line-ends := add!(line-ends, as(<integer>, stream.stream-position));
   end while;
   stream.stream-position := current-pos;
   
   // Binary search.
   local method line-col-position (pos :: <integer>)
         => (line :: <integer>, col :: <integer>)
            let part-start = 0;
            let part-end = line-ends.size;
            while (part-start < part-end)
               let pivot = part-start + floor/(part-end - part-start, 2);
               if (pos >= line-ends[pivot])
                  part-start := pivot + 1
               else
                  part-end := pivot
               end if;
            end while;
            let line = part-start;
            let col =
                  if (part-start > 0)
                     pos - line-ends[part-start - 1]
                  else
                     pos
                  end if;
            values(line + 1, col + 1)
         end method;
   
   line-col-position
end method;
