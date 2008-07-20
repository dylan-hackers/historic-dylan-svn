// Split a sequence into parts at each occurrance of the 'separator'
// and return a sequence containing the parts.  The sequence is
// searched from beginning to end for the given 'separator' and stops
// when it reaches the end of 'sequence' or when the size of the
// result reaches 'count' elements.  The meaning of the 'start' and
// 'end' parameters may differ for different methods, but the intent
// is that it be the same as if you passed in the subsequence delimited
// by 'start' and 'end'.  See the individual methods for details.
//
define open generic split
    (sequence :: <sequence>, separator :: <object>,
     #key start :: <integer> = 0,
          end: epos :: <integer>,
          count :: <integer>)
 => (parts :: <sequence>);

// This is in some sense the most basic method, since others can be
// implemented in terms of it.  The 'separator' function must accept
// three arguments: (1) the sequence in which to search for a
// separator, (2) the start index in that sequence at which to begin
// searching, and (3) the index at which to stop searching.  The
// 'separator' function must return #f to indicate that no separator
// was found, or two values: the start and end indices of the
// separator in the given sequence.  The initial start and end
// indices passed to the 'separator' function are the same as the
// 'start' and 'end' arguments passed to this method.  The
// 'separator' function should stay within the given bounds whenever
// possible.  (In particular it may not always be possible when the
// separator is a regex.)
define method split
    (seq :: <sequence>, find-separator :: <function>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = seq.size,
          count :: <integer> = epos + 1)
 => (parts :: <sequence>)
  reverse!(iterate loop (bpos :: <integer> = start,
                         parts :: <list> = #(),
                         nparts :: <integer> = 1)
             let (sep-start, sep-end) = find-separator(seq, bpos, epos);
             if (sep-start & sep-end & (sep-end <= epos) & (nparts < count))
               loop(sep-end,
                    pair(copy-sequence(seq, start: bpos, end: sep-start), parts),
                    nparts + 1)
             else
               pair(copy-sequence(seq, start: bpos, end: epos), parts)
             end
           end)
end method split;

// Splits seq around occurrances of the separator subsequence.
// Works for the relatively common case where seq and separator
// are both <string>s.
define method split
    (seq :: <sequence>, separator :: <sequence>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = seq.size,
          count :: <integer> = epos + 1,
          test :: <function> = \==)
 => (parts :: <sequence>)
  // Is there a function that does this already?
  local method looking-at? (pattern :: <sequence>, big :: <sequence>,
                            bpos :: <integer>)
          block (return)
            let len :: <integer> = big.size;
            for (thing in pattern, pos from bpos)
              if (pos >= len | ~test(thing, big[pos]))
                return(#f)
              end if;
            end for;
            #t
          end
        end method looking-at?;
  local method find-subseq (seq :: <sequence>,
                            bpos :: <integer>,
                            epos :: false-or(<integer>))
          // Note that this only splits on the separator sequence if it is
          // entirely contained between the start and end positions.
          let epos :: <integer> = epos | seq.size;
          let max-separator-start :: <integer> = epos - separator.size;
          block (exit-loop)
            for (seq-index from bpos to max-separator-start)
              if (looking-at?(separator, seq, seq-index))
                exit-loop(seq-index, seq-index + separator.size);
              end;
            end;
            #f      // separator not found
          end
        end;
  split(seq, find-subseq, start: start, end: epos, count: count);
end method split;

// Split on a given object.
// This handles the common (<string>, <character>) case.
define method split
    (seq :: <sequence>, separator :: <object>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = seq.size,
          count :: <integer> = epos + 1,
          test :: <function> = \==)
 => (parts :: <sequence>)
  local method find-pos (seq :: <sequence>,
                         bpos :: <integer>,
                         epos :: false-or(<integer>))
          // Unfortunately the position function doesn't accept
          // start and end parameters so we have to write our own.
          block (exit-loop)
            for (i from bpos below epos)
              if (test(seq[i], separator))
                exit-loop(i, i + 1)
              end;
            end;
            #f
          end block
        end method;
  split(seq, find-pos, start: start, end: epos, count: count);
end method split;

// Join several sequences together, including a separator between each sequence.
define open generic join
    (items :: <sequence>, separator :: <sequence>, #key key, conjunction)
 => (joined :: <sequence>);

// join(range(from: 1, to: 3), ", ",
//      key: integer-to-string,
//      conjunction: " and ");
// => "1, 2 and 3"

define method join
    (sequences :: <sequence>, separator :: <sequence>,
     #key key :: <function> = identity,
          conjunction :: false-or(<sequence>))
 => (joined :: <sequence>)
  let length :: <integer> = sequences.size;
  if (length == 0)
    error("Attempt to join an empty sequence.")
  elseif (length == 1)
    key(sequences[0])
  else
    let result-size :: <integer>
      = (reduce(method (len, seq)
                  len + seq.size
                end,
                0,
                sequences)
           + (separator.size * (length - 1))
           + if (conjunction)
               // the last separator is replaced by the conjunction
               conjunction.size - separator.size
             else
               0
             end);
    let first = key(sequences[0]);   // don't call key > once on sequences[0]
    let result = make(object-class(first), size: result-size);
    let result-index :: <integer> = 0;
    local method copy-to-result (seq :: <sequence>)
            result := replace-subsequence!(result, seq, start: result-index);
            result-index := result-index + seq.size;
          end;
    copy-to-result(first);
    let max-index :: <integer> = length - 1;
    for (i :: <integer> from 1 to max-index)
      let seq :: <sequence> = sequences[i];
      copy-to-result(if(conjunction & i == max-index)
                       conjunction
                     else
                       separator
                     end);
      copy-to-result(key(seq));
    end;
    result
  end if
end method join;
    
