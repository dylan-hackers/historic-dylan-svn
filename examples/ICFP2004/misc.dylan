module: ants

define method split-at-whitespace (string :: <byte-string>)
    => res :: <list>;
  split-at(method (x :: <character>) x <= ' ' end, string);
end method split-at-whitespace;


// Split a string at locations where test returns true, removing the delimiter
// characters.
define method split-at (test :: <function>, string :: <byte-string>)
    => res :: <list>;
  let size = string.size;
  local
    method scan (posn :: <integer>, results :: <list>)
        => res :: <list>;
      if (posn == size)
        results;
      elseif (test(string[posn]))
        scan(posn + 1, results);
      else
        copy(posn + 1, posn, results);
      end;
    end method scan,
    method copy (posn :: <integer>, start :: <integer>,
                 results :: <list>)
        => res :: <list>;
      if (posn == size | test(string[posn]))
        scan(posn,
             pair(copy-sequence(string, start: start, end: posn), results));
      else
        copy(posn + 1, start, results);
      end;
    end method copy;
  reverse!(scan(0, #()));
end method split-at;
