module: sha1
author: Hannes Mehnert

define constant <vector-of-double-integers>
  = limited(<simple-vector>, of: <double-integer>);

define method string-to-double-integer-vector (s :: <byte-vector>)
  => (ret :: <vector-of-double-integers>)
  let ret = make(<vector-of-double-integers>, size: truncate/(size(s), 4));
  for (i from 0 below size(ret))
    for (j from 0 below 4)
      ret[i] := ash(ret[i], 8);
      ret[i] := ret[i] + as(<integer>, s[i * 4 + j]);
    end for;
  end for;
  ret;
end method string-to-double-integer-vector;

define method rol (a :: <double-integer>, count :: <integer>)
  => (ret :: <double-integer>)
  logior(ash(a, count), ash(a, - (32 - count)));
end method rol;
