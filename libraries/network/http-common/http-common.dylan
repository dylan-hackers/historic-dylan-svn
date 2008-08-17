Module: http-common
Synopsis: Code shared by HTTP client and server.


// RFC 2616, 2.2
define constant $token-character-map
  = begin
      let vec = make(<vector>, size: 128, fill: #t);
      let separator-chars = "()<>@,;:\\\"/[]?={} \t";
      for (char in separator-chars)
        vec[as(<integer>, char)] := #f;
      end;
      // US ASCII control characters...
      for (code from 0 to 32)
        vec[code] := #f;
      end;
      vec[127] := #f;   // DEL
      vec
    end;

define inline function token-char?
    (char :: <byte-character>) => (token-char? :: <boolean>)
  let code :: <integer> = as(<integer>, char);
  code <= 127 & $token-character-map[code]
end;

define inline function non-token-char?
    (char :: <byte-character>) => (non-token-char? :: <boolean>)
  ~token-char?(char)
end;

define function token-end-position (buf :: <byte-string>,
                                    bpos :: <integer>,
                                    epos :: <integer>)
  char-position-if(non-token-char?, buf, bpos, epos)
end;


