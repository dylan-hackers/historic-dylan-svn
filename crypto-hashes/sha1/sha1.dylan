module: sha1
author: Hannes Mehnert

// XXX: 32-bit positive integer needs to be a <double-integer> in dylan :(
define method f
    (t :: <integer>,
     B :: <double-integer>,
     C :: <double-integer>,
     D :: <double-integer>)
  => (E :: <double-integer>)
  case
    (0 <= t & t <= 19) => logior(logand(B, C), logand(lognot(B), D));
    (20 <= t & t <= 39) => logxor(B, logxor(C, D));
    (40 <= t & t <= 59) => logior(logand(B, C), 
                                  logior(logand(B, D),logand(C, D)));
    (60 <= t & t <= 79) => logxor(B, logxor(C, D));
  end case;
end method f;

define method K (t :: <integer>)
 => (ret :: <double-integer>)
  case
    (0 <= t & t <= 19) => #x5A827999;
    (20 <= t & t <= 39) => #x6ED9EBA1;
    (40 <= t & t <= 59) => #x8F1BBCDC;
    (60 <= t & t <= 79) => #xCA62C1D6;
  end case;
end method K;

define class <sha1-state> (<object>)
  slot H0 :: <double-integer>, init-value: #x67452301;
  slot H1 :: <double-integer>, init-value: #xEFCDAB89;
  slot H2 :: <double-integer>, init-value: #x98BADCFE; 
  slot H3 :: <double-integer>, init-value: #x10325476;
  slot H4 :: <double-integer>, init-value: #xC3D2E1F0;
end;

define constant $block-size :: <integer> = 64;

define method update-state 
    (state :: <sha1-state>, W :: <simple-object-vector>)
 => (state :: <sha1-state>)
  for (t from 16 to 79)
    W[t] := rol(logxor(logxor(W[t - 3], W[t - 8]),
                       logxor(W[t - 14], W[t - 16])), 1);
  end for;
  
  let A = state.H0;
  let B = state.H1;
  let C = state.H2;
  let D = state.H3;
  let E = state.H4;
  
  for (t from 0 to 79)
    let temp = rol(A + f(t, B, C, D) + E + W[t] + K(t), 5);
    E := D; D := C; C := ash(B, 30); B := A; A := temp;
  end for;
  
  state.H0 := state.H0 + A;
  state.H1 := state.H1 + B;
  state.H2 := state.H2 + C;
  state.H3 := state.H3 + D;
  state.H4 := state.H4 + E;

  state;
end method update-state;

define method finalize-state
    (state :: <sha1-state>,
     s :: <byte-vector>,
     length :: <integer>)
 => (state :: <sha1-state>)

  // pad message with one 1 and n 0  
  s := concatenate(s, #x80);
  let block-int-vector = string-to-double-integer-vector(s);

  let W :: <simple-object-vector> = make(<simple-object-vector>,
                                         size: 80, fill: #x00);

  for(t from 0 below size(block-int-vector))
    W[t] := block-int-vector[t];
  end for;

  if (size(s) > 55) // need to add another block
    state := update-state(state, W);
    W := make(<simple-object-vector>, size: 80, fill: #x00);
  end if;

  //append size
  // XXX: length should be 64 bit, not 32 bit!
  W[$block-size - 1] = length * 8;
  update-state(state, W);
end method finalize-state;

define method sha1 (s :: <byte-vector>)
  let last-block-size = modulo(size(s), $block-size);
  format-out("lbs: %d\n", last-block-size);
  let block-int-vector = string-to-double-integer-vector(s);
  let block-count = size(block-int-vector);

  let state = make(<sha1-state>);
  let W :: <simple-object-vector> = make(<simple-object-vector>, size: 80);

  for (i from 0 below block-count)
    for (t from 0 below 16)
      W[t] := block-int-vector[i * 16 + t];
    end for;
    state := update-state(state, W);
  end for;

  let W = make(<simple-object-vector>, size: 80);
  for (t from 0 to last-block-size)
    W[t] := block-int-vector[block-count * 16 + t];
  end for;

  // XXX: size returns an integer (32 bit), sha1 must use a 64-bit size!
  let block-start :: <integer> = block-count * $block-size;
  state := finalize-state(state, 
                          copy-sequence(s,
                                        start: block-start,
                                        end: block-start + last-block-size),
                          size(s));

  //return H0 H1 H2 H3 H4
  values(state.H0, state.H1, state.H2, state.H3, state.H4);
end method sha1;

format-out("%d %d %d %d %d\n", sha1(as(<byte-vector>, format-to-string("abc"))));