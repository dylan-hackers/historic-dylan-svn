module: fragments
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/parser/fragments.dylan,v 1.2.1.1 1994/12/19 13:02:36 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


//// Program fragments.

define class <fragment> (<object>)
  slot fragment-start :: <piece>, init-keyword: start:;
  slot fragment-end :: <piece>,
    init-keyword: end:,
    init-function: compose(curry(make, <piece>, (token:)),
			   curry(make, <eof-token>));
end;

define method initialize (frag :: <fragment>, #key start, piece, pieces)
  if (piece)
    frag.fragment-start := piece;
    if (piece.piece-next)
      frag.fragment-end := piece.piece-next;
    else
      let eof = frag.fragment-end;
      piece.piece-next := eof;
      eof.piece-prev := piece;
    end;
  elseif (pieces)
    frag.fragment-start := frag.fragment-end;
    for (piece in pieces)
      postpend-piece!(frag, piece);
    end;
  elseif (~start)
    frag.fragment-start := frag.fragment-end;
  end;
end;

define method print-object (frag :: <fragment>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(frag, stream);
	     write(' ', stream);
	     write-address(frag, stream);
	     pprint-indent(#"block", 2, stream);
	     let head = frag.fragment-start;
	     let tail = frag.fragment-end;
	     unless (head == tail)
	       write(' ', stream);
	       pprint-newline(#"linear", stream);
	       print(head.piece-token, stream);
	       let tail-prev = tail.piece-prev;
	       unless (head == tail-prev)
		 write(' ', stream);
		 pprint-newline(#"linear", stream);
		 unless (head.piece-next == tail-prev)
		   write("... ", stream);
		   pprint-newline(#"linear", stream);
		 end;
		 print(tail-prev.piece-token, stream);
	       end;
	     end;
	   end,
     suffix: "}");
end;

define class <piece> (<source-location-mixin>)
  slot piece-prev :: union(<false>, <piece>),
    init-value: #f, init-keyword: prev:;
  slot piece-next :: union(<false>, <piece>),
    init-value: #f, init-keyword: next:;
  slot piece-token :: <token>,
    required-init-keyword: token:;
end;

define method print-object (piece :: <piece>, stream :: <stream>) => ();
  pprint-fields(piece, stream, token: piece.piece-token);
end;

define class <balanced-piece> (<piece>)
  slot piece-other :: <balanced-piece>,
    init-keyword: other:;
end;

define method prepend-piece! (piece :: <piece>, frag :: <fragment>)
  let start = frag.fragment-start;
  start.piece-prev := piece;
  piece.piece-next := start;
  frag.fragment-start := piece;
  frag;
end;

define method postpend-piece! (frag :: <fragment>, piece :: <piece>)
  let stop = frag.fragment-end;
  let tail = stop.piece-prev;
  stop.piece-prev := piece;
  piece.piece-next := stop;
  piece.piece-prev := tail;
  if (tail)
    tail.piece-next := piece;
  else
    frag.fragment-start := piece;
  end;
  frag;
end;

define method append-fragments! (frag1 :: <fragment>, frag2 :: <fragment>)
  if (frag1.fragment-start == frag1.fragment-end)
    frag2;
  elseif (frag2.fragment-start == frag2.fragment-end)
    frag1;
  else
    let frag1-tail = frag1.fragment-end.piece-prev;
    let frag2-start = frag2.fragment-start;
    frag1-tail.piece-next := frag2-start;
    frag2-start.piece-prev := frag1-tail;
    frag1.fragment-end := frag2.fragment-end;
    frag1;
  end;
end;


// Fragment source locations.

define class <fragment-source-location> (<fragment>, <source-location>)
  slot macro, required-init-keyword: macro:;
  slot origin :: <source-location>, required-init-keyword: origin:;
end;

define method join-source-locations
    (start :: <fragment-source-location>, stop :: <fragment-source-location>)
    => res :: union(<false>, <fragment-source-location>);
  if (start.macro == stop.macro & start.fragment-end == stop.fragment-start)
    make(<fragment-source-location>,
	 start: start.fragment-start,
	 end: stop.fragment-end,
	 macro: start.macro,
	 origin: compound-source-location(start.origin, stop.origin));
  end;
end;

define method simplify-source-location (loc :: <fragment-source-location>)
    => res :: <fragment-source-location>;
  make(<fragment-source-location>,
       start: loc.fragment-start,
       end: loc.fragment-end,
       macro: loc.macro,
       origin: simplify-source-location(loc.origin));
end;


// Fragment tokenizer.

define class <fragment-tokenizer> (<tokenizer>)
  slot macro, init-value: #f, init-keyword: macro:;
  slot current-piece :: <piece>;
  slot end-piece :: <piece>;
end class;

define method initialize (tokenizer :: <fragment-tokenizer>, #key fragment)
  tokenizer.current-piece := fragment.fragment-start;
  tokenizer.end-piece := fragment.fragment-end;
end method;

define method print-object (tokenizer :: <fragment-tokenizer>,
			    stream :: <stream>)
    => ();
  pprint-fields(tokenizer, stream, current: tokenizer.current-piece);
end method;

define method get-token (tokenizer :: <fragment-tokenizer>)
    => (token :: <token>, source-location :: <source-location>);
  let cur = tokenizer.current-piece;
  let macro = tokenizer.macro;
  if (cur == tokenizer.end-piece)
    values(make(<eof-token>), make(<unknown-source-location>));
  else
    let next = cur.piece-next;
    tokenizer.current-piece := next;
    values(cur.piece-token,
	   if (macro)
	     make(<fragment-source-location>,
		  start: cur, end: next, macro: macro,
		  origin: cur.source-location);
	   else
	     make(<unknown-source-location>);
	   end);
  end;
end method;

define method unget-token (tokenizer :: <fragment-tokenizer>, token :: <token>)
    => ();
  tokenizer.current-piece := tokenizer.current-piece.piece-prev;
end method;

define method unget-token (tokenizer :: <fragment-tokenizer>,
			   token :: <eof-token>)
    => ();
end method;


