module: validate
synopsis: simple validator to compare two SML/NG files
author: Bruce Hoult
copyright: 

define macro string-concatenator-definer
  {define string-concatenator ?class:name end} =>
    {define method concatenate-strings(v :: ?class)
      => result :: <byte-string>;
       let length = for (total = 0 then total + str.size,
			 str :: <byte-string> in v)
		    finally total;
		    end for;
       
       let result :: <byte-string> = make(<byte-string>, size: length);
       let (init-state, limit, next-state, done?, current-key, current-element,
	    current-element-setter) = forward-iteration-protocol(result);
       
       for (result-state = init-state
	      then for (char in str,
			state = result-state then next-state(result, state))
		     current-element(result, state) := char;
		   finally state;
		   end for,
	    str :: <byte-string> in v)
       end for;
       result;
     end method concatenate-strings;}
end macro;

define string-concatenator <stretchy-object-vector> end;

define method slurp-input(stream :: <buffered-stream>)
 => contents :: <byte-string>;
  let v = make(<stretchy-vector>);
  block ()
    for (buf :: false-or(<buffer>) = get-input-buffer(stream)
	   then next-input-buffer(stream),
	 while: buf)
      let s = buffer-subsequence(buf, <byte-string>,
				 buf.buffer-next,
				 buf.buffer-end);
      add!(v, s);
      buf.buffer-next := buf.buffer-end;
    end for;
  cleanup
    release-input-buffer(stream);
  end block;
  v.concatenate-strings;
end method slurp-input;

define constant <color> = one-of(#"red", #"green", #"blue", 
                                 #"cyan", #"magenta", #"yellow",
                                 #"black", #"white");

define constant <font-size> = limited(<integer>, min:0, max: 9);

define constant <maybe-color> = false-or(<color>);
define constant <maybe-font-size> = false-or(<font-size>);

define class <attributes> (<object>)
  slot b :: <boolean> = #f,
    init-keyword: b:;
  slot em :: <boolean> = #f,
    init-keyword: em:;
  slot i :: <boolean> = #f,
    init-keyword: i:;
  slot s :: <boolean> = #f,
    init-keyword: s:;
  slot tt :: <boolean> = #f,
    init-keyword: tt:;
  slot u :: <integer> = 0,
    init-keyword: u:;
  slot font-size :: <maybe-font-size> = #f,
    init-keyword: size:;
  slot color :: <maybe-color> = #f,
    init-keyword: color:;
end class <attributes>;

define sealed domain initialize(<attributes>);

define inline method clone(a :: <attributes>)
 => new :: <attributes>;
  make(<attributes>,
       b: a.b,
       em: a.em,
       i: a.i,
       s: a.s,
       tt: a.tt,
       u: a.u,
       size: a.font-size,
       color: a.color);
end clone;

define method \=(l :: <attributes>, r :: <attributes>) => res :: <boolean>;
  l.b == r.b &
  l.em == r.em &
  l.i == r.i &
  l.s == r.s &
  l.tt == r.tt &
  l.u == r.u &
  l.font-size == r.font-size &
  l.color == r.color;
end \=;

define sealed domain \=(<attributes>, <attributes>);


define method dump(a :: <attributes>, stream :: <stream>)
 => ();
  let first = #t;
  local
    method comma()
      if (first)
	first := #f;
      else
	format(stream, ",");
      end;
    end,
    method pp(cond :: <boolean>, desc :: <byte-string>)
      if (cond)
	comma();
	format(stream, desc);
      end;
    end;

  format(stream, "{");
  pp(a.b, "B");
  pp(a.em, "EM");
  pp(a.i, "I");
  pp(a.s, "S");
  pp(a.tt, "TT");
  comma();
  format(stream, "%=,%=,%=}", a.u, a.font-size, a.color);
end dump;


define class <doc> (<object>)
  slot text :: <byte-string>,
    required-init-keyword: text:;
  slot pos :: <integer> = 0;
  slot attr :: <attributes> = make(<attributes>);
  slot tag-stack :: <stretchy-object-vector> = make(<stretchy-vector>);
  slot attr-stack :: <stretchy-object-vector> = make(<stretchy-vector>);
  slot last-char-was-space :: <boolean> = #f;
  slot last-space-context :: <attributes> =  make(<attributes>);
end class <doc>;

define sealed domain initialize(<doc>);

define function eof?(d :: <doc>) => eof :: <boolean>;
  pos >= text.size;
end eof?;


define class <char> (<object>)
  slot ch :: <character>,
    required-init-keyword: ch:;
  slot attr :: <attributes>,
    required-init-keyword: attr:;
end class <char>;

define sealed domain initialize(<char>);

define constant <maybe-char> = false-or(<char>);

define method process-tag(d :: <doc>, tag :: <byte-string>, closing :: <boolean>)
 => ();
  if (closing)
    let top-tag = d.tag-stack.last;
    let top-attr = d.attr-stack.last;
    if (tag ~= top-tag)
      error("tag mismatch");
    end;
    d.attr := top-attr;
    d.tag-stack.size := d.tag-stack.size - 1;
    d.attr-stack.size := d.attr-stack.size - 1;
  else
    add!(d.tag-stack, tag);
    add!(d.attr-stack, d.attr);

    let a = d.attr.clone;
    select (tag by \=)
      "B"  => a.b := #t;
      "EM" => if (~a.s) a.em := ~a.em end;
      "I"  => a.i := #t;
      "PL" =>
	a.b := a.s := a.em := a.i := a.tt := #f;
	a.u := 0;
      "S"  => a.s := #t; a.em := #f;
      "TT" => a.tt := #t;
      "U"  => if (a.u < 3) a.u := a.u + 1 end;
	
      "0"  => a.font-size := 0;
      "1"  => a.font-size := 1;
      "2"  => a.font-size := 2;
      "3"  => a.font-size := 3;
      "4"  => a.font-size := 4;
      "5"  => a.font-size := 5;
      "6"  => a.font-size := 6;
      "7"  => a.font-size := 7;
      "8"  => a.font-size := 8;
      "9"  => a.font-size := 9;
	
      "r" => a.color := #"red";
      "g" => a.color := #"green";
      "b" => a.color := #"blue";
      "c" => a.color := #"cyan";
      "m" => a.color := #"magenta";
      "y" => a.color := #"yellow";
      "k" => a.color := #"black";
      "w" => a.color := #"white";
    end select;
    d.attr := a;
  end;
end;

define constant <space> =
  one-of(as(<character>, #x20), as(<character>, #x9), as(<character>, #xd),
         as(<character>, #x0a));

define inline function is-space?(c)
  instance?(c, <space>);
end function is-space?;

define method nextOutput(d :: <doc>) => ch :: <maybe-char>;
  let s = d.text;
  let p = d.pos;

  let result =
    block (return)
      while (p < s.size)
	let ch = s[p];
	if (ch == '<')
	  p := p + 1;
	  let closing =
	    if (s[p] == '/')
	      p := p + 1;
	      #t;
	    else
	      #f;
	    end;
	  let q = p + 1;
	  while (s[q] ~= '>')
	    q := q + 1;
	  end;
	  let tag = copy-sequence(s, start: p, end: q);
	  process-tag(d, tag, closing);
	  p := q + 1;
	elseif (ch.is-space?)
	  let curr = d.attr;
	  if (curr.tt)
	    let ch = make(<char>, ch: ' ', attr: curr);
	    p := p + 1;
	    d.last-char-was-space := #t;
	    d.last-space-context := curr;
	    return(ch);
	  elseif (d.last-char-was-space &
		    same-space-context?(curr, d.last-space-context))
	    // do nothing
	    p := p + 1;
	  else
	    let ch = make(<char>, ch: ' ', attr: curr);
	    p := p + 1;
	    d.last-char-was-space := #t;
	    d.last-space-context := curr;
	    return(ch);
	  end;	    
	else
	  let ch = make(<char>, ch: ch, attr: d.attr);
	  d.last-char-was-space := #f;
	  p := p + 1;
	  return(ch);
	end;
      end;
      #f;
    end;

  d.pos := p;
  result;
end nextOutput;


define method same-space-context?(l :: <attributes>, r :: <attributes>)
 => res :: <boolean>;
  let l-color = if (l.u == 0) #"white" else l.color end;
  let r-color = if (r.u == 0) #"white" else r.color end;

  l.tt == r.tt &
  l.u == r.u &
  l.font-size == r.font-size &
  l-color == r-color;
end same-space-context?;


define function getDoc(name) => doc :: <doc>;
  let file = make(<file-stream>, direction: #"input", locator: name);
  let text = slurp-input(file);
  make(<doc>, text: text);
end getDoc;

define function quit(s :: <byte-string>) => ();
  force-output(*standard-output*);
  error(s);
end quit;


define function main(name, arguments)
  if (arguments.size ~= 2)
    format-out("Need two filename args\n");
    exit-application(1);
  end;

  let doc1 = getDoc(arguments[0]);
  let doc2 = getDoc(arguments[1]);

  block (exit)
    while (#t)
      let ch1 = doc1.nextOutput;
      let ch2 = doc2.nextOutput;
      
      if (~ch1 & ~ch2)
	exit();
      end;
      if (~ch1)
	quit("file 1 finished too soon");
      end;
      if (~ch2)
	quit("file 2 finished too soon");
      end;

      let OK =
	block (OK)
	  if (ch1.ch ~= ch2.ch) OK(#f) end;
	  if (ch1.ch == ' ')
	    OK(same-space-context?(ch1.attr, ch2.attr));
	  end;
	  OK(ch1.attr = ch2.attr);
	end;

      if (~OK)
	format-out("Characters differ at %d and %d\n", doc1.pos, doc2.pos);
	format-out("%=:  ", ch1.ch);
	dump(ch1.attr, *standard-output*);
	format-out("\n");
	format-out("%=:  ", ch2.ch);
	dump(ch2.attr, *standard-output*);
	format-out("\n\n");
	exit-application(1);
      end;

    end;
  end;

  format-out("Files match\n");
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
