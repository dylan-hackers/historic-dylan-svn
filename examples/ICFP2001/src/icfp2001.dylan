module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Chris Double, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define function debug(#rest args)
  apply(format, *standard-error*, args);
  force-output(*standard-error*);
end function debug;

define constant <space> =
  one-of(as(<character>, #x20), as(<character>, #x9), as(<character>, #xd),
         as(<character>, #x0a));

define inline function is-space?(c)
  instance?(c, <space>);
end function is-space?;

define class <tag> (<object>)
  slot name :: <byte-string>,
    required-init-keyword: name:;
  slot open-tag :: <byte-string> = "";
  slot close-tag :: <byte-string> = "";
  slot operation, required-init-keyword: op:;
end;

define constant !all-tags = make(<table>);

define sealed method initialize(t :: <tag>, #key name)
  t.open-tag := concatenate("<", name, ">");
  t.close-tag := concatenate("</", name, ">");
  !all-tags[name[0]] := t;
end;

define sealed domain make(singleton(<tag>));

define method apply-op(attribute :: <attribute>, tag :: <tag>)
 => (<attribute>)
  tag.operation(attribute);
end method apply-op;
       

define constant tag-BB = make(<tag>, name: "B",  op: set-bold);
define constant tag-EM = make(<tag>, name: "EM", op: set-emphasis);
define constant tag-I  = make(<tag>, name: "I",  op: set-italic);
define constant tag-PL = make(<tag>, name: "PL", op: set-plain);
define constant tag-S  = make(<tag>, name: "S",  op: set-strong);
define constant tag-TT = make(<tag>, name: "TT", op: set-typewriter);
define constant tag-U  = make(<tag>, name: "U",  op: set-underline);
define constant tag-0  = make(<tag>, name: "0",  op: rcurry(set-font-size, 0));
define constant tag-1  = make(<tag>, name: "1",  op: rcurry(set-font-size, 1));
define constant tag-2  = make(<tag>, name: "2",  op: rcurry(set-font-size, 2));
define constant tag-3  = make(<tag>, name: "3",  op: rcurry(set-font-size, 3));
define constant tag-4  = make(<tag>, name: "4",  op: rcurry(set-font-size, 4));
define constant tag-5  = make(<tag>, name: "5",  op: rcurry(set-font-size, 5));
define constant tag-6  = make(<tag>, name: "6",  op: rcurry(set-font-size, 6));
define constant tag-7  = make(<tag>, name: "7",  op: rcurry(set-font-size, 7));
define constant tag-8  = make(<tag>, name: "8",  op: rcurry(set-font-size, 8));
define constant tag-9  = make(<tag>, name: "9",  op: rcurry(set-font-size, 9));
define constant tag-r  = make(<tag>, name: "r",  op: rcurry(set-color, #"red"));
define constant tag-g  = make(<tag>, name: "g",  op: rcurry(set-color, #"green"));
define constant tag-b  = make(<tag>, name: "b",  op: rcurry(set-color, #"blue"));
define constant tag-c  = make(<tag>, name: "c",  op: rcurry(set-color, #"cyan"));
define constant tag-m  = make(<tag>, name: "m",  op: rcurry(set-color, #"magenta"));
define constant tag-y  = make(<tag>, name: "y",  op: rcurry(set-color, #"yellow"));
define constant tag-k  = make(<tag>, name: "k",  op: rcurry(set-color, #"black"));
define constant tag-w  = make(<tag>, name: "w",  op: rcurry(set-color, #"white"));

define function parse-tag
    (s :: <byte-string>, p :: <integer>, state :: <attribute>)
 => (t :: <tag>, new-p :: <integer>, new-state :: <attribute>);
  let t :: <tag> = !all-tags[s[p]];
  let new-state :: <attribute> = apply-op(state, t);
  for (i from 1 below t.name.size)
    if (t.name[i] ~= s[p + i])
      error("unknown tag");
    end;
  end;
  if (s[p + t.name.size] ~= '>')
    error("close tag expected");
  end;
  values(t, p + t.name.size + 1, new-state);
end;

define sealed class <attributed-string> (<object>)
  slot string :: <byte-string>,
    required-init-keyword: string:;
  slot attributes :: <attribute>,
    required-init-keyword: attributes:;
end;

define sealed domain make(singleton(<attributed-string>));
define sealed domain initialize(<attributed-string>);


define function bgh-parse(s :: <byte-string>)
  let runs = make(<stretchy-vector>);
  let fragments = make(<stretchy-vector>);

  let state-stack = make(<stretchy-vector>);
  let tag-stack = make(<stretchy-vector>);

  let curr-state = make(<attribute>);
  let run-state = make(<attribute>);
  let last-char-was-space = #f;
  let non-space-char-in-run = #f;

  let p = 0;

  local
    method save-run()
      if (run-state.value ~== curr-state.value)
        let s :: <byte-string> = as(<byte-string>, fragments);
        if (s.size > 0)
          add!(runs,
               make(<attributed-string>, attributes: run-state, string: s));
        end;
        fragments.size := 0;
        non-space-char-in-run := #f;
        run-state := curr-state;
        check-timeout();
      end;
    end method save-run;
  
  while (p < s.size)
    let ch = s[p];
    case
      ch ~= '<' =>

	if (is-space?(ch))
	  if (~curr-state.typewriter)
	    ch := ' ';
	  end;
	  let run-space-state = run-state.space-context.value;
	  let space-state = curr-state.space-context.value;
	  let same-format = run-space-state == space-state;
	  if (~same-format)
	    save-run();
	  end;
	  unless (last-char-was-space & same-format)
	    add!(fragments, ch);
	  end;
	  last-char-was-space := #t;
	else
	  save-run();
	  add!(fragments, ch);
	  last-char-was-space := #f;
	  non-space-char-in-run := #t;
	end;
	p := p + 1;

      s[p + 1] ~= '/' =>
	let (tag, new-p, new-state) = parse-tag(s, p + 1, curr-state);
	add!(tag-stack, tag);
	add!(state-stack, curr-state);
	curr-state := new-state;
	p := new-p;

      otherwise =>
	let (tag, new-p) = parse-tag(s, p + 2, curr-state);

	if (tag-stack.last ~== tag)
	  error("mis-balanced tags");
	end;
	let new-state = state-stack.last;
	tag-stack.size := tag-stack.size - 1;
	state-stack.size := state-stack.size - 1;
	curr-state := new-state;
	p := new-p;

    end case;
  end;
  curr-state := make(<attribute>, value: -1);
  non-space-char-in-run := #t;
  save-run();

  runs;
end function bgh-parse;

define function time-is-not-up?()
  #f;
end function time-is-not-up?;

define method optimize()
end method optimize;

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
//  apply(concatenate, v);
  reduce1(concatenate, v);
end method slurp-input;

define variable check-timeout = identity;

define class <timeout> (<condition>)
end;

define function main(name, arguments)
  let start-time = get-universal-time();
  let end-time   = start-time + string-to-integer(arguments[0]);

  check-timeout := method()
                       if(end-time - get-universal-time() < 10)
                         signal(make(<timeout>));
                       end if;
                   end method;

  let input-stream = *standard-input*;

  let original-input      = slurp-input(input-stream);
  let best-transformation = original-input;

  block()
    let parse-tree          = bgh-parse(original-input);

    let optimized-output = reduce1(concatenate, generate-output(parse-tree));

    if (optimized-output.size < original-input.size)
      best-transformation := optimized-output;
    end if;
  exception (<timeout>)
  end;

  if(is-space?(best-transformation[best-transformation.size - 1]))
    best-transformation :=
      copy-sequence(best-transformation, end: best-transformation.size - 1);
  end if;
  write(*standard-output*, best-transformation);
  write(*standard-output*, "\n");
  force-output(*standard-output*);

  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
