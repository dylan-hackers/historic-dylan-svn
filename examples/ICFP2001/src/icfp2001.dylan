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
  slot cost :: <integer> = 0;
end;

define constant !all-tags = make(<table>);

define sealed method initialize(t :: <tag>, #key name)
  t.open-tag := concatenate("<", name, ">");
  t.close-tag := concatenate("</", name, ">");
  t.cost := t.open-tag.size + t.close-tag.size;
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
      let s :: <byte-string> = as(<byte-string>, fragments);
      if (s.size > 0)
	add!(runs,
	     make(<attributed-string>, attributes: run-state, string: s));
      end;
      fragments.size := 0;
      non-space-char-in-run := #f;
      run-state := curr-state;
      check-timeout();
    end method save-run;
  
  while (p < s.size)
    let ch = s[p];
    case
      ch ~= '<' =>
	// normal text
	if (is-space?(ch))
	  unless (curr-state.typewriter)
	    ch := ' ';
	  end;
	  let run-space-state = run-state.space-context.value;
	  let space-state = curr-state.space-context.value;
	  let same-space-context = run-space-state == space-state;
	  if (~same-space-context)
	    save-run();
	  end;
	  unless (last-char-was-space &
		    same-space-context &
		    ~curr-state.typewriter)
	    add!(fragments, ch);
	  end;
	  last-char-was-space := #t;
	else
	  if (run-state.value ~== curr-state.value)
	    // if the preceding stuff was just spaces, then it
	    // may be possible to retroactively make it the same
	    // format as the current stuff
	    if (non-space-char-in-run)
	      save-run();
	    elseif (run-state.space-context.value ==
		      curr-state.space-context.value)
	      run-state := curr-state;
	    else
	      save-run();
	    end;
	  end;
	  add!(fragments, ch);
	  last-char-was-space := #f;
	  non-space-char-in-run := #t;
	end;
	p := p + 1;

      s[p + 1] ~= '/' =>
	// opening tag
	let (tag, new-p, new-state) = parse-tag(s, p + 1, curr-state);
	add!(tag-stack, tag);
	add!(state-stack, curr-state);
	curr-state := new-state;
	p := new-p;

      otherwise =>
	// closing tag
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
define string-concatenator <list> end;


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


define function dump-attributed-string(run :: <attributed-string>) => ();
  describe-attributes(run.attributes);
  debug("    %=\n", run.string);
end function dump-attributed-string;

define function dump-parse-tree(v :: <stretchy-object-vector>) => ();
  for (run :: <attributed-string> in v)
    dump-attributed-string(run);
  end;
end function dump-parse-tree;


define variable $end-time$ :: <integer> = 0;

define function timeout() => ();
  signal(make(<timeout>));
end timeout;

define inline method check-timeout() => ();
  if($end-time$ < get-universal-time())
    timeout();
  end if;
end method check-timeout;

define class <timeout> (<condition>)
end;

define function main(name, arguments)
  let start-time :: <integer> = get-universal-time();
  $end-time$ := start-time + string-to-integer(arguments[0]) - 30;

  let input-stream = *standard-input*;

//  debug("Reading input.\n");
  let original-input      = slurp-input(input-stream);
  let best-transformation = original-input;

  local
    method see-if-best(new-output)
//      debug("In see-if-best\n");

      let old = best-transformation.size;
      let new = new-output.size;

      if(new-output ~= #())
        debug("old size = %d, new size = %d, time passed: %d", old, new, get-universal-time() - start-time);

        if (new < old)
          debug("  - using new");
          best-transformation := new-output; //FIXME -- when finished testing
        end if;
        debug("\n");
      end if;
    end method see-if-best,

    method iterate-generate-optimized-output(parse-tree)
      let exhausted = #f;
      let i = 0;
      while(~exhausted)
	let (string, done) =
	  generate-optimized-output(parse-tree, run: i);
	string.concatenate-strings.see-if-best;
	exhausted := done;
	i := i + 1;
      end while;
    end method iterate-generate-optimized-output;

  block()
//    debug("Parsing input.\n");
    let parse-tree          = bgh-parse(original-input);
//    dump-parse-tree(parse-tree);

//    debug("Generating output.\n");
    generate-output(parse-tree).concatenate-strings.see-if-best;
//    debug("Generating optimized output.\n");
//    iterate-generate-optimized-output(parse-tree);
    optimize-output(parse-tree).concatenate-strings.see-if-best;

  exception (<timeout>)
//    debug("Out of time!\n");
  end;

  if(best-transformation.size > 0 &
       is-space?(best-transformation[best-transformation.size - 1]))
    best-transformation[best-transformation.size - 1] := '\n';
  end if;
  write(*standard-output*, best-transformation);
  force-output(*standard-output*);

  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
