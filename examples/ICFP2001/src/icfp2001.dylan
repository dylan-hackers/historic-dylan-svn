module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <document> (<object>)
  slot characters;
end class <document>;

define class <char> (<object>)
  slot character;
  slot attribute;
end class <char>;






define function debug(#rest args)
  apply(format, *standard-error*, args);
end function debug;

define constant <space> =
  one-of(as(<character>, #x20), as(<character>, #x9), as(<character>, #xd),
         as(<character>, #x0a));

define function is-space?(c)
  instance?(c, <space>);
end function is-space?;

define function is-textchar?(c)
  c ~= '<' & c ~= '>';
end function is-textchar?;

define method parse-textstring(input, #key start = 0)
 => (string, end-index);
  let end-index =
    block(return)
      for(index from start below input.size)
        if(~ is-textchar?(input[index]))
          return(index);
        end if;
      end for;
      input.size;
    end block;
  values(copy-sequence(input, start: start, end: end-index), end-index);
end method parse-textstring;

define method parse-elem(input, #key start = 0)
 => (string, end-index);
  while(input[start] ~= '>')
    start := start + 1;
  end while;
  let (elem, end-index) = parse(input, start: start + 1);
  while(input[end-index] ~= '>')
    end-index := end-index + 1;
  end while;
  values(elem, end-index + 1);
end method parse-elem;

define method parse(input, #key start = 0)
  block(return)
    if(start >= input.size)
      return(#(), start);
    end;
    let (elem, end-index) =   if(input[start] = '<')
                                if(input[start + 1] ~= '/')
                                  parse-elem(input, start: start);
                                else
                                  return(#(), start);
                                end;
                              else
                                parse-textstring(input, start: start);
                              end;
    let (return-elem, return-consumed) = parse(input, start: end-index);
    values(pair(elem, return-elem), return-consumed);
  end;
end method parse;


define class <tag> (<object>)
  slot name :: <byte-string>,
    required-init-keyword: name:;
  slot open-tag :: <byte-string> = "";
  slot close-tag :: <byte-string> = "";
end;

define sealed method initialize(t :: <tag>, #key name)
  t.open-tag := concatenate("<", name, ">");
  t.close-tag := concatenate("</", name, ">");
end;

define sealed domain make(singleton(<tag>));

define constant tag-BB = make(<tag>, name: "B");
define constant tag-EM = make(<tag>, name: "EM");
define constant tag-I  = make(<tag>, name: "I");
define constant tag-PL = make(<tag>, name: "PL");
define constant tag-S  = make(<tag>, name: "S");
define constant tag-TT = make(<tag>, name: "TT");
define constant tag-U  = make(<tag>, name: "U");
define constant tag-0  = make(<tag>, name: "0");
define constant tag-1  = make(<tag>, name: "1");
define constant tag-2  = make(<tag>, name: "2");
define constant tag-3  = make(<tag>, name: "3");
define constant tag-4  = make(<tag>, name: "4");
define constant tag-5  = make(<tag>, name: "5");
define constant tag-6  = make(<tag>, name: "6");
define constant tag-7  = make(<tag>, name: "7");
define constant tag-8  = make(<tag>, name: "8");
define constant tag-9  = make(<tag>, name: "9");
define constant tag-r  = make(<tag>, name: "r");
define constant tag-g  = make(<tag>, name: "g");
define constant tag-b  = make(<tag>, name: "b");
define constant tag-c  = make(<tag>, name: "c");
define constant tag-m  = make(<tag>, name: "m");
define constant tag-y  = make(<tag>, name: "y");
define constant tag-k  = make(<tag>, name: "k");
define constant tag-w  = make(<tag>, name: "w");

define function parse-tag
    (s :: <byte-string>, p :: <integer>, state :: <attribute>)
 => (t :: <tag>, new-p :: <integer>, new-state :: <attribute>);
  let (t :: <tag>, new-state :: <attribute>) =
    select (s[p])
      'B' => values(tag-BB, state.set-bold);
      'E' => values(tag-EM, state.set-emphasis);
      'I' => values(tag-I,  state.set-italic);
      'P' => values(tag-PL, state.set-plain);
      'S' => values(tag-S,  state.set-strong);
      'T' => values(tag-TT, state.set-typewriter);
      'U' => values(tag-U,  state.set-underline);
      '0' => values(tag-0,  set-font-size(state, 0));
      '1' => values(tag-1,  set-font-size(state, 1));
      '2' => values(tag-2,  set-font-size(state, 2));
      '3' => values(tag-3,  set-font-size(state, 3));
      '4' => values(tag-4,  set-font-size(state, 4));
      '5' => values(tag-5,  set-font-size(state, 5));
      '6' => values(tag-6,  set-font-size(state, 6));
      '7' => values(tag-7,  set-font-size(state, 7));
      '8' => values(tag-8,  set-font-size(state, 8));
      '9' => values(tag-9,  set-font-size(state, 9));
      'r' => values(tag-r,  set-color(state, #"red"));
      'g' => values(tag-g,  set-color(state, #"green"));
      'b' => values(tag-b,  set-color(state, #"blue"));
      'c' => values(tag-c,  set-color(state, #"cyan"));
      'm' => values(tag-m,  set-color(state, #"magenta"));
      'y' => values(tag-y,  set-color(state, #"yellow"));
      'k' => values(tag-k,  set-color(state, #"black"));
      'w' => values(tag-w,  set-color(state, #"white"));
    end;
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

define function bgh-parse(s :: <byte-string>)
  let runs = make(<stretchy-vector>);
  let fragments = make(<stretchy-vector>);
  let first-char = 0;

  let state-stack = make(<stretchy-vector>);
  let tag-stack = make(<stretchy-vector>);
  let current-attributes = make(<attribute>);
  let p = 0;
  while (p < s.size)
    //format-out("char %= is %=\n", i, c);
    case
      s[p] ~= '<' =>
	p := p + 1;

      s[p + 1] ~= '/' =>
	add!(fragments, copy-sequence(s, start: first-char, end: p));
	let (tag, new-p, new-state) = parse-tag(s, p + 1, current-attributes);
	add!(tag-stack, tag);
	add!(state-stack, current-attributes);

	if (new-state.value ~= current-attributes.value)
	  add!(runs, pair(current-attributes, apply(concatenate, fragments)));
	  fragments.size := 0;
	  current-attributes := new-state;
	end;

	p := new-p;
	first-char := p;

      otherwise =>
	add!(fragments, copy-sequence(s, start: first-char, end: p));
	let (tag, new-p) = parse-tag(s, p + 2, current-attributes);

	if (tag-stack.last ~== tag)
	  error("mis-balanced tags");
	end;
	let new-state = state-stack.last;
	if (new-state.value ~= current-attributes.value)
	  add!(runs, pair(current-attributes, apply(concatenate, fragments)));
	  fragments.size := 0;
	  current-attributes := new-state;
	end;

	tag-stack.size := tag-stack.size - 1;
	state-stack.size := state-stack.size - 1;
	p := new-p;
	first-char := p;

    end case;
  end;
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
  apply(concatenate, v);
end method slurp-input;

define function main(name, arguments)
  let input-stream = *standard-input*;

  block ()

    let original-input      = slurp-input(input-stream);
    let best-transformation = original-input;
    let parse-tree          = bgh-parse(original-input);

    //format-out("%=\n", parse-tree);

    for (e in parse-tree)
      let a :: <attribute> = e.head;
      let s :: <byte-string> = e.tail;
      describe-attributes(a, *standard-output*);
      format-out("      '%s'\n", s);
    end;

    while(time-is-not-up?())
      optimize();
    end while;

    write(*standard-output*, best-transformation);
    force-output(*standard-output*);

    debug("Run successful. Original size: %=. Size after optimization: %=.\n", original-input.size, best-transformation.size);

  /*
<bruce> - readin the original text and save it in case we can't do any better
<bruce> - convert to fully-annotated runs of characters
<bruce> - do a one-pass simple-minded markup and keep it if it's better than the original in 
                                                        case we can't do any better
<andreas> - generate permutations of the markup until time runs out.
<bruce> - step through the text.  At each change you have to consider whether to open a new tag
           or close the most recent old one.  If multiple attributes change then you have 
           multiuple choices for which to open first
<bruce> all could be done in parallel, keeping track of the comparitive resulting sizes
<bruce> (this is the "big risk, big win" approach...)
<andreas> Some sort of pruning will be required.
                                 */
                                 

  exception (e :: <condition>)

    format-out("Sorry, Dylan Hacker has detected an error\n");
    format-out("\n=========================================\n");
    report-condition(e, *standard-output*);
    format-out("\n=========================================\n");
    format-out("\nProgram terminating with error status\n");

    exit-application(1);
  end;

  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
