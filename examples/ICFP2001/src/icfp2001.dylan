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


define macro attribute-slot-definer
  {define attribute-slot ?bit:expression ?:name end} =>
    {define inline method ?name(a :: <attribute>) => res :: <boolean>;
       logand(a.value, ?bit) == 1;
     end method ?name;

     define inline method "set-" ## ?name(a :: <attribute>)
      => newObj :: <attribute>;
	 make(<attribute>, value: logior(a.value, ?bit));
     end method "set-" ## ?name;

     define inline method "clear-" ## ?name(a :: <attribute>)
      => newObj :: <attribute>;
	 make(<attribute>, value: logand(a.value, lognot(?bit)));
     end method "clear-" ## ?name;}
end macro attribute-slot-definer;

define functional class <attribute> (<object>)
  slot value :: <integer> = 0,
    init-keyword: value:;
/*
  slot underline  :: limited(<integer>, min:0, max: 3) = 0;
  slot size       :: false-or(limited(<integer>, min:0, max: 9)) = #f;
  slot color      :: false-or(<color>) = #f;
*/
end class <attribute>;

define attribute-slot #x01 bold end;
define attribute-slot #x02 emphasis end;
define attribute-slot #x04 italic end;
define attribute-slot #x08 strong end;
define attribute-slot #x10 typewriter end;


define sealed domain make(singleton(<attribute>));
define sealed domain initialize(<attribute>);

define function dump(name :: <byte-string>, val :: <attribute>) => ()
  format-out("%=.bold = %=\n", name, val.bold);
end function dump;


define function test-attributes()
  let a = make(<attribute>, value: 8);
  dump("a", a);
  let b = make(<attribute>, value: 9);
  dump("b", b);
  let c = a.set-bold;
  dump("c", c);
  let d = c.clear-bold;
  dump("d", d);
end test-attributes;



define constant <color> = one-of(#"red", #"green", #"blue", 
                                 #"cyan", #"magenta", #"yellow",
                                 #"black", #"white");

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
    if (end-index == input.size)
      values(elem, end-index);
    else
      let (return-elem, return-consumed) = parse(input, start: end-index);
      values(pair(elem, return-elem), return-consumed);
    end;
  end;
end method parse;

define function bgh-parse(s :: <byte-string>)
  let v = make(<stretchy-vector>);
  for (c keyed-by i in s)
    //format-out("char %= is %=\n", i, c);
    
  end;
  v;
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

  test-attributes();

  block ()

    let original-input      = slurp-input(input-stream);
    let best-transformation = original-input;
    let parse-tree          = parse(original-input);

    format-out("%=\n", parse-tree);

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
