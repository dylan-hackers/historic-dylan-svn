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

define class <attribute> (<object>)  // make that a flyweight pattern one day
  slot bold       :: <boolean> = #f;
  slot emphasis   :: <boolean> = #f;
  slot italic     :: <boolean> = #f;
  slot strong     :: <boolean> = #f;
  slot typewriter :: <boolean> = #f;
  slot underline  :: limited(<integer>, min:0, max: 3) = 0;
  slot size       :: false-or(limited(<integer>, min:0, max: 9)) = #f;
  slot color      :: false-or(<color>) = #f;
end class <attribute>;

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
 => (string, bytes-consumed);
  let bytes-consumed =
    block(return)
      for(i in input, index from start)
        if(~ is-textchar?(i))
          return(index);
        end if;
      end for;
    end block;
  values(copy-sequence(input, end: bytes-consumed), bytes-consumed);
end method parse-textstring;

define method parse-elem(input, #key start = 0)
 => (string, bytes-consumed);
  while(input[start] ~= '>')
    start := start + 1;
  end while;
  let (elem, bytes-consumed) = parse(input, start: start + 1);
  while(input[bytes-consumed] ~= '>')
    bytes-consumed := bytes-consumed + 1;
  end while;
  values(elem, bytes-consumed);
end method parse-elem;

define method parse(input, #key start = 0)

  let (elem, bytes-consumed) =   if(input[start] = '<')
                                   if(input[start + 1] ~= '/')
                                     parse-elem(input, start: start);
                                   else
                                     values(#(), start);
                                   end;
                                 else
                                   parse-textstring(input, start: start);
                                 end;

  if (bytes-consumed == input.size)
    values(elem, bytes-consumed);
  else
    let (return-elem, return-consumed) = parse(input, start: bytes-consumed);
    values(pair(elem, return-elem), return-consumed);
  end;
end method parse;

define function bgh-parse(s :: <byte-string>)
  let v = make(<stretchy-vector>);
  for (c keyed-by i in s)
    format-out("char %= is %=\n", i, c);
    
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

  block ()

    let original-input      = slurp-input(input-stream);
    let best-transformation = original-input;
    let parse-tree          = bgh-parse(original-input);

    write(*standard-output*, parse-tree);

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
