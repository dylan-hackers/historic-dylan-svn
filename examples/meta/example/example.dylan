library: example
module: example

define function digit?(x :: <character>) => (b :: <boolean>)
  member?(x, "0123456789");
end function digit?;

define constant $zero = as(<integer>, '0');

define function digit-to-integer(x :: <character>) => (d :: <integer>)
  as(<integer>, x) - $zero;
end function digit-to-integer;
  

define function parse-integer (source :: <stream>);
  with-meta-syntax parse-stream (source)
    variables (d, (sign = +1), (num = 0));
    [{'+', ['-', set!(sign, -1)], []},
     test(digit?, d), set!(num, digit-to-integer(d)),
     loop([test(digit?, d), set!(num, digit-to-integer(d) + 10 * num)])];
    sign * num;
  end with-meta-syntax;
end function parse-integer;

define function parse-finger-query (query :: <string>)
  with-collector into-buffer user like query, collect: collect;
    with-meta-syntax parse-string (query)
      variables (whois, at, c);
      [loop(' '), {[{"/W", "/w"}, yes!(whois)], []},        // Whois switch?
       loop(' '), loop({[{'\n', '\r'}, finish()],           // Newline? Quit.
			{['@', yes!(at), do(collect('@'))], // @? Indirect.
			 [accept(c), do(collect(c))]}})];   // collect char
      values(whois, user(), at);
    end with-meta-syntax;
  end with-collector;
end function parse-finger-query;

define method main(appname, #rest arguments)
  format-out("Enter fixnum: "); force-output(*standard-output*);
  let number = parse-integer(*standard-input*);
  format-out("Result: %d\n", number);

  read-line(*standard-input*); // parse-integer won't consume trailing garbage

  format-out("Enter finger query: "); force-output(*standard-output*);
  let (whois, user, at) = parse-finger-query(read-line(*standard-input*));
  format-out("Results: Whois Switch: %=, Indirect: %=, User: %=\n",
	     whois, at, user);
end method main;

main("example", 1,2,3);


