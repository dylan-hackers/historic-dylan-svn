module: icfp2000
synopsis: Lexer
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

// <lexer> -- exported.
//
// An object holding the current lexer state.
//
define class <lexer> (<object>)
  slot source :: <stream>, required-init-keyword: source:;
  slot current-posn :: <integer>, init-value: 0;
  slot current-line :: <integer>, init-value: 1;

  slot char, init-value: #f;

  slot buf :: <stretchy-vector>, init-value: make(<stretchy-vector>);
  slot current-token :: false-or(<byte-string>), init-value: #f;
end class <lexer>;

define sealed domain make (singleton(<lexer>));
define sealed domain initialize (<lexer>);

define method getChar(lex :: <lexer>)
  let c = read-element(lex.source, on-end-of-stream: #f);
  //format-out("char = %=\n", c);
  lex.current-posn := lex.current-posn + 1;
  lex.char := c;
end;


define constant $error-token = -1;
define constant $EOF-token = 0;
define constant $left-bracket-token = 1;
define constant $right-bracket-token = 2;
define constant $left-brace-token = 3;
define constant $right-brace-token = 4;
define constant $operator-token = 5;
define constant $identifier-token = 6;
define constant $binder-token = 7;
define constant $boolean-token = 8;
define constant $integer-token = 9;
define constant $float-token = 10;
define constant $string-token = 11;


// make-identifier-or-binder -- internal.
//
// Extract the name from the source location, figure out what kind of word it
// is, and make it.
// 
define method make-identifier-or-binder(token :: <byte-string>)
//    => res :: <identifier-token>;

  format-out("==> in make-identifier-or-binder %=\n", token);

end method make-identifier-or-binder;

// parse-integer -- internal.
//
// Parse and return an integer in the supplied radix.
// 
define method parse-integer-literal(token :: <byte-string>)
 => res :: <integer>;
  format-out("==> in parse-integer-literal %=\n", token);

  let start = 0;
  let finish = token.size;
  local method repeat (posn, result)
	  if (posn < finish)
	    let digit = as(<integer>, token[posn]);
	    if (as(<integer>, '0') <= digit & digit <= as(<integer>, '9'))
	      repeat(posn + 1, result * 10 + digit - as(<integer>, '0'));
	    else
	      error("Bogus digit in integer: %=", as(<character>, digit));
	    end if;
	  else
	    result;
	  end if;
	end method repeat;
  let first = token[start];
  if (first == '-')
    -repeat(start + 1, 0);
  elseif (first == '+')
    error("Bogus integer: can't start with a '+'");
    repeat(start + 1, 0);
  else
    repeat(start, 0);
  end if;

end method parse-integer-literal;


// parse-fp-literal -- internal.
// 
define method parse-fp-literal(token :: <byte-string>)
//    => res :: <literal-token>;
  let value = atof(token);
  format-out("==> in parse-fp-literal %= => %=\n", token, #f); //value);

  value;
end method parse-fp-literal;


// make-string-literal -- internal.
//
// Should be obvious by now.
//
define method make-string-literal(token :: <byte-string>)
//    => res :: <literal-token>;
  format-out("==> in make-string-literal %=\n", token);

end method make-string-literal;



// state machine.

// <state> -- internal.
//
// A particular state in the state machine.
// 
define class <state> (<object>)
  //
  // The name of this state, a symbol.  Not really used once the state
  // machine is built, but we keep it around for debugging purposes.
  slot name :: <symbol>, required-init-keyword: name:;
  //
  // The acceptance result if this state is an accepting state, or #f
  // if it is not.  Symbols are used for magic interal stuff that never
  // makes it out of the lexer (e.g. whitespace), classes for simple
  // tokens that don't need any extra parsing, and functions for more
  // complex tokens.
  slot result :: type-union(<false>, <symbol>, <integer>, <function>),
    required-init-keyword: result:;
  //
  // Either #f or a vector of next-states indexed by character code.
  // During construction, vector elements are either state names or #f.
  // After construction, the state names are replaced by the actual
  // state objects.
  slot transitions :: false-or(<simple-object-vector>),
    required-init-keyword: transitions:;
end class <state>;

define sealed domain make (singleton(<state>));
define sealed domain initialize (<state>);

define method print-object (state :: <state>, stream :: <stream>) => ();
  format(stream, "%=", state.name);
end method print-object;


define method add-transition
    (table :: <simple-object-vector>,
     on :: type-union(<integer>, <character>, <byte-string>),
     new-state :: <symbol>)
    => ();
  //
  // Make as many entries are necessary to represent the transitions
  // from on to new-state.  On can be either an integer, a character,
  // or a byte-string.  If a byte-string, then it supports ranges
  // as in a-z.
  //
  // We also check to see if this entry classes with any earlier
  // entries.  If so, it means someone messed up editing the
  // state machine.
  // 
  select (on by instance?)
    <integer> =>
      if (table[on])
	error("input %= transitions to both %= and %=",
	      as(<character>, on), table[on], new-state);
      else
	table[on] := new-state;
      end if;
    <character> =>
      add-transition(table, as(<integer>, on), new-state);
    <byte-string> =>
      let last = #f;
      let range = #f;
      for (char in on)
	if (range)
	  if (last)
	    for (i from as(<integer>, last) + 1 to as(<integer>, char))
	      add-transition(table, i, new-state);
	    end for;
	    last := #f;
	  else
	    add-transition(table, as(<integer>, '-'), new-state);
	    add-transition(table, as(<integer>, char), new-state);
	    last := char;
	  end if;
	  range := #f;
	elseif (char == '-')
	  range := #t;
	else 
	  add-transition(table, as(<integer>, char), new-state);
	  last := char;
	end if;
      end for;
  end select;
end method add-transition;

define method state
    (name :: <symbol>,
     result :: type-union(<false>, <symbol>, <integer>, <function>),
     #rest transitions)
  //
  // Utility function for making states.  We expand the sequence
  // of transitions into a transition table and make the state object.
  //
  let table = size(transitions) > 0
    & make(<vector>, size: 128, fill: #f);
  for (transition in transitions)
    add-transition(table, head(transition), tail(transition));
  end for;
  make(<state>,
       name: name,
       result: result,
       transitions: table);
end method state;


define method compile-state-machine (#rest states)
    => start-state :: <state>;
  //
  // make a hash table mapping state names to states.
  // 
  let state-table = make(<table>);
  for (state in states)
    if (element(state-table, state.name, default: #f))
      error("State %= multiply defined.", state.name);
    else
      state-table[state.name] := state;
    end if;
  end for;
  //
  // Now that we have a table mapping state names to states, change the
  // entries in the transition tables to refer to the new state
  // object themselves instead of just to the new state name.
  // 
  for (state in states)
    let table = state.transitions;
    if (table)
      for (i from 0 below 128)
	let new-state = table[i];
	if (new-state)
	  table[i] := state-table[new-state];
	end if;
      end for;
    end if;
  end for;
  //
  // Return the start state, 'cause that is what we want
  // $Initial-State to hold.
  element(state-table, #"start");
end method compile-state-machine;


// $Initial-State -- internal.
//
// Build the state graph and save the initial state.
// 
define constant $Initial-State
  = compile-state-machine
      (state(#"start", #f,
	     pair(" \t\f\r\n\<b>", #"whitespace"),
	     pair('/', #"binder"),
	     pair('[', #"lbracket"),
	     pair(']', #"rbracket"),
	     pair('{', #"lbrace"),
	     pair('}', #"rbrace"),
	     pair('%', #"comment"),
	     pair('+', #"plus"),
	     pair('-', #"minus"),
	     pair("A-Za-z", #"symbol"),
	     pair('"', #"double-quote"),
	     pair("0-9", #"decimal")),
       state(#"whitespace", #"whitespace",
	     pair(" \t\f\r\n\<b>", #"whitespace")),
       state(#"comment", #"end-of-line-comment"),
       state(#"lbracket", $left-bracket-token),
       state(#"rbracket", $right-bracket-token),
       state(#"lbrace", $left-brace-token),
       state(#"rbrace", $right-brace-token),
       state(#"minus", #f,
	     pair("0-9", #"signed-decimal")),
       state(#"plus", #f,
	     pair("0-9", #"signed-decimal")),
       state(#"symbol", make-identifier-or-binder,
	     pair("a-zA-Z0-9_", #"symbol"),
	     pair('-', #"symbol")),
       state(#"binder", #f,
	     pair("a-zA-Z", #"symbol")),
       state(#"double-quote", #"string-literal"), 
       state(#"decimal", parse-integer-literal,
	     pair("0-9", #"decimal"),
	     pair('.', #"fp-frac"),
	     pair("eE", #"decimal-e")),

       state(#"signed-decimal", parse-integer-literal,
	     pair("0-9", #"signed-decimal"),
	     pair('.', #"fp-frac")),
       
       state(#"fp-frac", parse-fp-literal,
	     pair("0-9", #"fp-frac"),
	     pair("eE", #"fp-e")),
       state(#"fp-e", #f,
	     pair('-', #"fp-e-sign"),
	     pair('+', #"fp-e-sign"),
	     pair("0-9", #"fp-exp")),
       state(#"fp-e-sign", #f,
	     pair("0-9", #"fp-exp")),
       state(#"fp-exp", parse-fp-literal,
	     pair("0-9", #"fp-exp")),
       
       state(#"decimal-e", #f,
	     pair("0-9", #"decimal-exp"),
	     pair('-', #"decimal-e-sign"),
	     pair('+', #"decimal-e-sign")),
       state(#"decimal-exp", parse-fp-literal,
	     pair("0-9", #"decimal-exp")),
       state(#"decimal-e-sign", #f,
	     pair("0-9", #"decimal-exp")));


// internal-get-token -- internal.
//
// Tokenize the next token and return it.
//
define method internal-get-token (lexer :: <lexer>); // => res :: <token>;
  //
  // Basically, just record where we are starting, and keep
  // advancing the state machine until there are no more possible
  // advances.  We don't stop at the first accepting state we find,
  // because the longest token is supposed to take precedence.  We
  // just note where the last accepting state we came across was,
  // and then when the state machine jams, we just use that latest
  // accepting state's result.
  // 
  let source :: <stream> = lexer.source;
  let result-kind = #f;
  let token :: <stretchy-vector> = lexer.buf;
  token.size := 0;

  local
    method repeat (state)
      if (state.result)
	//
	// It is an accepting state, so record the result and where
	// it ended.
	// 
	//format-out("recording end state, kind %=, token = %=\n",
	//	   state.result, token);
	result-kind := state.result;
      end if;
      //
      // Try advancing the state machine once more if possible.
      // 
      let c = lexer.char;

      if (c)
	let table = state.transitions;
	let char :: <integer> = as(<integer>, c);
	let new-state = table & char < 128 & table[char];
	//format-out("char %=, new-state %=\n", c, new-state);

	if (new-state)
	  add!(token, c);
	  getChar(lexer);
	  repeat(new-state);
	else
	  maybe-done();
	end if;

      else
	maybe-done();
      end if;

    end method repeat,
    method maybe-done ()
      //
      // maybe-done is called when the state machine cannot be
      // advanced any further.  It just checks to see if we really
      // are done or not.
      //

      if (instance?(result-kind, <symbol>))
	//
	// The result-kind is a symbol if this is one of the magic
	// accepting states.  Instead of returning some token, we do
	// some special processing depending on exactly what symbol
	// it is, and then start the state machine over at the
	// initial state.
	//
	select (result-kind)
	  #"whitespace" =>
	    #f;

	  #"newline" =>
	    lexer.current-line := lexer.current-line + 1;

	  #"end-of-line-comment" =>
	    while (lexer.char ~= '\n')
	      getChar(lexer);
	    end;
	    getChar(lexer);

	  #"string-literal" =>
	    lexer.buf.size := 0;
	    while (lexer.char ~= '"')
	      add!(lexer.buf, lexer.char);
	      getChar(lexer);
	    end;
	    getChar(lexer);
	    let str = as(<byte-string>, lexer.buf);
	    make-string-literal(str);

	end select;
	result-kind := #f;
	lexer.buf.size := 0;
	if (lexer.char)
	  repeat($Initial-State);
	end if;
      end if;

    end method maybe-done;
  repeat($Initial-State);
  if (~result-kind)
    //
    // If result-kind is #f, that means we didn't find an accepting
    // state.  Check to see if that means we are at the end or hit
    // an error.
    // 

    if (~lexer.char)
      result-kind := $EOF-token;
    else
      result-kind := $error-token;
    end if;

  end if;


  //
  // And finally, make and return the actual token.
  // 

  let tok = as(<byte-string>, lexer.buf);
  lexer.current-token := tok;
  select(result-kind by instance?)
    <integer> =>
      result-kind;
    <function> =>
      result-kind(tok);
    otherwise =>
      //error("oops");
      format-out("unexpected result-kind = %=\n", result-kind);
      #"oops - bad result kind";
  end select;

end method internal-get-token;


define method atof (string :: <byte-string>,
		    #key start :: <integer> = 0,
		         end: finish :: <integer> = string.size)
    => (value :: <double-float>);

  let posn = start;
  let sign = 1;
  let mantissa = as(<extended-integer>, 0);
  let scale = #f;
  let exponent-sign = 1;
  let exponent = 0;

  // Parse the optional sign.
  if (posn < finish)
    let char = string[posn];
    if (char == '-')
      posn := posn + 1;
      sign := -1;
    elseif (char == '+')
      error("bogus float: explicit + not allowed");
      posn := posn + 1;
    end if;
  end if;

  block (return)
    block (parse-exponent)
      // Parse the mantissa.
      while (posn < finish)
	let char = string[posn];
	posn := posn + 1;
	if (char >= '0' & char <= '9')
	  let digit = as(<integer>, char) - as(<integer>, '0');
	  mantissa := mantissa * 10 + digit;
	  if (scale)
	    scale := scale + 1;
	  end if;
	elseif (char == '.')
	  if (scale)
	    error("bogus float.");
	  end if;
	  scale := 0;
	elseif (char == 'e' | char == 'E')
	  parse-exponent();
	else
	  error("bogus float.");
	end if;
      end while;
      return();
    end block;

    // Parse the exponent.
    if (posn < finish)
      let char = string[posn];
      if (char == '-')
	exponent-sign := -1;
	posn := posn + 1;
      elseif (char == '+')
	error("bogus float: explicit + not allowed");
	posn := posn + 1;
      end if;

      while (posn < finish)
	let char = string[posn];
	posn := posn + 1;
	if (char >= '0' & char <= '9')
	  let digit = as(<integer>, char) - as(<integer>, '0');
	  exponent := exponent * 10 + digit;
	else
	  error("bogus float");
	end if;
      end while;
    end if;
  end block;

  as(<double-float>,
     sign * mantissa * ratio(10,1)
       ^ (exponent-sign * exponent - (scale | 0)));
end method atof;

