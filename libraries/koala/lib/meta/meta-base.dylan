module: meta-base
author: David Lichteblau (david.lichteblau@snafu.de)
copyright: Copyright (c) 1999 David Lichteblau.  All Rights Reserved.

// Please read the file README and the documentation.
// For licensing information see LICENSE.

define macro with-meta-syntax
    { with-meta-syntax ?source-type:name (?source:expression, ?args:*)
	variables (?vars);
	?meta:*;
	?result:body
      end }
      => { ?vars;
	   meta-parse-aux ?source-type, ?source, (?args),
	      ?meta,
	      ?result
	    end }

    { with-meta-syntax ?source-type:name (?source:expression, ?args:*)
	?meta:*;
	?result:body
      end }
      => { meta-parse-aux ?source-type, ?source, (?args),
	      ?meta,
	      ?result
	    end }

  vars:
    { ?:variable, ... } => { let ?variable = #f; ... }
    { (?:variable = ?init:expression), ... } => { let ?variable = ?init; ... }
    { } => { }
end macro with-meta-syntax;

define macro meta-parse-aux
    { meta-parse-aux parse-stream, ?source:expression, (),
       ?meta:*,
       ?result:expression
      end }
      => { let stream = ?source;
	   local method match (form) => (success :: <boolean>);
		   if (peek(stream) == form)
		     read-element(stream);
		     #t;
		   end if;
		 end method match;

          // match-type always is looking at one <character>, so
          // it's more efficient to use rcurry(member?, seq) than
          // instance? on singletons.  match-element fits the bill.
	   local method match-type (type :: <type>)
		  => (success :: <boolean>, result :: <object>);
		   let success = instance?(peek(stream), type);
		   values(success, success & read-element(stream));
		 end method match-type;
          local method match-element(seq :: <sequence>)
                 => (success :: <boolean>, result :: <object>);
                  let success = member?(peek(stream), seq);
                  values(success, success & read-element(stream));
                end method match-element;
	   local method match-test (matches? :: <function>)
		  => (success :: <boolean>, result :: <object>);
		   let success = matches?(peek(stream));
		   values(success, success & read-element(stream));
		 end method match-test;
	   local method match-peek ()
		  => (success :: <boolean>, result :: <object>);
		   values(#t, peek(stream));
		 end method match-peek;
	   local method match-slurp () => ();
		   read-element(stream);
		 end method match-slurp;
	   local method call-sub (sub :: <function>);
		   sub(stream);
		 end method call-sub;
	   process-meta(?meta, match, match-type, match-test, match-peek,
			match-slurp, match-element, call-sub)
	     & ?result }

    { meta-parse-aux parse-string, ?source:expression,
         (#key 
	    ?start:expression = 0,
	    ?end:expression = #f,
	    ?pos:name = index),
        ?meta:*,
        ?result:expression
      end }
    => { let string = ?source;
	 let ?pos = ?start;
	 let stop = ?end | size(string);

	 local method match (form :: <object>) => (success :: <boolean>);
		 select (form by instance?)
		   <byte-character> =>
		     if ((?pos < stop) & (string[?pos] == form))
		       ?pos := ?pos + 1;
		       #t;
		     end if;
		   <byte-string> =>
		     let old-index = ?pos;
		     let success = #f;
		     for (char in form,
			  while: success := match(char))
		     end for;
		     if (success) #t; else ?pos := old-index; #f; end;
		 end select;
	       end method match;

     // same caveat for this match-type as for the stream one; use
     // match-element instead, please
	 local method match-type (type :: <type>);
		 let success = (?pos < stop) & instance?(string[?pos], type);
		 block ()
		   values(success, success & string[?pos]);
		 afterwards
		   if (success) ?pos := ?pos + 1; end;
		 end block;
	       end method match-type;
     local method match-element(seq :: <sequence>)
            => (success :: <boolean>, result)
             let success = (?pos < stop) & member?(string[?pos], seq);
             block ()
               values(success, success & string[?pos]);
             afterwards
               if(success) ?pos := ?pos + 1; end;
             end block;
           end method;
	 local method match-test (matches? :: <function>)
		=> (success :: <boolean>, result :: <object>);
		 let success = (?pos < stop) & matches?(string[?pos]);
		 block ()
		   values(success, success & string[?pos]);
		 afterwards
		   if (success) ?pos := ?pos + 1; end;
		 end block;
	       end method match-test;
	 local method match-peek ()
		=> (success :: <boolean>, result :: <object>);
		 let success = (?pos < stop);
		 values(success, success & string[?pos]);
	       end method match-peek;
	 local method match-slurp () => ();
		 ?pos := ?pos + 1;
	       end method match-slurp;
	   local method call-sub (sub :: <function>);
		   let (position, #rest results)
		     = sub(string, start: ?pos, end: stop);
		   if (position)
		     ?pos := position;
		     apply(values, #t, results);
		   else
		     #f;
		   end if;
		 end method call-sub;
	  process-meta(?meta, match, match-type, match-test, match-peek,
		       match-slurp, match-element, call-sub)
	    & ?result }
end macro meta-parse-aux;

define macro process-meta
    { process-meta(?meta, ?match:name, ?match-type:name, ?match-test:name,
		   ?match-peek:name, ?match-slurp:name, ?match-element:name, ?call-sub:name) }
      => { let match = ?match;
	   let match-type = ?match-type;
	   let match-test = ?match-test;
	   let match-peek = ?match-peek;
	   let match-slurp = ?match-slurp;
           let match-element = ?match-element;
	   let call-sub = ?call-sub;
	   block (finish)
	     ?meta
	   end block }

  meta:
    // Baker:
    { (?:body) }
      => { ?body }
    { [ ?ands ] }
      => { ?ands }
    { { ?ors } }
      => { ?ors }
    { loop(?meta) }
      => { while (?meta) end | #t }

// avoid type ... use element-of if possible instead
    { type(?type:expression, ?:variable) }
      => { begin
	     let (success, value) = match-type(?type);
	     if (success)
	       ?variable := value;
	     end if;
	   end }
  
    // Extensions:
    { do(?:body) }
      => { (?body | #t) }
    { finish() }
      => { finish(#t) }
    { yes!(?var:expression) }
      => { (?var := #t) }
    { no!(?var:expression) }
      => { ((?var := #f) | #t) }
    { set!(?var:expression, ?value:expression) }
      => { ((?var := ?value) | #t) }
    { test(?predicate:expression) }
      => { match-test(?predicate) }
    { accept(?variable:name) }
      => { begin
	     let (success, value) = match-type(<object>);
	     if (success)
	       ?variable := value;
	     end if;
	   end }
    { element-of(?seq:expression, ?:variable) }
    => { begin
           let(success, value) = match-element(?seq);
           if(success) ?variable := value; end if;
         end }
    { test(?predicate:expression, ?variable:name) }
      => { begin
	     let (success, value) = match-test(?predicate);
	     if (success)
	       ?variable := value;
	       #t;
	     end if;
	   end }
    { peek(?var:name, ?test:expression) }
    => { begin
           let (success, value) = match-peek();
           if(success) ?var := value; if(?test) #t; end if; end if;
         end }

// There's no reason for the -ing ending, as none of the other
// pseudo-function expr use that ending.  Therefore, peeking
// is deprecated, use peek instead.  Besides, peeking is very much like
// match-test in that it consumes the character -- not at all in line with
// the real peek, semantically.  The meta peek follows the convention of
// keeping the stream state as it was.
    { peeking(?variable:name, ?test:expression) }
      => { begin
	     let (success, value) = match-peek();
	     if (success)
	       ?variable := value;
	       if (?test)
		 match-slurp();
		 #t;
	       end if;
	     end if;
	   end }
    { ?subroutine:name(?variables:*) }
      => { call-meta-subroutine(call-sub(?subroutine),
			        (?variables), (?variables)) }

    // Simple matching:
    { ?:expression } => { match(?expression) }

  ands:
    { } => { #t }
    { ?meta, ... } => { begin ?meta & ... end }

  ors:
    { } => { #f }
    { ?meta, ... } => { begin ?meta | ... end }
end macro process-meta;

define macro call-meta-subroutine
    { call-meta-subroutine(?sub:expression, (?temp), (?results)) }
      => { let (success, ?temp) = ?sub;
	   if (success)
	     ?results;
	     #t;
	   else
	     #f;
	   end if }

  temp:
    { ?variable:name, ... } => { "temp-" ## ?variable, ... }
    { } => { }
  results:
    { ?variable:name, ... } => { ?variable := "temp-" ## ?variable; ... }
    { } => { }
end macro call-meta-subroutine;


// EOF
