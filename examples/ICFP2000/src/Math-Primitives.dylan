language: infix-dylan
module: GML-compiler
file: Math-Primitives.dylan
author: Gabor Greif, mailto: gabor@mac.com

define macro unary-primitive-definer
  { define unary-primitive ?:name(?type:expression) end }
    => { define unary-primitive ?name(?type, ?name) end; };
    
  { define unary-primitive ?:name(?type:expression, ?operator:expression) end }
    => { define method compile-one(token == ?#"name", more-tokens :: <list>)
	  => (closure :: <function>, remaining :: <list>);
	   let (cont, remaining) = more-tokens.compile-GML;
	   values(
		  method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
		      let (arg :: ?type, rest :: <list>) = values(stack.head, stack.tail);
		      cont(pair(?operator(arg), rest), env)
		  end method,
		  remaining)
	 end;
	 
  define method optimizable-two(arg :: ?type, token2 == ?#"name", more :: <pair>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
    pair(?operator(arg), more)
  end;

  define method optimizable-two(arg :: ?type, token2 == ?#"name", more :: <list>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
    pair(?operator(arg), more)
  end; };
    
end macro unary-primitive-definer;

define macro numeric-unary-primitive-definer
  { define numeric-unary-primitive ?:name(?operator:expression) end }
    => {define unary-primitive ?name ## "i"(<integer>, ?operator) end;
        define unary-primitive ?name ## "f"(<fp>, ?operator) end;}
end;


define macro binary-primitive-definer
  { define binary-primitive ?:name(?types:expression, ?operator:expression) end }
    => { define binary-primitive ?name(?types, ?types, ?operator) end; };
    
  { define binary-primitive ?:name(?front:expression, ?back:expression, ?operator:expression) end }
    =>
  { define method compile-one(token == ?#"name", more-tokens :: <list>)
                              => (closure :: <function>, remaining :: <list>);
	   let (cont, remaining) = more-tokens.compile-GML;
	   values(
		  method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
		      let (right :: ?back, rest :: <pair>) = values(stack.head, stack.tail);
		      let (left :: ?front, rest :: <list>) = values(rest.head, rest.tail);
		      cont(pair(?operator(left, right), rest), env)
		  end method,
		  remaining)
    end;

    define method optimizable-three(token1 :: ?front, token2 :: ?back, token3 == ?#"name", more :: <pair>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
      pair(?operator(token1, token2), more)
    end;
    
    define method optimizable-three(token1 :: ?front, token2 :: ?back, token3 == ?#"name", more :: <list>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
      pair(?operator(token1, token2), more)
    end;
    
    define method optimizable-two(right :: ?back, token2 == ?#"name", more-tokens :: <pair>, suppress-closure == #f, #key orig :: <pair>) => (remaining :: <list>, closure);
      let (cont, remaining) = more-tokens.optimize-compile-GML;
      values( remaining,
              method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
              let (left :: ?front, rest :: <list>) = values(stack.head, stack.tail);
              cont(pair(?operator(left, right), rest), env)
              end method)
    end method;
    
    define method optimizable-two(right :: ?back, token2 == ?#"name", more-tokens :: <list>, suppress-closure == #f, #key orig :: <pair>) => (remaining :: <list>, closure);
      let (cont, remaining) = more-tokens.optimize-compile-GML;
      values( remaining,
              method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
              let (left :: ?front, rest :: <list>) = values(stack.head, stack.tail);
              cont(pair(?operator(left, right), rest), env)
              end method)
    end method; }

end macro binary-primitive-definer;

define macro numeric-binary-primitive-definer
  { define numeric-binary-primitive ?:name(?operator:expression) end }
    => { define binary-primitive ?name ## "i"(<integer>, ?operator) end;
         define binary-primitive ?name ## "f"(<fp>, ?operator) end;}
end;

define constant deg2rad :: <fp> = $double-pi / 180.0;
define constant rad2deg :: <fp> = 180.0 / $double-pi;

define unary-primitive acos(<fp>, method(f :: <fp>) rad2deg * acos(f) end) end;
define unary-primitive asin(<fp>, method(f :: <fp>) rad2deg * asin(f) end) end;
define unary-primitive cos (<fp>, method(f :: <fp>) cos(f * deg2rad) end) end;
define unary-primitive sin (<fp>, method(f :: <fp>) sin(f * deg2rad) end) end;

define unary-primitive clampf(<fp>,method(f :: <fp>) min(1.0, max(0.0, f)) end) end;
define unary-primitive floor(<fp>) end;
define unary-primitive frac(<fp>,  method(f :: <fp>) f - f.truncate end method) end;
define unary-primitive real(<integer>,  curry(as, <fp>)) end;
define unary-primitive sqrt(<fp>) end;

define numeric-unary-primitive neg(negative) end;

define numeric-binary-primitive add(\+) end;
define numeric-binary-primitive eq(\==) end;
define numeric-binary-primitive less(\<) end;
define numeric-binary-primitive mul(\*) end;
define numeric-binary-primitive sub(\-) end;

define binary-primitive divi(<integer>, truncate/) end;

define method optimizable-three(token1 :: <integer>, token2 == 0, token3 == #"divi", more :: <pair>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
  orig
end;

define method optimizable-three(token1 :: <integer>, token2 == 0, token3 == #"divi", more :: <list>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
  orig
end;

//    define method optimizable-three(token1 :: ?front, token2 :: ?back, token3 == ?#"name", more :: <list>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
//      pair(?operator(token1, token2), more)
//    end;
    
/*    define method optimizable-two(right :: ?back, token2 == ?#"name", more-tokens :: <pair>, suppress-closure == #f, #key orig :: <pair>) => (remaining :: <list>, closure);
      let (cont, remaining) = more-tokens.optimize-compile-GML;
      values( remaining,
              method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
              let (left :: ?front, rest :: <list>) = values(stack.head, stack.tail);
              cont(pair(?operator(left, right), rest), env)
              end method)
    end method;
    
    define method optimizable-two(right :: ?back, token2 == ?#"name", more-tokens :: <list>, suppress-closure == #f, #key orig :: <pair>) => (remaining :: <list>, closure);
      let (cont, remaining) = more-tokens.optimize-compile-GML;
      values( remaining,
              method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
              let (left :: ?front, rest :: <list>) = values(stack.head, stack.tail);
              cont(pair(?operator(left, right), rest), env)
              end method)
    end method; }
*/
define binary-primitive divf(<fp>, \/) end;
define binary-primitive modi(<integer>, modulo) end;
