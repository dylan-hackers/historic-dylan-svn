language: infix-dylan
module: GML-compiler
file: Math-Primitives.dylan
author: Gabor Greif, mailto: gabor@mac.com

define constant preferred-float :: <class> = <float>;	// change them when settled

define macro unary-primitive-definer
	{ define unary-primitive ?:name(?type:expression) end }
	=>
	{ define unary-primitive ?name(?type, ?name) end; }

	{ define unary-primitive ?:name(?type:expression, ?operator:expression) end }
	=>
	{
		define method compile-one(token == ?#"name", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
		  let (cont, remaining) = more-tokens.compile-GML;
		  values(
		         method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
		           let (arg :: ?type, rest :: <list>) = values(stack.head, stack.tail);
		           cont(pair(?operator(arg), rest), env)
		         end method,
		         remaining)
		end;
	}
end macro unary-primitive-definer;

define macro numeric-unary-primitive-definer
	{ define numeric-unary-primitive ?:name(?operator:expression) end }
	=>
	{
		define unary-primitive ?name ## "i"(<integer>, ?operator) end;
		define unary-primitive ?name ## "f"(<float>, ?operator) end;
	}
end;


define macro binary-primitive-definer
	{ define binary-primitive ?:name(?types:expression, ?operator:expression) end }
	=>
	{ define binary-primitive ?name(?types, ?types, ?operator) end; }
	
	{ define binary-primitive ?:name(?front:expression, ?back:expression, ?operator:expression) end }
	=>
	{
		define method compile-one(token == ?#"name", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
		  let (cont, remaining) = more-tokens.compile-GML;
		  values(
		         method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
		           let (right :: ?back, rest :: <pair>) = values(stack.head, stack.tail);
		           let (left :: ?front, rest :: <list>) = values(rest.head, rest.tail);
		           cont(pair(?operator(left, right), rest), env)
		         end method,
		         remaining)
		end;
	}
end macro binary-primitive-definer;

define macro numeric-binary-primitive-definer
	{ define numeric-binary-primitive ?:name(?operator:expression) end }
	=>
	{
		define binary-primitive ?name ## "i"(<integer>, ?operator) end;
		define binary-primitive ?name ## "f"(<float>, ?operator) end;
	}
end;


define unary-primitive acos(<float>) end;
define numeric-binary-primitive add(\+) end;
define unary-primitive asin(<float>) end;
define unary-primitive clampf(<float>, method(f :: <float>) case f < 0 => 0; f > 1 => 1 end case end method) end;
define unary-primitive cos(<float>) end;
define binary-primitive divi(<integer>, truncate/) end;
define binary-primitive divf(<float>, \/) end;
define numeric-binary-primitive eq(\==) end;
define unary-primitive floor(<float>) end;
define unary-primitive frac(<float>, method(f :: <float>) f - f.truncate end method) end;
define numeric-binary-primitive less(\<) end;
define binary-primitive modi(<integer>, modulo) end;
define numeric-binary-primitive mul(\*) end;
define numeric-unary-primitive neg(negative) end;
define unary-primitive real(<float>, curry(as, <double-float>)) end;
define unary-primitive sin(<float>) end;
define unary-primitive sqrt(<float>) end;
define numeric-binary-primitive sub(\-) end;
