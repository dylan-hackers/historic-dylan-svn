language: infix-dylan
module: GML-compiler

define method run-gml(tokens :: <list>) => new-stack :: <list>;
  let (closure, remaining) = tokens.compile-GML;
  if (remaining.empty?)
    closure(#(), top-level-environment)
  else
    error("could not compile tokens: %=", remaining)
  end if
end;

define method top-level-environment(binding :: <symbol>) => closure :: <function>;
  error("no symbol %= in environment", binding);
end;

define constant identity-continuation
  = method(stack :: <list>, env :: <function>) => new-stack :: <list>;
      stack
    end;

// the compile function

define generic compile-GML(tokens :: <list>) => (closure :: <function>, remaining :: <list>);

/*
compilation results in a closure that takes a stack and an environment and returns a new stack
                 compilation takes a token list, and returns all tokens that it cannot consume
*/

define method compile-GML(tokens :: <empty-list>) => (closure :: <function>, remaining :: <list>);
  values(identity-continuation, #())
end;

define method compile-GML(tokens :: <pair>) => (closure :: <function>, remaining :: <list>);
  compile-one(tokens.head, tokens.tail)
end;

define constant test-phrase-1 = #(1,'{', '/', #"x", #"x", #"x", '}', #"apply", #"addi");

define constant test-phrase-2 = #(1, '/', x:, '{', x:, '}', '/', f:, 2, '/', x:, f:, apply:, x:, addi:);

define constant test-phrase-3 = #(#t, '{', 1, '}', '{', 2, '}', if:);

define constant test-phrase-4
  = #('{', '/',  self: , '/',  n:, n:, 2, lessi:, '{', 1, '}', '{', n:, 1, subi:, self:, self:, apply:, n:, muli:, '}', if:, '}', '/',  fact:,
      12, fact:, fact:, apply:);

define constant test-phrase-5 = #(1, '/', a:, '[', 1, 2, a:, 1.9, 2.4, 5.6, point:, ']', length:);

define constant test-phrase-6
  = #('[', ']', '/', nil:,
      '{', '/', cdr:, '/', car:, '[', car:, cdr:, ']', '}', '/', cons:,
      '{', '/', if-cons:, '/', if-nil:, '/', lst:,
      lst:, length:, 0, eqi:,
      if-nil:,
      '{', lst:, 0, get:, lst:, 1, get:, if-cons:, apply:, '}',
      if:,
      '}', '/', match:,
      
      nil:, '{', 0, '}', '{', 2, '}', match:, apply:,
      3, 4, cons:, apply:, '{', 0, '}', '{', addi:, '}', match:, apply:
    );

define generic compile-one(token, tokens :: <list>) => (closure :: <function>, remaining :: <list>);

define method compile-one(token, more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = compile-GML(more-tokens);
  values(method(stack :: <list>, env :: <function>) => new-stack :: <list>;
           cont(pair(token, stack), env)
         end,
         remaining)
end;

define method compile-one(token == '}', more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  values(identity-continuation, pair(token, more-tokens))
end;

define method compile-one(token == '{', more-tokens :: <pair>) => (closure :: <function>, remaining :: <list>);
  let (closure, remaining) = more-tokens.compile-GML;
  let (closing :: '}'.singleton, remaining :: <list>) = values(remaining.head, remaining.tail);
  let (cont, remaining) = remaining.compile-GML;
  values(method(stack :: <list>, env :: <function>) => new-stack :: <list>;
           cont(pair(rcurry(closure, env), stack), env)
         end,
         remaining)
end;

define method compile-one(token :: <symbol>, more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = more-tokens.compile-GML;
  values(method(stack :: <list>, env :: <function>) => new-stack :: <list>;
           cont(pair(env(token), stack), env)
         end,
         remaining)
end;

define method compile-one(token == '/', more-tokens :: <pair>) => (closure :: <function>, remaining :: <list>);
  let binding :: <symbol> = more-tokens.head;

  if (sorted-applicable-methods(compile-one, binding, more-tokens.tail).size > 2)
  	error("cannot rebind reserved word '%s'", as(<byte-string>, binding));
  end if;

  let (cont, remaining) = compile-GML(more-tokens.tail);
  values(
         method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
           let value = stack.head;
           cont(stack.tail,
                method(var :: <symbol>) => value;
                  if (var == binding)
                    value
                  else
                    env(var)
                  end if
                end method)
         end method,
         remaining)
end;

define method compile-one(token == #"apply", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = more-tokens.compile-GML;
  values(
         method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
           let closure :: <function> = stack.head;
           cont(closure(stack.tail), env)
         end method,
         remaining)
end;

define method compile-one(token == #"if", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = more-tokens.compile-GML;
  values(
         method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
           let (false-closure :: <function>, rest :: <pair>) = values(stack.head, stack.tail);
           let (true-closure :: <function>, rest :: <pair>) = values(rest.head, rest.tail);
           let (condition :: <boolean>, rest :: <list>) = values(rest.head, rest.tail);
           cont((condition & true-closure | false-closure)(rest), env)
         end method,
         remaining)
end;

define method compile-one(token == #"true", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = more-tokens.compile-GML;
  values(
         method(stack :: <list>, env :: <function>) => new-stack :: <list>;
           cont(pair(#t, stack), env)
         end method,
         remaining)
end;

define method compile-one(token == #"false", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = more-tokens.compile-GML;
  values(
         method(stack :: <list>, env :: <function>) => new-stack :: <list>;
           cont(pair(#f, stack), env)
         end method,
         remaining)
end;


// Points

define sealed class <point>(<object>)
     slot x :: <fp>, required-init-keyword: x:;
     slot y :: <fp>, required-init-keyword: y:;
     slot z :: <fp>, required-init-keyword: z:;
end class <point>;

define method compile-one(token == #"point", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = more-tokens.compile-GML;
  values(
         method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
           let (z :: <fp>, rest :: <pair>) = values(stack.head, stack.tail);
           let (y :: <fp>, rest :: <pair>) = values(rest.head, rest.tail);
           let (x :: <fp>, rest :: <list>) = values(rest.head, rest.tail);
           cont(pair(make(<point>, x: x, y: y, z: z), rest), env)
         end method,
         remaining)
end;

define method compile-one(token == #"getx", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = more-tokens.compile-GML;
  values(
         method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
           let (p :: <point>, rest :: <list>) = values(stack.head, stack.tail);
           cont(pair(p.x, rest), env)
         end method,
         remaining)
end;

define method compile-one(token == #"gety", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = more-tokens.compile-GML;
  values(
         method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
           let (p :: <point>, rest :: <list>) = values(stack.head, stack.tail);
           cont(pair(p.y, rest), env)
         end method,
         remaining)
end;

define method compile-one(token == #"getz", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = more-tokens.compile-GML;
  values(
         method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
           let (p :: <point>, rest :: <list>) = values(stack.head, stack.tail);
           cont(pair(p.z, rest), env)
         end method,
         remaining)
end;

// Arrays

define method compile-one(token == '[', more-tokens :: <pair>) => (closure :: <function>, remaining :: <list>);
  let (array-builder, remaining :: <pair>) = more-tokens.compile-GML;
  let (closing :: ']'.singleton, remaining :: <list>) = values(remaining.head, remaining.tail);
  let (cont, remaining) = remaining.compile-GML;
  values(
         method(stack :: <list>, env :: <function>) => new-stack :: <list>;
           cont(pair(as(<simple-object-vector>, array-builder(#(), env)).reverse!, stack), env)
         end method,
         remaining)
end;

define method compile-one(token == ']', more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  values(identity-continuation, pair(token, more-tokens))
end;

define method compile-one(token == #"get", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = more-tokens.compile-GML;
  values(
         method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
           let (i :: <integer>, rest :: <pair>) = values(stack.head, stack.tail);
           let (a :: <vector>, rest :: <list>) = values(rest.head, rest.tail);
           cont(pair(a[i], rest), env)
         end method,
         remaining)
end;

define method compile-one(token == #"length", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (cont, remaining) = more-tokens.compile-GML;
  values(
         method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
           let (a :: <vector>, rest :: <list>) = values(stack.head, stack.tail);
           cont(pair(a.size, rest), env)
         end method,
         remaining)
end;