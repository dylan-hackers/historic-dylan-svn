language: infix-dylan
module: GML-compiler
file: optimizer.dylan
author: Gabor Greif, mailto: gabor@mac.com

define function optimize-compile-GML(tokens :: <list>) => (closure :: <function>, remaining :: <list>);
  let (remaining :: <list>, closure :: <function>.false-or) = tokens.optimize-all;
  if (closure)
    values(closure, remaining)
  else
    remaining.compile-GML;
  end if
end optimize-compile-GML;

define generic optimizable-one(token, more :: <list>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);

define generic optimizable-two(token1, token2, more :: <list>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);

define generic optimizable-three(token1, token2, token3, more :: <list>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);

define generic optimize-all(tokens :: <list>, #key suppress-closure) => (tokens :: <list>, closure);

define method optimizable-one(token, more :: <list>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
  orig
end;

define method optimizable-one(token :: <symbol>, more :: <list>, suppress-closure == #t, #key orig :: <pair>) => (tokens :: <list>, closure);
  orig
end;

define method optimizable-one(token :: <integer>, more :: <pair>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
  optimizable-two(token, more.head, more.tail, suppress-closure, orig: orig)
end;

define method optimizable-one(token :: <fp>, more :: <pair>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
  optimizable-two(token, more.head, more.tail, suppress-closure, orig: orig)
end;

define method optimizable-two(token1, token2, more :: <list>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
  orig
end;


define method optimizable-two(token1, token2, more :: <pair>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
//  debug-print("####1### %= %= %= %= %= ", token1, token2, more.head, more.tail, suppress-closure);
  optimizable-three(token1, token2, more.head, more.tail, suppress-closure, orig: orig)
end;

define method optimizable-three(token1, token2, token3, more :: <list>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
  orig
end;

// optimize #"point"

define method optimizable-three(token1 :: <fp>, token2 :: <fp>, token3 :: <fp>, more :: <pair>, suppress-closure :: <boolean>, #key orig :: <pair>) => (tokens :: <list>, closure);
    //debug-print("####2###");

  if (more.head == #"point")
    pair(make(<point>, x: token1, y: token2, z: token3), more.tail)
  else
    orig
  end if
end;

define method optimize-all(tokens == #(), #key suppress-closure) => (tokens :: <list>, closure);
  tokens
end;

define method optimize-all(orig-tokens :: <pair>, #key suppress-closure) => (tokens :: <list>, closure);
  //debug-print("entering optimize-all: %= suppress: %=\n", orig-tokens, suppress-closure);
  let tokens-tail = orig-tokens.tail;
  let optimized-tail :: <list> = optimize-all(tokens-tail, suppress-closure: #t);
///    debug-print("tokens-tail.optimize-all: %=\n", optimized-tail);

 // let optimized = closure & orig-tokens | optimized-tail; // for now, undo the opt.
  let orig-tokens = tokens-tail == optimized-tail & orig-tokens | pair(orig-tokens.head, optimized-tail);
///  debug-print("optimizable-one gets passed: %= %=\n", orig-tokens.head, orig-tokens.tail, suppress-closure);
  let (tokens :: <list>, closure) = optimizable-one(orig-tokens.head, orig-tokens.tail, suppress-closure, orig: orig-tokens);
  //debug-print("optimizable-one returned: %= %=\n", tokens, closure);
  if (closure | tokens == orig-tokens)
    //debug-print("returning them\n");
    values(tokens, closure)
  else
    //debug-print("optimizing more\n");
    optimize-all(tokens, suppress-closure: suppress-closure)
  end if
end;
