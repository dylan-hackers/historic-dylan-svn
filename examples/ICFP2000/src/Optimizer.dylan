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


define generic optimizable-one(token, more :: <list>, #key orig :: <pair>) => (tokens :: <list>, closure);

define generic optimizable-two(token1, token2, more :: <list>, #key orig :: <pair>) => (tokens :: <list>, closure);

define generic optimizable-three(token1, token2, token3, more :: <list>, #key orig :: <pair>) => (tokens :: <list>, closure);

define generic optimize-all(tokens :: <list>) => (tokens :: <list>, closure);

define method optimizable-one(token, more :: <list>, #key orig :: <pair>) => (tokens :: <list>, closure);
  orig
end;

define method optimizable-one(token :: <integer>, more :: <pair>, #key orig :: <pair>) => (tokens :: <list>, closure);
  optimizable-two(token, more.head, more.tail, orig: orig)
end;

define method optimizable-two(token1 :: <integer>, token2 == #"negi", more :: <pair>, #key orig :: <pair>) => (tokens :: <list>, closure);
  pair(token1.negative, more)
end;

define method optimizable-two(token1 :: <integer>, token2 == #"negi", more :: <list>, #key orig :: <pair>) => (tokens :: <list>, closure);
  pair(token1.negative, more)
end;

define method optimizable-two(token1, token2, more :: <list>, #key orig :: <pair>) => (tokens :: <list>, closure);
  orig
end;


define method optimizable-two(token1, token2, more :: <pair>, #key orig :: <pair>) => (tokens :: <list>, closure);
  optimizable-three(token1, token2, more.head, more.tail, orig: orig)
end;

define method optimizable-three(token1, token2, token3, more :: <list>, #key orig :: <pair>) => (tokens :: <list>, closure);
  orig
end;

define method optimizable-three(token1 :: <integer>, token2 :: <integer>, token3 == #"addi", more :: <list>, #key orig :: <pair>) => (tokens :: <list>, closure);
  pair(token1 + token2, more)
end;


define method optimize-all(tokens == #()) => (tokens :: <list>, closure);
  tokens
end;

define method optimize-all(orig-tokens :: <pair>) => (tokens :: <list>, closure);
  let tokens-tail = orig-tokens.tail;
  let (optimized :: <list>, closure) = tokens-tail.optimize-all; // closure not needed?
  let orig-tokens = tokens-tail == optimized & orig-tokens | pair(orig-tokens.head, optimized);
  let (tokens :: <list>, closure) = optimizable-one(orig-tokens.head, optimized, orig: orig-tokens);
  if (closure | tokens == orig-tokens)
    values(tokens, closure)
  else
    optimize-all(tokens)
  end if
end;