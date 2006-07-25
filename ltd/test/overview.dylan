//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File overview.lisp: miscellaneous functions from Overview chapter
define method tax-bracket (income)
  // Determine what percent tax should be paid for this income.
  if (income < 10000.0)
    0.0;
  elseif (income < 30000.0)
    0.2;
  elseif (income < 50000.0)
    0.25;
  elseif (income < 70000.0)
    0.3;
  else
    0.35;
  end if;
end method tax-bracket;

//  ==============================
define class <player> (<object>)
  slot player-score = 0, init-keyword: #"player-score";
  slot player-wins = 0, init-keyword: #"player-wins";
end class <player>;

define method determine-winner (players)
  // Increment the WINS for the player with highest score.
  inc!(player-wins(first(sort!(players,
                               test: method (x, y)
                                       x.player-score > y.player-score;
                                     end method))));
end method determine-winner;

//  ==============================
define method length1 (list)
  let len = 0;
  //  start with LEN=0
  for (element in list)
    //  and on each iteration
    inc!(len);
  end for;
  //   increment LEN by 1
  len;
end method length1;

//  and return LEN
//  ==============================
define method length1.1 (list)
  let len = 0;
  //  (not my preference)
  for (element in list)
    //  uses len as result here
    inc!(len);
  finally
    len;
  end for;
end method length1.1;

//  ==============================
define method length2 (list)
  let len = 0;
  //  start with LEN=0
  begin
    do(method (element)
         //  and on each iteration
         inc!(len);
       end method,
       //   increment LEN by 1
       list);
    list;
  end;
  len;
end method length2;

//  and return LEN
//  ==============================
define method length3 (list)
  for (len = 0 then len + 1,
       //  start with LEN=0, increment
       l(list, rest(l)) = nil then nil,
       until //  ... on each iteration
       empty?(l))
    #f;
  finally
    len;
  end for;
end method length3;

//  (until the end of the list)
//  ==============================
define method length4 (list)
  let _acc = 0;
  for (element in list) if (#t) inc!(_acc); end if; finally _acc; end for;
end method length4;

//    counting each one
define method length5 (list)
  let _acc = 0;
  for (element in list) inc!(_acc, 1); finally _acc; end for;
end method length5;

//    adding 1 each time
define method length6 (list)
  let len = 0;
  block (return)
    for (until empty?(list), element = pop!(list) then pop!(list))
      inc!(len);
      //   increment LEN by 1
      finally;
      return(len);
    end for;
  end block;
end method length6;

//  and return LEN
//  ==============================
define method length7 (list) cl-count-if(true, list); end method length7;

define method true (x) #t; end method true;

//  ==============================
define method length8 (list)
  if (empty?(list)) 0; else 1 + find-key(list, true); end if;
end method length8;

//  ==============================
define method length9 (list)
  if (empty?(list)) 0; else 1 + length9(tail(list)); end if;
end method length9;

//  ==============================
define method length10 (list) length10-aux(list, 0); end method length10;

define method length10-aux (sublist, len-so-far)
  if (empty?(sublist))
    len-so-far;
  else
    length10-aux(tail(sublist), 1 + len-so-far);
  end if;
end method length10-aux;

//  ==============================
define method length11 (list, #key len-so-far = 0)
  if (empty?(list))
    len-so-far;
  else
    length11(tail(list), 1 + len-so-far);
  end if;
end method length11;

//  ==============================
define method length12 (the-list)
  local method length13 (list, len-so-far)
          if (empty?(list))
            len-so-far;
          else
            length13(tail(list), 1 + len-so-far);
          end if;
        end method length13;
  length13(the-list, 0);
end method length12;

//  ==============================
define method product (numbers)
  // Multiply all the numbers together to compute their product.
  let prod = 1;
  block (return)
    for (n in numbers)
      if (n = 0) return(0); else prod := n * prod; end if;
    finally
      prod;
    end for;
  end block;
end method product;

//  ==============================
// LTD: No macros.
#"while";

//  ==============================
// LTD: No macros.
#"while";

//  ==============================
// LTD: No macros.
#"while";

//  ==============================
define method dprint (x)
  // Print an expression in dotted pair notation.
  if (not(instance?(x, <list>)))
    print(x, *standard-output*);
  else
    print("(", *standard-output*);
    dprint(first(x));
    pr-rest(tail(x));
    print(")", *standard-output*);
    x;
  end if;
end method dprint;

define method pr-rest (x)
  print(" . ", *standard-output*);
  dprint(x);
end method pr-rest;

//  ==============================
define method pr-rest (x)
  let _that = #f;
  if (_that := empty?(x))
    _that;
  elseif (not(instance?(x, <list>)))
    print(" . ", *standard-output*);
    print(x, *standard-output*);
  else
    print(" ", *standard-output*);
    dprint(first(x));
    pr-rest(tail(x));
  end if;
end method pr-rest;

//  ==============================
define method same-shape-tree (a, b)
  // Are two trees the same except for the leaves?
  // LTD: Function TREE-EQUAL not yet implemented.
  tree-equal(a, b, test: true);
end method same-shape-tree;

define method true (#rest ignore) #t; end method true;

//  ==============================
define method english->french (words)
  replace-multiple-in-tree(#(#(#"are" . #"va"), #(#"book" . #"libre"),
                             #(#"friend" . #"ami"), #(#"hello" . #"bonjour"),
                             #(#"how" . #"comment"), #(#"my" . #"mon"),
                             #(#"red" . #"rouge"), #(#"you" . #"tu")),
                           words);
end method english->french;

//  ==============================
define class <node> (<object>)
  slot node-name, init-keyword: #"node-name";
  slot node-yes = #f, init-keyword: #"node-yes";
  slot node-no = #f, init-keyword: #"node-no";
end class <node>;

define variable *db* =
  make-node(name: #"animal", yes: make-node(name: #"mammal"),
            no: make-node(name: #"vegetable", no: make-node(name: #"mineral")));

define method questions (#key node = *db*)
  format-out("\nIs it a %S? ", node.node-name);
  select (read())
    (#"y", #"yes")
       => if (~ empty?(node.node-yes))
            questions(node.node-yes);
          else
            node.node-yes := give-up();
          end if;
    (#"n", #"no")
       => if (~ empty?(node.node-no))
            questions(node.node-no);
          else
            node.node-no := give-up();
          end if;
    #"it"
       => #"aha!";
    otherwise
       => format-out("Reply with YES, NO, or IT if I have guessed it.");
           questions(node);
  end select;
end method questions;

define method give-up ()
  format-out("\nI give up - what is it? ");
  make-node(name: // LTD: Function READ not yet implemented.
                  read());
end method give-up;

//  ==============================
define method average (numbers)
  if (empty?(numbers))
    error("Average of the empty list is undefined.");
  else
    reduce1(\+, numbers) / size(numbers);
  end if;
end method average;

//  ==============================
define method average (numbers)
  if (empty?(numbers))
    cerror("Use 0 as the average.",
           "Average of the empty list is undefined.");
    0;
  else
    reduce1(\+, numbers) / size(numbers);
  end if;
end method average;

//  ==============================
define method sqr (x)
  // Multiply x by itself.
  check-type(x, number);
  x * x;
end method sqr;

//  ==============================
define method sqr (x)
  // Multiply x by itself.
  assert(instance?(x, <number>));
  x * x;
end method sqr;

//  ==============================
define method sqr (x)
  // Multiply x by itself.
  assert(instance?(x, <number>));
  x * x;
end method sqr;

//  ==============================
define method eat-porridge (bear)
  assert(begin
           let g113933 = temperature(bear-porridge(bear));
           too-cold < g113933 & g113933 < too-hot;
         end);
  eat(bear-porridge(bear));
end method eat-porridge;

//  ==============================
define method adder (c)
  // Return a function that adds c to its argument.
  method (x) x + c; end method;
end method adder;

//  ==============================
define method bank-account (balance)
  // Open a bank account starting with the given balance.
  method (action, amount)
    select (action)
      #"deposit"
         => balance := balance + amount;
      #"withdraw"
         => balance := balance - amount;
      otherwise
         => #f;
    end select;
  end method;
end method bank-account;

//  ==============================
define method math-quiz (op, range, n)
  // Ask the user a series of math problems.
  for (i from 0 below n)
    problem(random-uniform(to: range), op, random-uniform(to: range));
  end for;
end method math-quiz;

define method problem (x, op, y)
  // Ask a math problem, read a reply, and say if it is correct.
  format-out("\nHow much is %d %S %d?", x, op, y);
  if (// LTD: Function READ not yet implemented.
      read()
       == op(x, y))
    print("Correct!", *standard-output*);
  else
    print("Sorry, that's not right.", *standard-output*);
  end if;
end method problem;

//  ==============================
define method math-quiz (#key op = #"+", range = 100, n = 10)
  // Ask the user a series of math problems.
  for (i from 0 below n)
    problem(random-uniform(to: range), op, random-uniform(to: range));
  end for;
end method math-quiz;

//  ==============================
define method math-quiz (#key op = #"+", range = 100, n = 10)
  // Ask the user a series of math problems.
  for (i from 0 below n)
    problem(random-uniform(to: range), op, random-uniform(to: range));
  end for;
end method math-quiz;

//  ==============================
define method find-all (item, sequence, #rest keyword-args, #key test = \==,
                        test-not, #"#all-keys")
  // Find all those elements of sequence that match item,
  //   according to the keywords.  Doesn't alter sequence.
  if (test-not)
    apply(cl-remove, item, sequence, test-not: complement(test-not),
          keyword-args);
  else
    apply(cl-remove, item, sequence, test: complement(test), keyword-args);
  end if;
end method find-all;

//  ==============================
// LTD: No macros.
#"while2";

//  ==============================
define method length14 (list)
  for (element in list) inc!(len); finally len; end for;
end method length14;

//  ==============================
define method length-r (list)
  reduce1(\+, map(method (x) 1; end method, list));
end method length-r;

define method length-r (list)
  reduce(method (x, y) x + 1; end method, 0, list);
end method length-r;

define method length-r (list)
  reduce1(compose(\+, method (x) 1; end method), list);
end method length-r;

//  ==============================
"eof";

