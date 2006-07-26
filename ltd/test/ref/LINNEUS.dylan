//  LINNEUS.CL
//  (C) Copyright 1995 by Steven L. Tanimoto.
//  This program is described in Chapter 4 ("Knowledge Representation") of
//  "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
//  published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
//  Permission is granted for noncommercial use and modification of
//  this program, provided that this copyright notice is retained
//  and followed by a notice of any modifications made to the program.
//  The function LINNEUS embodies the main loop.
define method linneus ()
  // Top-level loop for interaction with LINNEUS.
  format-out("(THIS IS LINNEUS. PLEASE TALK TO ME)\n");
  block (return)
    while (#t)
      format-out("--> ");
      //  Print prompt symbol.
      if (interpret(// LTD: Function READ not yet implemented.
                    read())
           == #"bye")
        return(#(#"goodbye"));
      end if;
    end while;
  end block;
end method linneus;

//  INTERPRET contains the production rules.
define method interpret (text)
  // Applies production rules embedded in a COND form to
  //    interpret and process the user's input sentence.
  let _that = #f;
  if (_that := handle-assertion(text))
    _that;
  elseif (_that := handle-what-is(text))
    _that;
  elseif (_that := handle-is-a(text))
    _that;
  elseif (_that := handle-why(text))
    _that;
    //  rule for session termination:
    elseif (match(#(#"bye"), text))
    #"bye";
    //  rule for all other inputs:
    else
    answer(#(#"i", #"do", #"not", #"understand"));
  end if;
end method interpret;

define method answer (answer-list)
  // Prints out the answer and returns T.
  print(answer-list, *standard-output*);
  write-element(*standard-output*, '\n');
  #t;
end method answer;

define method handle-assertion (text)
  // Tests for and handles assertion of fact by the user.
  let b
      = match(#(#(#"match-article", #"article1"), #(#"?", #"x"), #"is",
                #(#"match-article", #"article2"), #(#"?", #"y")),
              text);
  if (b)
    let x = val(#"x", b);
    let y = val(#"y", b);
    let article1 = val(#"article1", b);
    let article2 = val(#"article2", b);
    add-superset(x, y);
    add-subset(y, x);
    set-article(x, article1);
    set-article(y, article2);
    answer(#(#"i", #"understand"));
  end if;
end method handle-assertion;

define method handle-what-is (text)
  // Tests for and handles questions e.g., (WHAT IS A DOG).
  block (return-from-handle-what-is)
    let b
        = match(#(#"what", #"is", #(#"match-article", #"article1"),
                  #(#"?", #"x")),
                text);
    let isaflag = #f;
    let includeflag = #f;
    if (b)
      let x = val(#"x", b);
      let y = val(#"y", b);
      if (y := get-isa(x))
        isaflag := #t;
      elseif (y := get-includes(x))
        includeflag := #t;
      else
        format-out("(I DON'T KNOW)\n");
        return-from-handle-what-is(#t);
      end if;
      answer(concatenate(list(get-article(x)), list(x),
                         if (isaflag)
                           #(#"is");
                         else
                           #(#"is", #"something", #"more", #"general",
                             #"than");
                         end if,
                         make-conj(y)));
    end if;
  end block;
end method handle-what-is;

define method handle-is-a (text)
  // Tests for and handles queries of the form (IS A DOG AN ANIMAL).
  let b
      = match(#(#"is", #(#"match-article", #"article1"), #(#"?", #"x"),
                #(#"match-article", #"article2"), #(#"?", #"y")),
              text);
  if (b)
    let x = val(#"x", b);
    let y = val(#"y", b);
    if (isa-test(x, y, 10))
      answer(concatenate(#(#"yes", #"indeed"), tell(x, y)));
    else
      answer(#(#"sorry", #"not", #"that", #"i", #"know", #"of"));
    end if;
  end if;
end method handle-is-a;

define method handle-why (text)
  // Tests for and handles WHY questions.
  let b
      = match(#(#"why", #"is", #(#"match-article", #"article1"),
                #(#"?", #"x"), #(#"match-article", #"article2"),
                #(#"?", #"y")),
              text);
  if (b)
    let x = val(#"x", b);
    let y = val(#"y", b);
    if (isa-test(x, y, 10))
      answer(pair(#"because", explain-links(x, y)));
    else
      answer(#(#"but", #"as", #"far", #"as", #"i", #"know", #"it", #"is",
               #"not!"));
    end if;
  end if;
end method handle-why;

//  Create hash tables to store the knowledge
//  base components, and define the functions
//  for storing and retrieving knowledge.
begin
  let isa-base = make(<table>, size: 20);
  let includes-base = make(<table>, size: 20);
  let article-base = make(<table>, size: 20);
  define method set-isa (x, y) isa-base[x] := y; end method set-isa;
  define method get-isa (x) isa-base[x]; end method get-isa;
  define method set-includes (x, y)
    includes-base[x] := y;
  end method set-includes;
  define method get-includes (x) includes-base[x]; end method get-includes;
  define method set-article (x, article)
    article-base[x] := article;
  end method set-article;
  define method get-article (x) article-base[x]; end method get-article;
end;

//  ADD-SUPERSET makes X one of the supersets of ANAME:
define method add-superset (aname, x)
  // Establishes X as a superset of ANAME.
  set-isa(aname, add!(x, get-isa(aname)));
end method add-superset;

//  ADD-SUBSET makes X one of the subsets of ANAME:
define method add-subset (aname, x)
  // Establishes X as a subset of ANAME.
  set-includes(aname, add!(x, get-includes(aname)));
end method add-subset;

define method match-article (x)
  // Returns T if X is in the list of articles.
  member?(x, #(#"a", #"an", #"the", #"that", #"this", #"those", #"these"));
end method match-article;

//  MAKE-CONJ takes a list, e.g., (X Y Z) and inserts
//  appropriate articles in front of each element, and
//  inserts the atom AND between successive elements,
//  e.g., (AN X AND A Y AND A Z):
define method make-conj (lst)
  // Expresses LST as a conjunctive phrase.
  if (empty?(lst))
    #f;
  elseif (empty?(tail(lst)))
    pair(get-article(first(lst)), lst);
  else
    pair(get-article(first(lst)),
         pair(first(lst), pair(#"and", make-conj(tail(lst)))));
  end if;
end method make-conj;

//  ISA-TEST returns T if there is a path from X to Y
//  consisting of ISA links, with path length no more
//  than N.
define method isa-test (x, y, n)
  // Tests whether an X is a Y.
  block (isa) isa-test1(x, y, n); end block;
end method isa-test;

//  ISA-TEST1 is the recursive slave of ISA-TEST.
define method isa-test1 (x, y, n)
  // Implements the depth-first search for ISA-TEST.
  if (x == y)
    #t;
  elseif (zero?(n))
    #f;
  elseif (member?(y, get-isa(x)))
    isa(#t);
  else
    for (xx in get-isa(x)) isa-test1(xx, y, n - 1); end for;
  end if;
end method isa-test1;

define method explain-links (x, y)
  // EXPLAIN-LINKS answers WHY questions.
  if (x == y)
    #(#"they", #"are", #"identical");
    //  1st special case
    elseif (member?(y, get-isa(x)))
    #(#"you", #"told", #"me");
    //  2nd special case
    else
    explain-chain(x, get-isa(x), y);
  end if;
end method explain-links;

//  general case
//  The recursive function EXPLAIN-CHAIN is called by
//  EXPLAIN-LINKS.  EXPLAIN-CHAIN produces an
//  explanation of the first chain from X to Y that passes
//  through a member of L.
define method explain-chain (x, l, y)
  // Recursively helps EXPLAIN-LINKS.
  if (empty?(l))
    #f;
    //  L should never be null.
    elseif (member?(y, l))
    //  See if last link --
    pair(#"and", tell(x, y));
    //  Yes, precede by AND.
    elseif (isa-test(first(l), y, 10))
    //  Does chain go through first L?
    concatenate(tell(x, first(l)), //  Yes, explain this link, etc.
                explain-chain(first(l), get-isa(first(l)), y));
  else
    explain-chain(x, tail(l), y);
  end if;
end method explain-chain;

// else try next in L.
//  TELL is a helping function for EXPLAIN-CHAIN.
define method tell (x, y)
  // Explains the (single) link from X to Y.
  list(get-article(x), x, #"is", get-article(y), y);
end method tell;

