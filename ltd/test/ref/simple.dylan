//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
define method sentence ()
  concatenate(noun-phrase(), verb-phrase());
end method sentence;

define method noun-phrase ()
  concatenate(article(), noun());
end method noun-phrase;

define method verb-phrase ()
  concatenate(verb(), noun-phrase());
end method verb-phrase;

define method article () one-of(#(#"the", #"a")); end method article;

define method noun ()
  one-of(#(#"man", #"ball", #"woman", #"table"));
end method noun;

define method verb ()
  one-of(#(#"hit", #"took", #"saw", #"liked"));
end method verb;

//  ==============================
define method one-of (set)
  // Pick one element of set, and make a list of it.
  list(random-elt(set));
end method one-of;

define method random-elt (choices)
  // Choose an element from a list at random.
  choices[random-uniform(to: size(choices))];
end method random-elt;

//  ==============================
define method adj* ()
  if (random-uniform(to: 2) = 0) #f; else concatenate(adj(), adj*()); end if;
end method adj*;

define method pp* ()
  if (random-elt(#(#"t", #()))) concatenate(pp(), pp*()); else #f; end if;
end method pp*;

define method noun-phrase ()
  concatenate(article(), adj*(), noun(), pp*());
end method noun-phrase;

define method pp () concatenate(prep(), noun-phrase()); end method pp;

define method adj ()
  one-of(#(#"big", #"little", #"blue", #"green", #"adiabatic"));
end method adj;

define method prep ()
  one-of(#(#"to", #"in", #"by", #"with", #"on"));
end method prep;

//  ==============================
// A grammar for a trivial subset of English.
define variable *simple-grammar* =
  #(#(#"sentence", #"->", #(#"noun-phrase", #"verb-phrase")),
    #(#"noun-phrase", #"->", #(#"article", #"noun")),
    #(#"verb-phrase", #"->", #(#"verb", #"noun-phrase")),
    #(#"article", #"->", #"the", #"a"),
    #(#"noun", #"->", #"man", #"ball", #"woman", #"table"),
    #(#"verb", #"->", #"hit", #"took", #"saw", #"liked"));

// The grammar used by generate.  Initially, this is
//   *simple-grammar*, but we can switch to other grammers.
define variable *grammar* = *simple-grammar*;

//  ==============================
define method rule-lhs (rule)
  // The left hand side of a rule.
  first(rule);
end method rule-lhs;

define method rule-rhs (rule)
  // The right hand side of a rule.
  tail(tail(rule));
end method rule-rhs;

define method rewrites (category)
  // Return a list of the possible rewrites for this category.
  rule-rhs(cl-assoc(category, *grammar*));
end method rewrites;

//  ==============================
define method generate (phrase)
  // Generate a random sentence or phrase
  if (instance?(phrase, <list>))
    mappend(generate, phrase);
  elseif (rewrites(phrase))
    generate(random-elt(rewrites(phrase)));
  else
    list(phrase);
  end if;
end method generate;

//  ==============================
define variable *bigger-grammar* =
  #(#(#"sentence", #"->", #(#"noun-phrase", #"verb-phrase")),
    #(#"noun-phrase", #"->", #(#"article", #"adj*", #"noun", #"pp*"),
      #(#"name"), #(#"pronoun")),
    #(#"verb-phrase", #"->", #(#"verb", #"noun-phrase", #"pp*")),
    #(#"pp*", #"->", #"()", #(#"pp", #"pp*")),
    #(#"adj*", #"->", #"()", #(#"adj", #"adj*")),
    #(#"pp", #"->", #(#"prep", #"noun-phrase")),
    #(#"prep", #"->", #"to", #"in", #"by", #"with", #"on"),
    #(#"adj", #"->", #"big", #"little", #"blue", #"green", #"adiabatic"),
    #(#"article", #"->", #"the", #"a"),
    #(#"name", #"->", #"pat", #"kim", #"lee", #"terry", #"robin"),
    #(#"noun", #"->", #"man", #"ball", #"woman", #"table"),
    #(#"verb", #"->", #"hit", #"took", #"saw", #"liked"),
    #(#"pronoun", #"->", #"he", #"she", #"it", #"these", #"those", #"that"));

*grammar* := *bigger-grammar*;

//  ==============================
define method generate-tree (phrase)
  // Generate a random sentence or phrase,
  //   with a complete parse tree.
  if (instance?(phrase, <list>))
    map(generate-tree, phrase);
  elseif (rewrites(phrase))
    pair(phrase, generate-tree(random-elt(rewrites(phrase))));
  else
    list(phrase);
  end if;
end method generate-tree;

//  ==============================
define method generate-all (phrase)
  // Generate a list of all possible expansions of this phrase.
  if (empty?(phrase))
    list(#f);
  elseif (instance?(phrase, <list>))
    combine-all(generate-all(first(phrase)), generate-all(tail(phrase)));
  elseif (rewrites(phrase))
    mappend(generate-all, rewrites(phrase));
  else
    list(list(phrase));
  end if;
end method generate-all;

define method combine-all (xlist, ylist)
  // Return a list of lists formed by appending a y to an x.
  //   E.g., (combine-all '((a) (b)) '((1) (2)))
  //   -> ((A 1) (B 1) (A 2) (B 2)).
  mappend(method (y)
            map(method (x) concatenate(x, y); end method, xlist);
          end method,
          ylist);
end method combine-all;

