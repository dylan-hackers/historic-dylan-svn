//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File lexicon.lisp:  Macros and functions to support the entry of
//  words into the lexicon.
define variable *abbrevs* = make(<table>);

// LTD: No macros.
#"abbrev";

define method clear-abbrevs () size(*abbrevs*) := 0; end method clear-abbrevs;

define method get-abbrev (symbol) *abbrevs*[symbol]; end method get-abbrev;

//  ==============================
define variable *words* = make(<table>, size: 500);

// LTD: No macros.
#"word";

define method add-word (word, cat, #rest info)
  // Put word, with category and other info, into lexicon.
  push!(pair(cat, map(expand-abbrevs-and-variables, info)), *words*[word]);
  word;
end method add-word;

define method kwote (x) list(#"quote", x); end method kwote;

//  ==============================
define method expand-abbrevs-and-variables (exp)
  // Replace all variables in exp with vars, and expand abbrevs.
  let bindings = #f;
  local method expand (exp)
          let _that = #f;
          if (_that := lookup(exp, bindings))
            _that;
          elseif (exp == #"?")
            ?();
          elseif (variable-p(exp))
            begin let var = ?(); push!(pair(exp, var), bindings); var; end;
          elseif (instance?(exp, <pair>))
            reuse-cons(expand(first(exp)), expand(tail(exp)), exp);
          else
            let (expansion, found?) = get-abbrev(exp);
            if (found?)
              expand-abbrevs-and-variables(expansion);
            else
              exp;
            end if;
          end if;
        end method expand;
  expand(exp);
end method expand-abbrevs-and-variables;

//  ==============================
define method word/n (word, cat, cont, #rest info)
  // Retrieve a word from the lexicon.
  if (~ unbound-var-p(deref(word)))
    let old-trail = size(*trail*);
    for (old-entry in *words*[word])
      let entry = deref-copy(old-entry);
      if (instance?(entry, <pair>) & unify!(cat, first(entry))
           & unify!(info, tail(entry)))
        cont();
      end if;
      undo-bindings!(old-trail);
    end for;
  end if;
end method word/n;

//  ==============================
define method word/2 (w, cat, cont) word/n(w, cat, cont); end method word/2;

define method word/3 (w, cat, a, cont)
  word/n(w, cat, cont, a);
end method word/3;

define method word/4 (w, cat, a, b, cont)
  word/n(w, cat, cont, a, b);
end method word/4;

define method word/5 (w, cat, a, b, c, cont)
  word/n(w, cat, cont, a, b, c);
end method word/5;

define method word/6 (w, cat, a, b, c, d, cont)
  word/n(w, cat, cont, a, b, c, d);
end method word/6;

//  ==============================
// LTD: No macros.
#"noun";

define method add-noun-form (base, #key plural = symbol(base, #"s"),
                             sem = base, #rest slots)
  if (plural == #"*")
    add-word(base, #"noun", #"?", slots, sem);
  else
    add-word(base, #"noun", #"3sing", slots, sem);
    add-word(plural, #"noun", #"3plur", slots, sem);
  end if;
end method add-noun-form;

// LTD: No macros.
#"verb";

define method add-verb (senses, base,
                        #key past = symbol(strip-vowel(base), #"ed"),
                        past-part = past,
                        pres-part = symbol(strip-vowel(base), #"ing"),
                        plural = symbol(base, #"s"))
  // Enter a verb into the lexicon.
  add-word(base, #"verb", #"nonfinite", senses);
  add-word(base, #"verb", #(#"finite", #"~3sing", #"present"), senses);
  add-word(past, #"verb", #(#"finite", #"?", #"past"), senses);
  add-word(past-part, #"verb", #"-en", senses);
  add-word(pres-part, #"verb", #"-ing", senses);
  add-word(plural, #"verb", #(#"finite", #"3sing", #"present"), senses);
  add-word(past-part, #"verb", #"passive",
           map(passivize-sense, expand-abbrevs-and-variables(senses)));
end method add-verb;

//  ==============================
define method strip-vowel (word)
  // Strip off a trailing vowel from a string.
  let str = as(<string>, word);
  let end = size(str) - 1;
  if (vowel-p(str[end])) copy-sequence(str, 0, end); else str; end if;
end method strip-vowel;

define method vowel-p (char)
  cl-find(char, "aeiou", test: char-equal?);
end method vowel-p;

//  ==============================
define method passivize-sense (sense)
  //  The first element of sense is the semantics; rest are slots
  pair(first(sense), apply(concatenate!, map(passivize-subcat, tail(sense))));
end method passivize-sense;

define method passivize-subcat (slots)
  // Return a list of passivizations of this subcat frame.
  //  Whenever the 1 slot is of the form (?any 1 (NP ?)),
  //  demote the 1 to a (3), and promote any 2 to a 1.
  if (slot-number(first(slots)) == 1
       & starts-with(third(first(slots)), #"np"))
    let old-1 = pair(first(first(slots)), #(#(3), #(#"pp", #"by", #"?")));
    let _acc = make(<deque>);
    for (slot in slots)
      if (slot-number(slot) == 2)
        push-last(_acc,
                  pair(list(first(slot), 1, third(slot)),
                       concatenate(remove(tail(slots), slot), list(old-1))));
      end if;
    finally
      _acc;
    end for;
  end if;
end method passivize-subcat;

define method slot-number (slot)
  first-or-self(second(slot));
end method slot-number;

//  ==============================
define method copula (senses, entries)
  // Copula entries are both aux and main verb.
  //  They also are used in passive verb phrases and aux-inv-S
  for (entry in entries)
    add-word(first(entry), #"aux", second(entry), third(entry));
    add-word(first(entry), #"verb", second(entry), senses);
    add-word(first(entry), #"aux", second(entry), #"passive");
    add-word(first(entry), #"be");
  end for;
end method copula;

//  ==============================
define method clear-lexicon ()
  size(*words*) := 0;
  clear-abbrevs();
end method clear-lexicon;

define method clear-grammar ()
  clear-examples();
  clear-db();
end method clear-grammar;

//  ==============================
// LTD: No macros.
#"try";

define method try-dcg (#key cat, words)
  // Tries to parse WORDS as a constituent of category CAT.
  //   With no words, runs all the :ex examples for category.
  //   With no cat, runs all the examples.
  if (empty?(words))
    run-examples(cat);
  else
    let args
        = apply(list, #(#"gap", #()), #(#"gap", #()), #"?sem", words,
                #(#"()"));
    begin do(test-unknown-word, words); words; end;
    top-level-prove(select (cat)
                      #"np"
                         => list(apply(list,
                                       #"np",
                                       #"?",
                                       #"?",
                                       #"?wh",
                                       #"?x",
                                       args));
                      #"vp"
                         => list(apply(list,
                                       #"vp",
                                       #"?infl",
                                       #"?x",
                                       #"?sl",
                                       #"?v",
                                       args));
                      #"pp"
                         => list(apply(list,
                                       #"pp",
                                       #"?prep",
                                       #"?role",
                                       #"?wh",
                                       #"?x",
                                       args));
                      #"xp"
                         => list(apply(list,
                                       #"xp",
                                       #"?slot",
                                       #"?constituent",
                                       #"?wh",
                                       #"?x",
                                       args));
                      #"s"
                         => list(apply(list,
                                       #"s",
                                       #"?",
                                       #"?sem",
                                       words,
                                       #(#"()")));
                      #"rel-clause"
                         => list(apply(list,
                                       #"rel-clause",
                                       #"?",
                                       #"?x",
                                       #"?sem",
                                       words,
                                       #(#"()")));
                      #"clause"
                         => list(apply(list,
                                       #"clause",
                                       #"?infl",
                                       #"?x",
                                       #"?int-subj",
                                       #"?v",
                                       #"?g1",
                                       #"?g2",
                                       #"?sem",
                                       words,
                                       #(#"()")));
                    end select);
  end if;
end method try-dcg;

define method test-unknown-word (word)
  // Print a warning message if this is an unknown word.
  if (~ (*words*[word] | instance?(word, <number>)))
    format-out("~&Unknown word: ~a", word);
  end if;
end method test-unknown-word;

//  ==============================
"eof";

