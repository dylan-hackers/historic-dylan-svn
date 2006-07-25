//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
//  This file was stolen from Matt Ginsberg's MVL 3/8/93
"(in-package dtp)";

define variable *logical-operators* =
  #(#"<=", #"=>", #"<=>", #"and", #"or", #"not");

define method sentence-to-cnf (sentence)
  // Rewrite constant predicates, then call CNF
  if (cl-find(head(sentence), *logical-operators*))
    sentence := pair(head(sentence), map(rewrite-predicate, tail(sentence)));
  end if;
  cnf(sentence);
end method sentence-to-cnf;

define method rewrite-predicate (predicate)
  // P becomes (P), and (NOT Q) becomes (NOT (Q)), else unchanged
  if (not(instance?(predicate, <list>)))
    list(predicate);
  elseif (#"not" == first(predicate)
           & not(instance?(second(predicate), <list>)))
    list(#"not", list(second(predicate)));
  else
    predicate;
  end if;
end method rewrite-predicate;

define method simplify-dnf (dnf)
  if (head(dnf) == #"or" & empty?(tail(tail(dnf))))
    second(dnf);
  else
    dnf;
  end if;
end method simplify-dnf;

// ----------------------------------------------------------------------------
//  stuff to manipulate propositions
//  functions defined:
// 
//  cnf (p)			returns conjunctive normal form
//  dnf (p)			disjunctive normal form
//  standardize-operators	removes => <= <=> if iff
//  negate (p)			negates it
//  cnf-to-logic (cnf)		inverts cnf
//  dnf-to-logic (dnf)		inverts dnf
//  normal-form (p)		returns something suitable for stating
//  conjunctive normal form.  Remove the nonstandard operators from p and
//  then look at (car p):
//   1.  If it's NOT, then (cnf p) is the result of negating each term
//   inside (dnf (not (p))).
//   2.  If it's OR, then we have to combine the results of cnf'ing each
//   of the disjuncts.  cnf-merge-lists does this, one disjunct at a
//   time.
//   3.  If it's AND, we just call the cnf'er and nconc the results
//   together.
//   4.  If it's none of these, it must be a term and we return ((p)).
define method cnf (p)
  select (car(setq(p, standardize-operators(p))))
    #"not"
       => napcar(method (l) napcar(negate, l); end method, dnf(second(p)));
    #"or"
       => let ans = #f;
           for (item in tail(p))
             ans := cnf-merge-lists(cnf(item), ans);
           finally
             reverse!(ans);
           end for;
    #"and"
       => apply(concatenate!, map(cnf, tail(p)));
    otherwise
       => list(list(p));
  end select;
end method cnf;

//  given a partial cnf expression d, and a new cnf exp c to be merged,
//  construct a list of all entries that have an entry from d followed by
//  one from c.  As we go through, the list is maintained backwards, so
//  that we have terms with entries late in d early in the returned
//  answer.  Three cases:
//   1.  If c is NIL, there is nothing to do.  Return d.
//   2.  If d is NIL, there is still nothing to do, but we reverse c to
//   get d into "backwards" form.
//   3.  Otherwise, work through c and for each entry in it, work through
//   d, appending each entry of d onto that entry of c and pushing the
//   result onto the answer being accumulated.  Note that this maintains
//   the "backwardness" of the answer.  We make sure that each term in
//   the final answer is a fresh copy.
define method cnf-merge-lists (c, d)
  if (empty?(c))
    d;
  elseif (empty?(d))
    reverse!(c);
  else
    begin
      let ans = #f;
      for (item in c)
        push!(concatenate(head(d), item), ans);
        for (x in tail(d))
          push!(concatenate(x, copy-sequence(item)), ans);
        end for;
      finally
        ans;
      end for;
    end;
  end if;
end method cnf-merge-lists;

//  remove nonstandard connectives from p.  Handles =>, <=, <=>, if and
//  iff.
//   1.  (=> p1 ... pn q) means (if (and p1 ... pn) q), or
//   (or (not (and p1 ... pn)) q).
//   2.  (<= q p1 ... pn) means (if (and p1 ... pn) q) as well.
//   3.  (if p q) means (or (not p) q)
//   4.  (iff p q) means (or (and p q) (and (not p) (not q))); <=> is
//   similar.
define method standardize-operators (p)
  select (car(p))
    #"=>"
       => list(#"or",
               list(#"not",
                    pair(#"and",
                         begin
                           let l93820 = tail(p);
                           copy-sequence(l93820, size(l93820) - 1);
                         end)),
               head(copy-sequence(p, start: size(p) - 1)));
    #"<="
       => list(#"or", second(p), list(#"not", pair(#"and", tail(tail(p)))));
    #"if"
       => list(#"or", third(p), list(#"not", second(p)));
    (#"<=>", #"iff")
       => list(#"or", list(#"and", second(p), third(p)),
               list(#"and", negate(second(p)), negate(third(p))));
    otherwise
       => p;
  end select;
end method standardize-operators;

//  dnf is a lot like cnf.
define method dnf (p)
  select (car(setq(p, standardize-operators(p))))
    #"not"
       => napcar(method (l) napcar(negate, l); end method, cnf(second(p)));
    #"and"
       => let ans = #f;
           for (item in tail(p))
             ans := cnf-merge-lists(dnf(item), ans);
           finally
             reverse!(ans);
           end for;
    #"or"
       => apply(concatenate!, map(dnf, tail(p)));
    otherwise
       => list(list(p));
  end select;
end method dnf;

//  negate a sentence.  If it begins with not, return what's left.
//  Otherwise put a not on the front and return that.
define method negate (p)
  if (#"not" == head(p)) second(p); else list(#"not", p); end if;
end method negate;

//  utility functions to turn a cnf expression, or a conjunct/disjunct,
//  into a logical sentence.  They're all simple -- if the thing is a
//  list, push "and" or "or" on the front and proceed.  Otherwise, just
//  return it.
define method cnf-to-logic (cnf)
  conj-to-logic(map(disj-to-logic, cnf));
end method cnf-to-logic;

define method dnf-to-logic (dnf)
  disj-to-logic(map(conj-to-logic, dnf));
end method dnf-to-logic;

define method conj-to-logic (list)
  if (tail(list)) pair(#"and", list); else head(list); end if;
end method conj-to-logic;

define method disj-to-logic (list)
  if (tail(list)) pair(#"or", list); else head(list); end if;
end method disj-to-logic;

//  *if-translation* controls how if is converted.  If bc (the default),
//  two backward-chaining versions are created.  If fc, two
//  forward-chaining versions.  If mix, (if a b) translates to (<= b a)
//  and (=> a b).
define variable *if-translation* = #"bc";

//  *equivalence-translation* controls how <=> is converted.  If bc (the
//  default), two backward-chaining versions are created.  If fc, two
//  forward-chaining versions.  If mix, (<=> a b) translates to (<= a b)
//  and (=> a b).
define variable *equivalence-translation* = #"bc";

//  normal form.  Depends on (car p):
//   => invokes nf-forward
//   <= invokes nf-backward
//   if is treated by examining *if-translation*
//   <=>is treated by examining *equivalence-translation*
//   (or p1 ... pn) is turned into (<= p1 (not (and p2 ... pn)))
//   (iff a b) is (and (if a b) (if b a))
//   everything else is treated by calling the cnf'er and then calling
//   disj-to-logic on each term
define method normal-form (p)
  select (car(p))
    #"=>"
       => nf-forward(p);
    #"<="
       => nf-backward(p);
    #"if"
       => select (*if-translation*)
            #"bc"
               => concatenate!(nf-backward(p),
                               nf-backward(list(#"if",
                                                list(#"not", third(p)),
                                                list(#"not", second(p)))));
            #"fc"
               => concatenate!(nf-forward(p),
                               nf-forward(list(#"if",
                                               list(#"not", third(p)),
                                               list(#"not", second(p)))));
            #"mix"
               => concatenate!(nf-backward(p), nf-forward(p));
            otherwise
               => #f;
          end select;
    #"<=>"
       => select (*equivalence-translation*)
            #"bc"
               => concatenate!(nf-backward(pair(#"<=", tail(p))),
                               nf-backward(list(#"<=", third(p), second(p))));
            #"fc"
               => concatenate!(nf-forward(pair(#"=>", tail(p))),
                               nf-forward(list(#"=>", third(p), second(p))));
            #"mix"
               => concatenate!(nf-backward(pair(#"<=", tail(p))),
                               nf-forward(pair(#"=>", tail(p))));
            otherwise
               => #f;
          end select;
    #"or"
       => if (tail(tail(p)))
            normal-form(list(#"<=", second(p),
                             list(#"not", pair(#"and", tail(tail(p))))));
          else
            normal-form(second(p));
          end if;
    #"iff"
       => concatenate!(normal-form(pair(#"if", tail(p))),
                       normal-form(list(#"if", third(p), second(p))));
    #"and"
       => apply(concatenate!, map(normal-form, tail(p)));
    otherwise
       => napcar(disj-to-logic, cnf(p));
  end select;
end method normal-form;

//  normal form for forward-chaining.  Convert to cnf, then for each
//  term (or p1 ... pn), rewrite it as p1 (if n=1) or
//  (=> (not p1) ... (not pn-1) pn)
define method nf-forward (p)
  napcar(method (x)
           if (tail(x))
             pair(#"=>",
                  concatenate!(napcar(negate, copy-sequence(x, size(x) - 1)),
                               list(head(copy-sequence(x,
                                                       start: size(x)
                                                               - 1)))));
           else
             head(x);
           end if;
         end method,
         cnf(p));
end method nf-forward;

//  backward-chaining.  Convert to cnf, then for each term (or p1 ... pn),
//  rewrite it as p1 (if n=1) or (<= p1 (not p2) ... (not pn))
define method nf-backward (p)
  napcar(method (x)
           if (tail(x))
             apply(list, #"<=", head(x), napcar(negate, tail(x)));
           else
             head(x);
           end if;
         end method,
         cnf(p));
end method nf-backward;

