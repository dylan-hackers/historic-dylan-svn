//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
//  This file was stolen from Matt Ginsberg's MVL 5/31/92
"(in-package dtp)";

define method first-binding-list (list-of-bl)
  if (first(list-of-bl))
    first(list-of-bl);
  elseif (instance?(list-of-bl, <pair>))
    #(#(#"t" . #"t"));
  else
    #f;
  end if;
end method first-binding-list;

define variable unify-dummies = #f;

//  I comment out the original below
define method dtp-instp (sexp1, sexp2, #key old-bl = #f)
  first-binding-list(remove(map(subst-bdgs,
                                map(method (a)
                                      map(method (x)
                                          pair(head(x), tail(x));
                                          end method,
                                          a);
                                    end method,
                                    inst-1(sexp1, sexp2, list(old-bl), #f))),
                            #"fail"));
end method dtp-instp;

define method dtp-unifyp (sexp1, sexp2)
  first-binding-list(unifyp(sexp1, sexp2));
end method dtp-unifyp;

define method dtp-samep-binding-list (sexp1, sexp2)
  let bls = samep(sexp1, sexp2, #t);
  if (first(bls))
    first(bls);
    //  Only one binding list matters
    elseif (instance?(bls, <pair>))
    #f;
    //  Success, with no binding required
    else
    #"not-a-binding-list";
  end if;
end method dtp-samep-binding-list;

// ----------------------------------------------------------------------------
//  This file contains matching functions that handle sequence (*)
//  variables.
// 
//  Pre-unify --  determines whether two expressions unify ignoring
//                possible variable binding conflicts.  Returns t or nil
//  Samep     --  determines whether two expressions are the same
//     	         modulo variable renaming.  Returns t or nil.  (Returns
//     	         a binding list if one is needed.)
//  Instp     --  determines whether one expression is an instance
//      		 of another.  Returns a list of binding lists.
//  Unifyp    --  determines whether two expressions unify.
// 		 Returns a list of binding lists.
// 
//  The truth table for these functions is:
//  
//            Nil  List  Const  ?   *
//          ---------------------------
//     Nil  |  T    F      F    F   LR
//     List |  F   Mtch    F    T   LR
//     Const|  F    F      =    T   LR		LR= Lop and recur
//     ?    |  F    T      T    T   LR
//     *    |  LR   LR     LR   LR  LR     
// 
//  pre-unify is the simplest of the matching functions.  Note that this
//  function expects two lists as arguments.  Walk down each expression, doing:
//   1.  If they are eq (presumably both NIL), succeed.
//   2.  If the car's are equal, keep walking.
//   3.  If the car's are both lists, make sure they pre-unify and then
//   keep walking.
//   4.  If the car of p is a sequence variable, call pre-unify*var, i.e.:
//    4a. If the cdr of p is NIL, then you're done since p matches all of q
//    4b. If the cdr of p matches any tail of q, you're also done
//   5.  If the car of p is a variable, then:
//    5a. If the car of q is a sequence variable, call pre-unify*var
//    5b. Otherwise, keep walking
//   6.  If the car of p is not a variable, then check out q and behave
//   similarly, except if the car of q isn't a variable either, return
//   failure.
define method pre-unify (p, q)
  block (return)
    while (#t)
      let _that = #f;
      if (p == q)
        return(#t);
      elseif (_that := head(p) == head(q))
        _that;
      elseif (instance?(head(p), <list>) & instance?(head(q), <list>))
        if (~ pre-unify(head(p), head(q))) return(#f); end if;
      else
        select (vartype(car(p)))
          #"?*"
             => return(pre-unify*var(p, q));
          #"?"
             => if (varp*(head(q))) return(pre-unify*var(q, p)); end if;
          otherwise
             => select (vartype(car(q)))
                  #"?*"
                     => return(pre-unify*var(q, p));
                  #"?"
                     => #f;
                  otherwise
                     => return(#f);
                end select;
        end select;
      end if;
      begin p := tail(p); q := tail(q); end;
    end while;
  end block;
end method pre-unify;

define method pre-unify*var (p, q)
  empty?((p := tail(p)))
   | block (return)
       while (#t)
         if (pre-unify(p, q)) return(#t); end if;
         if (~ q) return(#f); end if;
         (q := tail(q));
       end while;
     end block;
end method pre-unify*var;

//  Simple matcher that determines whether or not two expressions are the
//  same up to variable renaming.  Keeps variable equivalences in the
//  special binding list blist.  This function accepts an optional
//  argument that indicates that T or NIL is not good enough as an answer
//  -- you want the binding list created.
define method samep (p, q, #key return-answer?)
  if (samep1(p, q))
    empty?(return-answer?)
     | // knowing they're the same is good enough
    samep-answer(blist);
  end if;
end method samep;

//  Take the binding list produced by samep1, and make it suitable to be
//  returned.  There are two things that need to be done:
//   1.  Any variable bound to itself should be pruned.
//   2.  Any sequence variable is in fact bound not to a list, but to the
//   value it represents (it can't be bound to a list of values -- this
//   is samep, not unifyp).  So we have to bind it to the *list*
//   containing this value.
define method samep-answer (blist)
  list(choose(complement(method (x)
                           let _that = #f;
                           if (_that := head(x) == tail(x))
                             _that;
                             // x bound to x
                             elseif (varp*(head(x)))
                             tail(x) := list(tail(x));
                             #f;
                           end if;
                         end method),
              // if x is * var, adjust
              // its binding and then
              // don't remove it
              blist));
end method samep-answer;

//  The algorithm should be familiar -- walk down the two lists,
//  accumulating a binding list.  If p is an atom (this includes NIL),
//  then check to see if q has the same variable type as p (in which case
//  bind them to each other if possible); if neither is a variable, make
//  sure they are equal.  If only one is an atom, fail.  If neither is an
//  atom, recur.
define method samep1 (p, q)
  if (not(instance?(p, <list>)))
    //  catches nil also
    select (vartype(p))
      #"?"
         => varp?(q) & samep-var(p, q);
      #"?*"
         => varp*(q) & samep-var(p, q);
      otherwise
         => p == q;
    end select;
  elseif (not(instance?(q, <list>)))
    #f;
  elseif (samep1(head(p), head(q)))
    samep1(tail(p), tail(q));
  end if;
end method samep1;

//  samep-var checks to see if p can be bound to q by seeing if either p
//  or q has already been bound.  If so, then the binding had better be
//  the same; if not, stick a new cons cell describing the binding onto
//  the binding list being accumulated.  It's ok to use eq instead of eql
//  because p and q are both symbols.
define method samep-var (p, q)
  if (bdg := cl-assoc(p, blist))
    tail(bdg) == q;
  elseif (bdg
           := // LTD: Function RASSOC not yet implemented.
              rassoc(q, blist))
    p == head(bdg);
  else
    push!(pair(p, q), blist);
  end if;
end method samep-var;

//  One sided unifier that determines whether q is an instance of p.
//  Like UNIFY, passes around a set of binding lists to allow for the
//  possibility that multiple answers can result from the presence of *
//  variables.  The parameter rflg is used to indicate whether or not the
//  lists are reversed.  reversal is done for efficiency whenever a
//  non-terminal * variable is encountered.  Ignores atomic cdrs; i.e.
//  treats them as if they were nil.  A list of binding lists is returned.
define method instp (p, q) inst-1(p, q, list(#f), #f); end method instp;

//  the work is actually done here.  p and q can be assumed to be lists.
//  If q is NIL, then in order for it to be an instance of p, (car p)
//  must be a sequence variable, which is then bound to NIL and NIL must
//  be an instance of (cdr p) as well.
//  In general, if (car p) is a cons, we basically need to recur,
//  checking first that (car q) is also a cons and then doing the
//  recursion.  If (car p) is an atom, then:
//   1.  If it is a sequence variable, call inst*var
//   2.  If it is a normal variable, then provided that (car q) is not a
//   sequence variable (in which case we fail), bind (car p) to (car q)
//   and continue.
//   3.  If it is not a variable, but is eql to (car q), simply recur.
define method inst-1 (p, q, blists, rflg)
  if (empty?(p))
    if (~ q) blists; end if;
    // to catch p and q both NIL
    elseif (empty?(q))
    varp*(head(p)) & (blists := inst?var(head(p), #f, blists))
     & inst-1(tail(p), #f, blists, rflg);
  elseif (instance?(head(p), <pair>))
    instance?(head(q), <list>)
     & // to catch (car q) nil
    (blists := inst-1(head(p), head(q), blists, #f))
     & inst-1(tail(p), tail(q), blists, rflg);
  else
    select (vartype(car(p)))
      #"?*"
         => inst*var(p, q, blists, rflg);
      #"?"
         => if (~ varp*(head(q))
                 & (blists := inst?var(head(p), head(q), blists)))
              inst-1(tail(p), tail(q), blists, rflg);
            end if;
      otherwise
         => head(p) == head(q) & inst-1(tail(p), tail(q), blists, rflg);
    end select;
  end if;
end method inst-1;

//  Handles the unification for star variables.  The first element of p
//  is guaranteed to be a * variable.  There are the following three
//  cases:
//   1.  If (cdr p) is NIL, then the given * variable is all there is, so
//   just call inst?var to do the actual unification.
//   2.  If rflg is NIL, then this is the first * variable, and you can
//   get away with simply reversing the two lists and trying again.
//   3.  If rflg is T, then you have to walk down q, trying inst at each
//   point.  This is done in a do loop, where qhead is the stuff being
//   matched to (car p) and q contains what's left in q.  For each point
//   in q, we call inst-1 to try to match the rest of p with the rest of
//   q and if this succeeds, try to add the variable binding for (car p)
//   to the result.
//  Note that qhead in the following routine is apparently reversed from its
//  true value.  In fact, this is as it should be, since the lists themselves
//  were presumably reversed when the first * variable was encountered.
// LTD: No macros.
#"pushconc";

define method inst*var (p, q, blists, rflg)
  if (empty?(tail(p)))
    inst?var(head(p), if (rflg) reverse(q); else q; end if, blists);
  elseif (rflg)
    block (return)
      for (newblists = nil then nil, qhead = nil then nil, until %f)
        pushconc(inst?var(head(p), qhead, inst-1(tail(p), q, blists, #t)),
                 newblists);
        if (~ q) return(newblists); end if;
        push!(pop!(q), qhead);
      end for;
    end block;
  else
    inst-1(reverse(p), reverse(q), blists, #t);
  end if;
end method inst*var;

//  inst?var actually adds a new variable binding to the binding lists
//  being accumulated.  The variable is in var, and the binding is q.
//  The binding lists that have been collected so far are in blists.
//  If the variable has already been bound, then the binding had better
//  be the same.  If it hasn't been bound, just push the binding onto the
//  accumulated list.
define method inst?var (var, q, blists)
  for (blist in blists)
    bdg := cl-assoc(var, blist);
    if (empty?(bdg))
      push!(cons(cons(var, q), blist), newblists);
    elseif (tail(bdg) = q)
      push!(blist, newblists);
    end if;
  finally
    newblists;
  end for;
end method inst?var;

//  General unification function and matcher.  Both call unify to do the
//  unification, then call subst-bdgs to fully instantiate the binding
//  lists and to check for any loops.  MATCHP is slightly different from
//  UNIFYP in that it must make sure new variables are created for any
//  database variables that end up free in the binding list.  It does
//  this by adding extra binding pairs for database variables to the
//  binding list before substitution.  After substitution MATCHP returns
//  a list of conses, the cars of which are the bindings for the
//  variables that appear in the query, and the cdrs of which are the
//  bindings generated for database variables.
//  In order that these functions be as efficient as possible, they
//  attempt to reuse as many conses as possible.  To make the code clear
//  while this is going on, these macros have been defined for pushing
//  and popping cells off of lists in ways that allow the conses to be
//  reused:
// LTD: No macros.
#"popcell";

// LTD: No macros.
#"pushcell";

//  The unifier works by first calling unify, which does the basic
//  unification but may return binding lists with loops or incomplete
//  bindings (e.g., x is bound to y and y is then bound to z).  In order
//  to deal with these possibilities, we work through the list of binding
//  lists returned, calling subst-bdgs on each one to simplify it.  Since
//  subst-bdgs is destructive, we have to copy the binding list unless
//  it's the last one we need to do (in which case we don't mind
//  destroying it).
//  subst-bdgs returns FAIL if there is an internal conflict in the
//  binding list.  Thus we first check to see if the binding is empty (in
//  which case we just push it on to the list unchanged); if it isn't we
//  try subst-bdgs.  Then we just remove any FAILs from the result.
define variable unify*flg = #f;

// binding lists must be copied
//  (defvar unify-dummies)			;list of dummy variables
//  ^^^^^^^^^^^^^^^^^^^^^^ commented out 5/18/93 by Don Geddis (moved to top)
define variable nasty-unify = #f;

// variables may be in wrong order?
define method unifyp (p, q)
  if (head(blists))
    napcar(method (a)
             map(method (x) pair(head(x), tail(x)); end method, a);
           end method,
           tail(blists));
    napcar(subst-bdgs, blists);
    remove!(blists, #"fail");
  else
    blists;
  end if;
end method unifyp;

//  the matcher is much the same, but matches an expression to a database
//  sentence (which is referenced by its proposition symbol).  There are
//  a couple of additional subtleties, though:
//  1.  We want to bind the database variables and not the variables in
//  the query wherever possible.  check-order is responsible for fiddling
//  with the binding lists to make this happen; nasty-unify is a flag
//  indicating that we couldn't get by without it being a possibility in
//  the unification phase.
//  2.  We don't want to have things bound to the uninterned symbols in
//  the database sentence, so we next bind each database variable to a new
//  variable.  We simplify as for unify, and then split the bindings into 
//  those appearing in the query and those appearing in the database.
define method matchp (exp, datum)
  if (head(blists))
    if (nasty-unify)
      begin
        do(method (blist)
             napcar(method (b) check-order(b, blist); end method, blist);
           end method,
           blists);
        blists;
      end;
    end if;
    begin
      let db = symbol-get-property(datum, #"vars-in");
      napcar(method (a)
               map(method (x) pair(head(x), tail(x)); end method, a);
             end method,
             tail(blists));
      napcar(method (b) match-db-vars(b, db); end method, blists);
      napcar(divide-bdgs, remove!(blists, #"fail"));
    end;
  else
    blists;
  end if;
end method matchp;

//  Here we check the order of the variables appearing in a particular
//  binding list.  If either the car is a database variable or the cdr
//  is bound, we don't have to worry.  (If the cdr is bound, the binding
//  will be simplified away.)  Now there are two cases:
//  1.  If the car is a *var, and is bound to a list consisting of a
//      *var alone, and the second *var is a database variable, we have
//      to swap them.
//  2.  If the car is a normal var, it's much the same.
define method check-order (bdg, blist)
  if (database-variable(head(bdg)) | cl-assoc(tail(bdg), blist))
    bdg;
  elseif (varp?(head(bdg)))
    if (instance?(tail(bdg), <symbol>) & database-variable(tail(bdg)))
      pair(tail(bdg), head(bdg));
    else
      bdg;
    end if;
  elseif (instance?(second(bdg), <symbol>) & database-variable(second(bdg)))
    list(second(bdg), head(bdg));
  else
    bdg;
  end if;
end method check-order;

//  Here we replace the database variables with new variables.  Any unbound
//  database variable (there is a list in vars) gets bound to a new variable
//  before we call subst-bdgs to sort it all out.
define method match-db-vars (bdgs, vars)
  for (var in vars)
    if (~ cl-assoc(var, bdgs))
      push!(if (varp*(var))
              list(var, new-*var());
            else
              pair(var, new-?var());
            end if,
            bdgs);
    end if;
  finally
    subst-bdgs(bdgs);
  end for;
end method match-db-vars;

//  And here we split the binding list into a database set and a query set.
//  We do it without consing up a whole bunch of new cells.
define method divide-bdgs (bdgs)
  for (bdg = nil then nil, query = nil then nil, database = nil then nil,
       until empty?(bdgs))
    bdg := popcell(bdgs);
    if (database-variable(head(head(bdg))))
      pushcell(bdg, database);
    else
      pushcell(bdg, query);
    end if;
  finally
    pair(query, database);
  end for;
end method divide-bdgs;

//  Unifier that handles star variables.  Passes around a set of binding
//  lists to allow for the possibility that multiple answers can result
//  from the presence of * variables.  The parameter rflg is used to
//  indicate whether or not the lists are reversed.  reversal is done
//  for efficiency whenever a non-terminal * variable is encountered.
//  Note:  requires post pass to eliminate all variable loops and to
//  simplify variable bindings.  There is one other catch -- when possible,
//  we would like to bind the variables in p and not those in q.  This
//  functionality is needed by the matcher.  The special variable
//  nasty-unify is used to record the fact that we may have failed to
//  do this.
define method unify (p, q)
  if (not(instance?(p, <list>)) | not(instance?(q, <list>)))
    p := list(p);
    q := list(q);
  end if;
  unify-1(p, q, list(#f), #f);
end method unify;

//  Here we go.
//   1.  If p and q are eq, you're done.
//   2.  If p is NIL, then q needs to be a list starting with a *
//   variable, in which case we try to unify (car q) with NIL and (cdr q)
//   with NIL if that succeeds.
//   3.  If q is NIL, it's much the same.
//   4.  If the car's are eql, we unify the cdrs.
//   5.  If the car's are both lists, we unify them and then the cdrs.
//   6.  If the car of p is a variable, then:
//    6a.  If the car of q is a sequence variable, we call unify*var
//    6b.  Otherwise, we just try to set (car p) to (car q) and continue
//   7.  If the car of p is a sequence variable, we call unify*var
//   8.  If the car of p is not a variable, then if the car of q is a
//    variable of any type, we treat it as in 6,7
define method unify-1 (p, q, blists, rflg)
  if (p == q)
    blists;
  elseif (empty?(p))
    varp*(head(q)) & (blists := unify?var(head(q), #f, blists))
     & unify-1(tail(q), #f, blists, rflg);
  elseif (empty?(q))
    varp*(head(p)) & (blists := unify?var(head(p), #f, blists))
     & unify-1(tail(p), #f, blists, rflg);
  elseif (head(p) == head(q))
    unify-1(tail(p), tail(q), blists, rflg);
  elseif (instance?(head(p), <list>) & instance?(head(q), <list>))
    (blists := unify-1(head(p), head(q), blists, #f))
     & unify-1(tail(p), tail(q), blists, rflg);
  else
    select (vartype(car(p)))
      #"?"
         => if (varp*(head(q)))
              nasty-unify := #t;
              unify*var(q, p, blists, rflg);
            elseif (blists := unify?var(head(p), head(q), blists))
              unify-1(tail(p), tail(q), blists, rflg);
            end if;
      #"?*"
         => unify*var(p, q, blists, rflg);
      otherwise
         => select (vartype(car(q)))
              #"?"
                 => if (blists := unify?var(head(q), head(p), blists))
                      nasty-unify := #t;
                      unify-1(tail(p), tail(q), blists, rflg);
                    end if;
              #"?*"
                 => nasty-unify := #t; unify*var(q, p, blists, rflg);
              otherwise
                 => #f;
            end select;
    end select;
  end if;
end method unify-1;

//  Handles the unification for star variables.  The first element of p
//  is guaranteed to be a * variable.  The situation is complicated by
//  the fact that if the variable is already bound, we want to plug for
//  it here and not risk the looping that happens when two * variables
//  meet.  So we have to do the analysis for each binding list in blists.
//  There are the following four cases:
//   1.  If (cdr p) is NIL, then the given * variable is all there is, so
//   just call unify?var to do the actual unification.
//   2.  If (car q) is a * var and (cdr q) is NIL, we treat it like (1).
//   3.  If rflg is NIL, then this is the first * variable, and we can
//   get away with simply reversing the two lists and trying again.
//   4.  Otherwise we look through the blists to bind p if possible:
//     4a.  If p is bound, then we stick its binding onto the cdr
//   	   of p and repeat the call.
//     4b.  If q does not begin with a * variable, things are somewhat 
//          simpler; we have to walk down q, trying unify at each point.
//          This is done by unify*loop.
//     4c.  If q is bound, we plug in the binding as in case 1.
//     4d.  The last case is nasty and unify-** is responsible for sorting
//          it out.
define method unify*var (p, q, blists, rflg)
  if (empty?(tail(p)))
    unify?var(head(p), if (rflg) reverse(q); else q; end if, blists);
  elseif ((qstar := varp*(head(q))) & empty?(tail(q)))
    nasty-unify := #t;
    unify?var(head(q), if (rflg) reverse(p); else p; end if, blists);
  elseif (rflg)
    unify*flg := #t;
    for (newblists = nil then nil, until empty?(blists))
      let blist = popcell(blists);
      let bdg = find*var(head(p), blist);
      pushconc(if (bdg)
                 unify-1(concatenate(reverse(tail(bdg)), tail(p)), q, blist,
                         #t);
               elseif (qstar)
                 begin
                   let qbdg = find*var(head(q), blist);
                   if (qbdg)
                     unify-1(p, concatenate(reverse(tail(qbdg)), tail(q)),
                             blist, rflg);
                   else
                     unify-**(p, q, blist);
                   end if;
                 end;
               else
                 unify*loop(p, #f, q, blist);
               end if,
               newblists);
    finally
      newblists;
    end for;
  else
    unify-1(reverse(p), reverse(q), blists, #t);
  end if;
end method unify*var;

//  Here we handle the loop where p begins with a * variable and q
//  doesn't.  qhead is the reverse of the stuff being matched to (car p)
//  and qtail is what's left in q.
//  It's pretty simple, except if q contains a * variable itself.  This
//  case is handled after the * variable is pushed onto qhead, so if it's
//  on qtail we don't have to do anything.  In the "simple" case where qtail
//  begins with something other than a * variable, we just call unify-1
//  to unify qtail with the cdr of p and bind the * variable beginning p
//  to all of qhead.  (If there is a * variable at the beginning of the
//  reversed qhead, this operation is a special case of the first clause
//  of the main cond below.)
//  To handle the case where there is a * variable in q, we have to 
//  allow for the possibility that (car p) only unifies with a *part* 
//  of this * variable (toegether with all of qhead, of course).  This 
//  is done by splitting the * variable into two new variables, and 
//  assuming that (car p) only includes the first.
//  There are two additional subtleties.  First, the new * variables created
//  are just dummies, and we want to be able to get rid of them at the end of
//  the unification process if possible.  So instead of invoking
//  (new-*var) to create them, we invoke (new-dummy-var), which records
//  their dummy status.
//  Second, we never "resplit" a * variable.  This is not complete, but
//  otherwise the unifier may loop in a variety of situations (which was
//  felt to be worse).
define method unify*loop (p, qhead, qtail, blists)
  block (return)
    while (#t)
      if (member?(head(qhead), unify-dummies))
        #f;
        // don't resplit *var
        elseif (varp*(head(qhead)))
        pushconc(begin
                   let v1 = new-dummy-var();
                   let v2 = new-dummy-var();
                   unify-1(tail(p), pair(v1, qtail),
                           unify?var(head(p), pair(v2, tail(qhead)),
                                     unify?var(head(qhead),
                                               list(v1, v2),
                                               copy-sequence(blists))),
                           #t);
                 end,
                 newblists);
      elseif (~ varp*(head(qtail)))
        pushconc(unify-1(tail(p), qtail,
                         unify?var(head(p), qhead, copy-sequence(blists)),
                         #t),
                 newblists);
      end if;
      if (~ qtail) return(newblists); end if;
      push!(pop!(qtail), qhead);
    end while;
  end block;
end method unify*loop;

//  p and q both begin with * variables.  Now there are two
//  possibilities:
//   1.  (car p) unifies with (car q) and perhaps a little more
//   2.  (car q) unifies with (car p) and a little more
//  Of course, by "a little more", we mean some portion of the tail of
//  the list being considered -- which may mean a *part* of some
//  subsequent * variable.  Thus if ?*x is unifying with the car of
//  (?*y ?*z) and a little more, it might unify with ?*y and some *part*
//  of ?*z and not the whole thing.
//  To handle this (admittedly bizarre) case, when we decide to unify
//  with a "little more" and this little more ends in a * variable, we
//  split that * variable into two parts, including the first in the
//  little more and leaving the rest to unify with the remainder of the
//  expression.  But now note that since the first of these parts can
//  unify with nil (in which case it is just the same as if the *
//  variable wasn't included in the "little more" after all), there is no
//  point in ending the "little more" just before a sequence variable.
define method unify-** (p, q, blists)
  concatenate!(unify-1(tail(p), tail(q),
                       unify?var(head(p), list(head(q)),
                                 copy-sequence(blists)),
                       #t),
               unify-**-1(p, q, copy-sequence(blists)),
               unify-**-1(q, p, copy-sequence(blists)));
end method unify-**;

//  here is the basic function; (car p) is assumed to unify with (car q)
//  and a little more.  But this is just what unify*loop does; we have to
//  be sure to move the first two elements of q to qhead and leave the
//  rest as qtail.
define method unify-**-1 (p, q, blists)
  if (tail(q))
    unify*loop(p, list(second(q), first(q)), tail(tail(q)), blists);
  end if;
end method unify-**-1;

define method new-dummy-var ()
  push!(var, unify-dummies);
  var;
end method new-dummy-var;

//  modify a collection of binding lists to indicate that the variable
//  var is bound to the value q.  If unify*flg is T, then blists cannot
//  be destroyed in the process.
//  We proceed through blists one at a time.  For each blist, we check to
//  see if var is already bound; find?var returns q if it isn't and the
//  existing binding if it is.  There are therefore the following cases:
//   1.  If the value returned is q (for whatever reason), then
//   everything is fine and we just pass this blist off as done.
//   2.  If the value returned is "badloopcheck", then we have
//   essentially hit an occurcheck problem, in that the variable is
//   already being investigated by a higher-level call to unify?var.  In
//   this case, we give up.
//   3.  If both the current and desired values are lists, then we have
//   to try to unify them.  We mark the variable under consideration as
//   "badloopcheck" (see 2 above), and invoke the unifier recursively.
//   Then we reset the old value to which the variable is bound, and add
//   the result of the unification call to the growing list of new
//   answers.  (We do these two steps in the reverse order.)
//   4.  If the desired value is a variable, then we basically do things
//   in the reverse order, finding the value to which *q* is bound.  This
//   leads to:
//    4a.  If q is bound (for whatever reason) to the desired value,
//    proceed with success.
//    4b.  If q is bound to badloopcheck by a higher-level unification
//    call, give up.  (This happens by default.)
//    4c.  If both var's value and q's value are lists, try to unify
//    them as in (3).
define method unify?var (var, q, blists)
  for (blist = nil then nil, bdg = nil then nil, val = nil then nil,
       valq = nil then nil, newblists = nil then nil, until empty?(blists))
    blist := popcell(blists);
    // strip off first blist
    begin
      bdg
       := if (unify*flg)
            find?var(var, q, blist);
          else
            nfind?var(var, q, blist);
          end if;
      val := tail(bdg);
    end;
    let _that = #f;
    if (val == q)
      pushcell(blist, newblists);
    elseif (_that := val == #"badloopcheck")
      _that;
    elseif (instance?(val, <pair>) & instance?(q, <pair>))
      tail(bdg) := #"badloopcheck";
      pushconc(unify-1(val, q, blist, #f), newblists);
      tail(bdg) := val;
    elseif (varp(q))
      begin
        bdg
         := if (unify*flg)
              find?var(q, val, blist);
            else
              nfind?var(q, val, blist);
            end if;
        valq := tail(bdg);
      end;
      if (val == valq)
        pushcell(blist, newblists);
      elseif (instance?(val, <pair>) & instance?(valq, <pair>))
        tail(bdg) := #"badloopcheck";
        pushconc(unify-1(val, valq, blist, #f), newblists);
        tail(bdg) := valq;
      end if;
    end if;
  finally
    newblists;
  end for;
end method unify?var;

//  Find the value to which a variable is bound.  The binding list
//  examined is (car blist).
//  We first look for var's binding.  If it has none, we add a dotted
//  pair (var . q) giving the desired binding to the front of the
//  binding list, and return that.
//  If var is bound to a variable, then we basically just recur, trying
//  to find the binding for *that* variable.  The only catch is that if
//  we encounter the given variable again, we should just note that
//  we've hit a loop and bind either one of the two variables to the
//  desired value.  In practice, the variable var is bound to q.
//  If var is bound to something other than a variable, we just return
//  that.
//  There are two routines here, one for * variables (that returns nil if
//  the * variable is unbound, and the binding otherwise), and one for
//  normal variables, that successfully binds the variable to the given
//  value if there is no other binding.
define method find*var (var, blist)
  if (empty?(bdg))
    #f;
    // failure -- no existing binding
    elseif (instance?(val, <pair>) & empty?(tail(val)) & varp*(head(val)))
    // another * variable?
    begin
      tail(bdg) := #"varloopcheck";
      let _ = find*var(head(val), blist) | bdg;
      tail(bdg) := val;
      _;
    end;
  elseif (val == #"varloopcheck")
    // variable loop -- hit loopcheck
    head(blist) := remove(head(blist), bdg);
    #f;
  else
    bdg;
  end if;
end method find*var;

define method find?var (var, q, blist)
  if (empty?(bdg))
    // success -- no existing binding
    head(push!(pair(var, q), head(blist)));
  elseif (instance?(val, <pair>))
    bdg;
    // existing binding to a list
    elseif (q == val)
    bdg;
    // existing binding to same value
    elseif (val == #"varloopcheck")
    // variable loop -- remove current
    // binding for var and set up a new one
    head(blist) := remove(head(blist), bdg);
    head(push!(pair(head(bdg), q), head(blist)));
  elseif (varp(val))
    // bound to a variable but no loop yet
    begin
      tail(bdg) := #"varloopcheck";
      let _ = find?var(val, q, blist);
      tail(bdg) := val;
      _;
    end;
  else
    bdg;
  end if;
end method find?var;

// bound to a constant -- return it
//  Destructive version of the above routine.  As it unwinds, nfind?var
//  destructively substitutes the final value into any intermediate
//  variables.
define method nfind?var (var, q, blist)
  if (empty?(bdg))
    head(push!(pair(var, q), head(blist)));
  elseif (instance?(val, <pair>))
    bdg;
  elseif (q == val)
    bdg;
  elseif (val == #"varloopcheck")
    // bind it destructively
    tail(bdg) := q;
  elseif (varp(val))
    tail(bdg) := #"varloopcheck";
    val := nfind?var(val, q, blist);
    if (bdg == val)
      val;
    elseif (not(instance?(tail(val), <list>)))
      tail(bdg) := tail(val);
      // bind intermediates
      else
      tail(bdg) := head(val);
      val;
    end if;
  else
    bdg;
  end if;
end method nfind?var;

//  Functions that check for loops in the binding list and do complete
//  binding list substitution.  Returns FAIL if the check fails.
//  Otherwise, returns the substituted binding list.
//  This routine works by working down the given binding list, one
//  binding at a time.  A list of equivalent variables is maintained in
//  equivalent-bdgs; a list of the variables that have been accumulated
//  so far is kept in finished-bdgs (so finished-bdgs is returned at the
//  end, after removing any bindings to dummy variables).
define method subst-bdgs (working-bdgs)
  block (no-unify)
    for (temp = nil then nil, until empty?(working-bdgs))
      temp := popcell(working-bdgs);
      // chop a cell from working-bdgs
      subst-bdg(temp);
    finally
      remove-dummy-bindings(finished-bdgs);
    end for;
  end block;
end method subst-bdgs;

//  Process a single binding.  There are five possibilities:
//   1. If the variable is being bound to another variable (this will
//   only happen with regular variables; * variables will always be bound
//   to lists), add the variable to equivalent-bdgs and find the value
//   for the variable using subst-var.
//   2. If the variable is bound to an atom (including nil) move the
//   binding to finished-bdgs.
//   3. If the variable is a star variable and is bound to a list
//   consisting of another star variable then treat it the same way as
//   in case 1.
//   4. If the variable is not a * variable and is being bound to a list,
//   then process the given list to see (destructively) what *its*
//   variables are bound to.  This is done by subst-term; when we
//   call subst-term, we mark the given variable to make sure no binding
//   loops occur.
//   5. If the variable is a * variable and is bound to a list, it's a
//   little harder.  We still want to do basically the same thing, but
//   it is possible that the target list includes the given * variable
//   without there being a conflict if the only other things on the target
//   list are * variables that can be bound to NIL; consider unifying
//   (?*1) with (?*1 ?*2).  To handle this, we invoke subst-term *without*
//   pushing the given cell onto the list of finished bindings *or*
//   marking the variable loop.  In the process, the original * variable
//   will have been "forgotten" and therefore not rebound.  There are
//   now three possibilities:
//   5a. If the original * variable does not appear in the tree, it's ok
//   to return it.
//   5b. If it does appear in the tree, then it has to appear at the
//   top level only, and everything else has to be a *var that can be
//   bound to NIL.
//   5c. Otherwise, fail.
//   To make the check in (5a) faster, we do the whole process, keeping
//   a list of *vars that are pending.  Whenever we try to find a binding
//   for a *var and there isn't one (i.e., if it's pending, we just hit a
//   loop), we delete the *var from the list.  Then we can just check to
//   see if it's on the list to check (5a), avoiding the need to walk
//   through the tree.
define method subst-bdg (cell)
  if (varp(val))
    tail(bdg) := #"badloopcheck";
    tail(bdg) := subst-var(val);
    if (~ (head(bdg) == tail(bdg))) pushcell(cell, equivalent-bdgs); end if;
    if (equivalent-bdgs)
      pushconc(equivalent-bdgs, finished-bdgs);
      equivalent-bdgs := #f;
    end if;
  elseif (not(instance?(val, <list>)))
    pushcell(cell, finished-bdgs);
  elseif (empty?(tail(val)) & varp*(head(bdg)) & varp*(head(val)))
    tail(bdg) := #"badloopcheck";
    begin
      let newval = subst-var(head(val));
      tail(bdg) := if (instance?(newval, <list>)) newval; else val; end if;
    end;
    if (~ (head(bdg) == second(bdg))) pushcell(cell, equivalent-bdgs); end if;
    if (equivalent-bdgs)
      pushconc(equivalent-bdgs, finished-bdgs);
      equivalent-bdgs := #f;
    end if;
  elseif (instance?(val, <pair>))
    if (varp*(head(bdg)))
      push!(head(bdg), pending*);
      begin
        let term = subst-term(val);
        if (head(bdg) == head(pending*))
          pop!(pending*);
          pushcell(cell, finished-bdgs);
          tail(bdg) := term;
        else
          subst-bdg-special-case(bdg, term);
        end if;
      end;
    else
      pushcell(cell, finished-bdgs);
      tail(bdg) := #"badloopcheck";
      tail(bdg) := subst-term(val);
    end if;
  end if;
  bdg;
end method subst-bdg;

//  Here is where we handle the cases (5b) and (5c) above.  Since the
//  term to which the binding has been reduced has all the variables
//  substituted in, any remaining variable is free for substitution.
//  So we have to check that every element of the term is a *var,
//  then it's easy.  We bind them all to nil and also bind the *given*
//  *var to nil if it appears multiple times, as in
//  (unifyp '(?*1 ?*2 ?*2) '(?*2)).
define method subst-bdg-special-case (bdg, term)
  if (not(every?(varp*, term)))
    no-unify(#"fail");
  else
    begin
      let nil* = bdg;
      let var = head(bdg);
      let found1 = #f;
      let found2 = #f;
      tail(bdg) := #f;
      for (v in term)
        if (member?(v, nil*))
          if (v == var)
            let _that = #f;
            if (_that := found2)
              _that;
            elseif (found1)
              found2 := #t;
              push!(bdg, finished-bdgs);
            else
              found1 := #t;
            end if;
          end if;
        else
          push!(v, nil*);
          push!(list(v), finished-bdgs);
        end if;
      finally
        if (~ found2) tail(bdg) := list(head(bdg)); end if;
      end for;
    end;
  end if;
end method subst-bdg-special-case;

//  This routine finds the value for the variable var.  If a finished
//  binding is known for var, then either there is a variable loop (in
//  which case we give up), or we return that binding.  Otherwise, we:
//   1. Find the value on the working binding-list
//   2. Splice the binding out of the working binding list
//   3. Call subst-bdg on the bdg to find out the value
//   4. Return the fully substituted value
//  If there is no binding for the variable anywhere and it's a *var,
//  we remove it from the list of pending *vars as described in subst-bdg.
define method subst-var (var)
  if (bdg)
    if (tail(bdg) == #"badloopcheck")
      no-unify(#"fail");
    else
      tail(bdg);
    end if;
  elseif (cell
           := member?(var, working-bdgs,
                      test: method (x, y) x == head(y); end method))
    if (tail(cell))
      // complex version of
      bdg := head(cell);
      // (setq working
      head(cell) := second(cell);
      //   (delete bdg working))
      bdg := (head(tail(cell)) := bdg);
      tail(cell) := tail(tail(cell));
      cell := bdg;
    else
      working-bdgs := copy-sequence(working-bdgs, size(working-bdgs) - 1);
    end if;
    tail(subst-bdg(cell));
  else
    if (varp*(var)) pending* := remove!(pending*, var, count: 1); end if;
    var;
  end if;
end method subst-var;

define method subst-term (term)
  if (term)
    select (vartype(car(term)))
      #"?"
         => reuse-cons(subst-var(head(term)), subst-term(tail(term)), term);
      #"?*"
         => let val = subst-var(head(term));
             if (empty?(val))
               subst-term(tail(term));
             elseif (not(instance?(val, <list>)))
               // no bdg for the var
               reuse-cons(head(term), subst-term(tail(term)), term);
             else
               concatenate(val, subst-term(tail(term)));
             end if;
      otherwise
         => reuse-cons(if (instance?(head(term), <list>))
                         subst-term(head(term));
                       else
                         head(term);
                       end if,
                       subst-term(tail(term)), term);
    end select;
  end if;
end method subst-term;

define method remove-dummy-bindings (bdgs)
  if (unify-dummies)
    choose(complement(compose(method (x)
                                member?(x, unify-dummies);
                              end method,
                              head)),
           bdgs);
  else
    bdgs;
  end if;
end method remove-dummy-bindings;

