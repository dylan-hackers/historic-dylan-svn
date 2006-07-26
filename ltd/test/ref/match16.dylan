//  -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
//  Matching program for pseudo-mathematica(tm) 
//  copyright (c) 1991, Richard J. Fateman
//  version 16 does not include
//  flat+orderless. or Optional.
//  note: I'm equivocating somewhat on the use of 'True and 'False
//  as Boolean values in Mma. Problem: most lisp functions naturally
//  use nil for false, and "anything else" but sometimes just 't
//  for true.  
//  Here's the decision: 
//    Common   Our       WRI's             Meaning
//     Lisp    mma       Mathematica (tm)
//  ----------------------------------
//  non-nil    True       True            Boolean Truth
//     nil     nil        False           Boolean False
//     nil     Null       Null            Null value 
//     nil    (List)     {} (i.e. List[]) Empty List
//  If you type in nil, it is converted to False.  If you display
//  nil, it is printed as False.eat
// LTD: Function PROVIDE not yet implemented.
provide(#"match");

"(in-package mma)";

// LTD: Function REQUIRE not yet implemented.
require("stack1");

//  definition: a patternobject is like an expression but it can
//  have (Pattern ...) or (Blank ..) subexpressions in it.
//  (a) structurally identical matches.
//  (b) (Blank) matches <anything>
//      (Blank foo) matches anything with a car of foo or
//      in the case of a match to an atom x, it will match
//      if (typep x foo) is true, foo is in {Integer, Rational ..?}
//  (c) (Pattern x <patternobject>) matches whatever would
//    otherwise be matched by <patternobject> but  binds the value of
//    patternobject to the name x.  From that time onward during the match
//    x will only match identical items.
//  That is, f[x_,x_] will match f[3,3] but not f[3,4].
//  f[x_,x_^2] will match f[a,a^2].  But not f[3,9]. (sorry).
//  (d)  (BlankSequence ...) or __ inside (non-Orderless) functions. Note:
//   If f is "Flat" then f[a,x_] matches f[a,z,w]
//    with x-> f[z,w]. 
//   If f is "Flat" then f[b,x__] matches f[b,z,w]
//    with x-> (z,w) .  That's Sequence[z,w]. Look it up...
//  (e) (BlankNullSequence ...) or ___ inside (non-Orderless) functions.
//  (f) Orderless functions are matched only if they have a fixed
//  number of arguments.
//  (g) (PatternTest <patternobj> Test) matches whatever <patternobj> would
//  match, but only if (Test <patternobj>) returns lisp t.  Question:
//  perhaps we should ask that (meval (list Test <patternobj>)) be True not t?
//  we keep to t.
//  g[x__,x__] matches g[1,2,3,1,2,3]  with x->(1,2,3)
//  Some Flat functions are Plus, Times.  There are hardly any
//  others.  Plus is also orderless (see below) complicating
//  the matching considerably.  
//  Functions which are both Flat and Orderless functions are
//  not handled by this version.
//  (h)  Condition is handled, partly by rulesetapply and partly here.
//      a condition  condit  is passed in to some functions, and before
//      a match is asserted, the condition is checked.  I hope it is
//      checked only once, and no sooner than appropriate.  It would
//      be nice to move it forward so it is checked as soon as possible.
//  Orderless is handled...  (not, "If you can't eat it all, order less"  but
//  "The universe tends toward a state without order -- orderless"..)
//  if (say) Plus is orderless, then Plus[x_,Sin[x_]] matches
//  Plus[3,Sin[3]] or Plus[Sin[3],3].
//  Also allowed: x_Integer, x_foo,  or x:f[_] .   The form x_foo has the
//  meaning that x's Head must be foo, or x must be of type foo.
//   x_foo parses into (Pattern x (Blank foo))
//  x:f[_] parses into (Pattern x (f (Blank)))
//  Return value for a match is  nil or non-nil.
//  If the value is non-nil, the stack structure env
//  will have a set
//  of bindings of all the pattern-variable bindings. If the return value
//  is nil, env will be unchanged.
//  define match stack, using up to 100 pattern variables if no-one else
//  has done it.
define variable env = make-stack(size: 100);

//  match provides a top level match of pattern to expression.
//  both pattern and expression are assumed to be Lisp lists
//  from the parser (lmath:parser)
//  Usually match won't be used by a program ... rather, it will use m1.
//  Typical usage would then be as follows...
//  match pat and exp in an environment which is a stack (perhaps empty)
#f;

//  a note on the data structuring.  
//  It would be possible to "abstract way" some of the cars and cdrs
//  but the representation from the parser is fairly definitive, close
//  to Mma, and unlikely to benefit from revision. Consequently it would
//  seem to be mostly coyness to pretend that we didn't know how things
//  were stored, and frankly, defining map-over-args as mapcar and
//  (head x) as (car x) gets tiresome, especially when you "pun" and
//  use argument lists as lisp lists. Of course, if we change the
//  data representation, this program will be harder to change as a
//  result of this decision.
//  a simple test function
define method trial (pat, exp, #key env = make-stack(size: 20))
  spushframe(env, #"trialmatch");
  if (m1(pat, exp))
    format-out("\nMatch Succeeded. Binding Stack = \n%=", env);
  else
    format-out("\nMatch Failed\n");
  end if;
  //  reset ptr to former position if environment was provided
  if (wasitprovided) spopframe(env); end if;
end method trial;

//  match returns  t/nil depending on whether it matches or not. Typically
//  if match returns non-nil, then the global variable 
//  env will be returned with some bindings: a stack-frame called "match"
//  or "matchlist". 
//  Therefore
//  the caller of match (res. matchlist)  should do an (spopframe env) after
//  appropriate use of the environment env for (say) the evaluation of
//  the rhs of a pattern-replacement rule.
define method match (pat, exp)
  spushframe(env, #"match");
  m1(pat, exp);
end method match;

// won't work for Condition on exp
define method matchlist (pl, el)
  // alternative top level for simultaneous matches
  spushframe(env, #"matchlist");
  mlist(pl, el, #f, #t);
end method matchlist;

define method m1 (p, e)
  if (not(instance?(p, <list>)))
    p = e;
  elseif (p == e)
    #t;
  elseif (p = e)
    #t;
    //  remove this if it isnt saving time..
    else
    begin
      let phead = head(p);
      // (format t "~%phead = ~s" phead)
      if (phead == #"blank")
        mblank(tail(p), e);
        // do Blank matching
        elseif (phead == #"pattern")
        //  (cadr p) is the name to be used for the
        //  pattern variable if the pattern object (caddr p)
        //  matches the expression e.
        mpat(second(p), third(p), e);
      elseif (phead == #"patterntest")
        begin
          let result = m1(second(p), e);
          if (result) (third(p))(meval(e)); else #f; end if;
        end;
      elseif (phead == #"condition")
        begin
          let result = m1(second(p), e);
          if (result & meval(third(p))) #t; else #f; end if;
        end;
      elseif (not(instance?(e, <list>)))
        #f;
        //  non-atom, non-blank p can't match atom.
        //  now both p and e are expressions.
        //  we match (recursively) their heads, and match,
        //  in sequence, elements in their cdrs, by mlist.
        //  Before calling mlist, we bind a spec variable phead
        //  to phead and note properties (Flat, Orderless) of it
        //  on spec variables isflat, isorderless, isfol.
        elseif (m1(phead, head(e)))
        // first check that heads match
        begin
          let isflat = flatp(phead);
          let isorderless = orderlessp(phead);
          let isfol = isflat & isorderless;
          //  if phead is not flat, it is convenient to
          //  set it to Sequence when matching __.
          if (isflat) #f; else phead := #"sequence"; end if;
          if (isfol)
            // Flat AND Orderless
            //  this is not written yet
            //  needed for Plus, Times, but not much
            //  else other than these important cases.
            //  (maybe "Bag"?)
            mlistfol(tail(p), tail(e), 0, size(e) - 1);
          elseif (isorderless)
            //  Orderless, but not Flat
            //  There are lots of commutative operators,
            //  including symmetric ones (e.g. x=y).
            //  this version is not quite right. see defun.
            mlistol(tail(p), tail(e), 0, size(e) - 1);
          else
            //  Flat or not, ordered.
            //  we must match in sequence all elements.
            mlist(tail(p), tail(e), #f, #t);
          end if;
        end;
      end if;
    end;
  end if;
end method m1;

// ******makeshift for now..********
define method orderlessp (x)
  member?(x, #(#"ol", #"plus", #"times"));
end method orderlessp;

define method flatp (x)
  member?(x, #(#"flat", #"plus", #"times"));
end method flatp;

// *********************************
//  mblank matches a (Blank h) or (Blank). This works if the test
//  headtest = (h) is nil, 
//  or if the head of e is (car headtest)
//    (i.e. (Blank foo) matches (foo ...))
//  or if headtest is the "type" of e.  
//    (i.e. (Blank Integer) matches 3.
define method mblank (headtest, e)
  if (headtest) mtypep(e, head(headtest)); else #t; end if;
end method mblank;

define method mblank2 (headtest, e)
  //  hardly enough for _ _ or _ _ _.
  if (headtest) mtypep(e, head(headtest)); else #t; end if;
end method mblank2;

define method blank1p (x)
  instance?(x, <pair>) & head(x) == #"blank";
end method blank1p;

define method blank2p (x)
  instance?(x, <pair>) & head(x) == #"blanksequence";
end method blank2p;

define method blank3p (x)
  instance?(x, <pair>) & head(x) == #"blanknullsequence";
end method blank3p;

define method patternp (x)
  instance?(x, <pair>) & head(x) == #"pattern";
end method patternp;

//  match two lists, ordered. If the pl (pattern list)
//  contains a Blank (_), BlankSequence (_), or BlankNullSequence (___).
//  then special routines must be used.
//  If phead has attribute flat, (isflat is true), then Blank matching
//  operates the same as BlankSequence.  Why?  So
//  a+x_ matches a+b+c with x-> b+c, instead of
//                          x-> Sequence[b,c]
define method mlist (pl, el, name, condition)
  if (empty?(pl))
    empty?(el) & meval-to-bool(condition);
    //  both must end at the same time to match
    elseif (patternp(head(pl)))
    //  must to assign the matched stuff to
    //  (cadar pl), the name..
    //  might try to avoid this cons...
    mlist(pair(caddar(pl), tail(pl)), el, cadar(pl), condition);
  elseif (blank2p(head(pl)))
    // since this is NOT orderless, we must match these suckers in
    // order, or not at all. We look for the shortest sequence
    // of length ONE or more that matches.
    //  Mma semantics requires the following glitch..
    if (isflat) phead := #"sequence"; end if;
    ml2(cadar(pl), tail(pl), el, name, 1);
  elseif (blank3p(head(pl)))
    // this one, BlankNullSequence (_ _ _) requires
    //  the shortest sequence of ZERO or more elements.
    ml2(cadar(pl), tail(pl), el, name, 0);
  elseif (isflat & blank1p(head(pl)))
    //  for a flat operator, treat f[...,x_,...] like f[...,x__,...].
    //  So says Mma semantics.
    ml2(cadar(pl), tail(pl), el, name, 1);
  elseif (name)
    mpat(name, head(pl), head(el)) & mlist(tail(pl), tail(el), #f, condition);
  elseif (m1(head(pl), head(el)))
    //  if the cars match, so must the elements of the cdrs
    mlist(tail(pl), tail(el), #f, condition);
  end if;
end method mlist;

//  match patobj h against minmatch or more initial elements of el. Succeed only
//  if matching all of pl against the rest of el succeeds.
//  if name is non-nil, the matching of h will result in the
//  binding of name to the value matched.
//  As usual, el is an expression list, and pl is a list of pattern objs.
//  This is called to match BlankSequence with minmatch=1, and
//  to match BlankNullSequence with minmatch=0
define method ml2 (h, pl, el, name, minmatch, condition)
  block (return-from-ml2)
    if (empty?(el))
      //  If there are no expressions left in the list then
      //  if it is acceptable for h to match "no" elements
      //  and if this is consistent with any previous assignments
      //  to "name", and the remaining patterns in the pattern list can
      //  also be matched to "no" elements, we will succeed here.
      begin
        let r = list(phead);
        if (minmatch = 0 & if (name) mpat(name, r, r); else #t; end if)
          if (mlist(pl, #f, #f, #t))
            #t;
          else
            stack-ptr(env) := ptr;
            #f;
          end if;
        else
          stack-ptr(env) := ptr;
          #f;
        end if;
      end;
      // remove the ;; below if you want to use the special end case.   
      // 	((null pl)(ml2quick h pl el name))
      else
      begin
        let lel = size(el);
        //  Starting with the minimum number (0, 1) of elements
        //  to match, try to match  h
        for (k = minmatch then 1+(k), collect = nil then nil,
             until //  termination with failure if we exhaust all
                   //  the elements up to the length of el, and
                   //  still haven't gotten a match. Reset the
                   //  stack pointer if we've changed it, and
                   //  return nil
             k > lel)
          //  try to match h against 1st, then 1 thru 2, then 1 thru lel
          //  of the elements in el. This program can't skip any elements
          //  since it is not for "orderless"  heads.
          // 		 (format t "~%k=~s" k) ;debug
          for (j = el then cdr(j),
               //  j is the list of expressions in el
               //  starting with el itself, and stepping down..
               count(1, 1+(count)) = nil then nil,
               until // termination check: when we've looked at the first k els
                     //  and haven't jumped out, we let the rest of the pattern
                     //  have a chance.
               count > k)
            //  the do-loop body
            // 		     (format t "~% j = ~s" j)
            if (mblank2(h, head(j)))
              collect := pair(head(j), collect);
              // 			    (format t "~% consed onto collect: ~s" collect)
              //  reset stack pointer after collection
              stack-ptr(env) := ptr;
              //  it should be possible to expedite failure here.
              //  If p fails to match e[j+n], then p's predecessor
              //  should be advanced n places. But the control
              //  flow is too messy to contemplate for now.
              // . (e.g. f[x__,p_pred])
              //  But, anyway, the predicate failed.
              else
              stack-ptr(env) := ptr;
              return-from-ml2(#f);
            end if;
          finally
            values(//  note that phead will be a function
                   //  head f if f is flat, else Sequence.
                   //  q? should use uniq/ucons here
                   collect := pair(phead, reverse(collect)),
                   // 		      (format t "~%Collected so far .. ~s" collect)
                   //  if we've been provided a non-nil name, then
                   //  check it against previous bindings if any;
                   //  push the value for future checks.
                   if (name) mpat(name, collect, collect); else #t; end if
                    & //  match the rest, if you can.
                   if (mlist(pl, j, #f, condition))
                     return-from-ml2(#t);
                   else
                     //  else un-assert this binding and try k:=k+1
                     (stack-ptr(env) := ptr);
                     #f;
                   end if);
          end for;
        finally
          values(stack-ptr(env) := ptr, #f);
        end for;
      end;
    end if;
  end block;
end method ml2;

//  special case in above..
//  if you have the last pattern element, you can be sure that either
//  it matches the rest of the expressions, or the pattern fails.
define method ml2quick (h, pl, el, name, condition)
  let collect = #f;
  let ptr = stack-ptr(env);
  block (return)
    for (j = el then cdr(j),
         until // termination check: when we've exhausted expr. list
               //  and haven't jumped out, we've absorbed all of el.
         empty?(j))
      //  the do-loop body
      if (mblank2(h, head(j)))
        collect := pair(head(j), collect);
        //  reset stack pointer after collection
        stack-ptr(env) := ptr;
      else
        return(#f);
      end if;
    finally
      values(//  note that phead will be a function
             //  head f if f is flat, else Sequence.
             collect := pair(phead, reverse!(collect)),
             //  if we've been provided a non-nil name, then
             //  check it against previous bindings if any;
             //  push the value for future checks. Return.
             if (name)
               mpat(name, collect, collect);
             else
               meval-to-bool(condition);
             end if);
    end for;
  end block;
end method ml2quick;

//  match two lists, orderless, not flat or allowing BlankSequence
//  start with element i of expressionlist el, length of el is h.
//  This program is not directly applicable to Mma since BlankSequence
//  is allowed anywhere..
define method mlistol (pl, el, i, h, condition)
  if (empty?(pl))
    empty?(el);
    //  success if we run out of p and e together
    elseif (i = h)
    #f;
    // exhausted all possibilities. fail
    else
    begin
      let p = head(pl);
      let ptr = stack-ptr(env);
      block (return)
        for (count = 0 then 1+(count), index = i then mod(1+(index), h),
             until count > h)
          if (m1(p, el[index]))
            //  if success, we leave a binding for (car p) on env,
            //  remove (elt el index) from el, 
            //  and try next pattern.
            // debug	(format t "~%matched ~s to ~s " p (elt el index))
            if (mlistol(tail(pl),
                        remove(copy-subsequence(el, start: index), #f,
                               test: true2, count: 1),
                        0, h - 1, condition))
              return(meval-to-bool(condition));
            elseif (nil);
            end if;
          elseif (nil);
          end if;
          //  failure to match p to (elt el i)
          //  or perhaps failure at another level below -->
          //  reset bindings, restore el, and keep trying
          stack-ptr(env) := ptr;
        finally
          //  we never matched  p=(car pl), starting at (elt el i).
          //  try matching p again, but starting at (elt el (1+ i))
          mlistol(pl, el, i + 1, h);
        end for;
      end block;
    end;
  end if;
end method mlistol;

//  this one handles orderless and flat and blanks..
//  try to match (car pl) against 0 or more elements of el.
//  start with element i of expressionlist el, length of el is h.
// this program is not structurally correct...
//
nil(#f, nil(#f), #f);

//  match a pattern and an expression
//  Changed from version 1 to allow for pattern vars to appear repeated
//  Changed from version 2 and 3 to deal with a:f[_]  etc.
define method mpat (name, patobj, e)
  //  to match (Pattern name patobj) to e, 
  //  first see if name has a value - if so, test for equality of value to e.
  //  we assume the user has not done f[x_foo,x_bar] (different tests...)
  //  Otherwise just bind name to e
  let (val, found) = sfind(env, name);
  if (found)
    val = e;
  elseif (m1(patobj, e))
    spush(env, name, e);
    #t;
  else
    #f;
  end if;
end method mpat;

//  if x is atomic, if typ is not a type, or x is not of type typ, return nil.
//  in case x is a cons, see if its head is eq to typ.
define method mtypep (x, typ)
  if (not(instance?(x, <list>)))
    let (isnoerr, val)
        = errorset(// excl returns nil if no err, value of form
                   instance?(x, typ));
    if (isnoerr) val; else #f; end if;
  else
    head(x) == typ;
  end if;
end method mtypep;

// these cause problems if Integer and integer are the same as INTEGER
// LTD: Can't handle complex deftypes.
#f;

// LTD: Can't handle complex deftypes.
#f;

//  etc could do single-float, double-float, complex, number, cons, ...
//  extra cases to consider: 
//   x_.
//     = (Optional (Pattern x (Blank))) 
//  Also, Orderless + Flat.  Any other Attributes?  How about Defaults?,
//  repeated, what else?
//  also f[x_]:=g[x]  /; x>0
//  some sample programs for combinatorial matching.
//  mapcomb applies the function f to each combination of
//  elements in l. Non-destructive.  Try (mapcomb 'print '(1 2 3))
define method mapcomb (f, l)
  local method mc (l, c)
          if (empty?(l))
            f(c);
          else
            mc(tail(l), c);
            mc(tail(l), pair(head(l), c));
          end if;
        end method mc;
  mc(reverse(l), #f);
end method mapcomb;

//  map over all combinations of l, and return the first combination c
//  for which (f c) is true.
define method mapcomb-if (f, l)
  block (return-from-mapcomb-if)
    local method mc (l, c)
            if (empty?(l))
              if (f(c)) return-from-mapcomb-if(c); end if;
            else
              mc(tail(l), c);
              mc(tail(l), pair(head(l), c));
            end if;
          end method mc;
    mc(reverse(l), #f);
  end block;
end method mapcomb-if;

//  list of permutations.
define method permlist (l)
  if (empty?(l))
    #f;
  elseif (empty?(tail(l)))
    list(l);
  else
    begin
      let res = #f;
      let this = list(head(l));
      let prev = permlist(tail(l));
      //  insert this in each position in elements in prev
      //  and put on the list res
      for (p = prev then cdr(p), until empty?(p))
        for (left = nil then append(left, list(car(right))),
             right = car(p) then cdr(right), until empty?(right))
          res := pair(concatenate(left, this, right), res);
        finally
          res := pair(concatenate(left, this, right), res);
        end for;
      finally
        res;
      end for;
    end;
  end if;
end method permlist;

//  these are not part of the matcher, but they are often used in
//  the matching process.  For the moment, we'll keep them in the
//  same file.
define method integerq (x) instance?(x, <integer>); end method integerq;

define method evenq (x) instance?(x, <integer>) & even?(x); end method evenq;

define method oddq (x) instance?(x, <integer>) & odd?(x); end method oddq;

define method numberq (x) instance?(x, <number>); end method numberq;

//  have not defined PrimeQ PolynomialQ VectorQ MatrixQ ValueQ OrderedQ UnSameQ
define method sameq (x, y) if (x = y) #t; end if; end method sameq;

//  this is really cute:  if x matches an element of l, return true
//  e.g. if pat = (m1 x l) (PatternTest (Pattern x (Blank)) IntegerQ) 
//  then (MemberQ '(a b 3 z) pat)  will return True
define method memberq (l, x)
  if (member?(x, l, test: match)) #t; end if;
end method memberq;

define method freeq (l, x)
  local method freeqx (h) freeq(h, x); end method freeqx,
        method dependsx (h) empty?(freeq(h, x)); end method dependsx;
  // returns t or nil
  if (matchq(l, x))
    #f;
  elseif (instance?(l, <pair>))
    if (any?(dependsx, tail(l))) #f; else #t; end if;
  else
    #t;
  end if;
end method freeq;

// truth-value conversion
// LTD: No macros.
#"tval";

define method matchq (l, x)
  if (match(x, l)) #t; else #f; end if;
end method matchq;

define method atomq (x)
  if (not(instance?(x, <list>))) #t; end if;
end method atomq;

define method greater (x, y)
  if (instance?(x, <number>) & instance?(y, <number>))
    tval(x > y);
  else
    list(#"inequality", x, #"greater", y);
  end if;
end method greater;

define method less (x, y)
  if (instance?(x, <number>) & instance?(y, <number>))
    tval(x < y);
  else
    list(#"inequality", x, #"less", y);
  end if;
end method less;

define method equal (x, y)
  if (member?(x, #(#"indeterminate", #"infinity")))
    list(#"inequality", x, #"equal", y);
  elseif (x = y)
    #"true";
    //  handles numbers too, if equal
    elseif (instance?(x, <number>) & instance?(y, <number>))
    #f;
    //  for now, we dump anything we can't prove into Inequality
    else
    list(#"inequality", x, #"equal", y);
  end if;
end method equal;

//  need LessEqual  etc.
//  this is NOT consistent with Mathematica's program, exactly,
//  since 2 numbers of different precision may be SameQ in mma.
define method sameq (x, y) tval(x = y); end method sameq;

define method meval-to-bool (x)
  if (x == #"true")
    #"true";
  elseif (empty?(x))
    #f;
    //  that is, False
    elseif (meval(x) == #"true")
    #"true";
  else
    #f;
  end if;
end method meval-to-bool;

