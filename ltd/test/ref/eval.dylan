//  -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
//  Mathematica(tm)-like evaluator
//  copyright (c) 1990 Richard J. Fateman; pieces by Tak Yan, Derek Lai
// LTD: Function LOAD not yet implemented.
load("mma");

"(in-package mma)";

// (provide 'math-eval)(require "ucons1")(require 'math-parser "parser")
// (require "stack1") (require "disp1")(require "pf")(require "simp1")(require "rat1")(require "match")
"(in-package mma)";

define module mma export tl, mread1; end module mma;

// line number for top-level. (same as (meval $Line))
define variable count = 1;

#f;

//  environment
#f;

//       funnyvars is a hash table containing variables which, when set,
//       cause function to be executed
define variable funnyvars = make(<table>, test: \==, size: 8);

define method tl ()
  fluid-bind (*package*
               = // LTD: Function FIND-PACKAGE not yet implemented.
                 find-package(#"mma"))
    let h = #f;
    let hs = #f;
    let hin = #f;
    let timesofar = 0;
    let timeunit = 1.0 / $internal-time-units-per-second;
    let env = make-stack(size: 50);
    if (count = 1)
      format-out("Mock-Mma (Sun-4) 2.0 (Oct. 3,, 1991) [With pre-loaded data]\n  Possibly Copyright 1990-1991 U.C.Berkeley\n   -- Terminal graphics initialized -- \n");
    end if;
    block (return)
      while (#t)
        timesofar
         := // LTD: Function GET-INTERNAL-RUN-TIME not yet implemented.
            get-internal-run-time();
        format-out("\nIn[%=] := ", count);
        //  actually In and Out are variables too.
        #"setq"(hin: #"multiple-value-bind"(#"isnoerr"(#"val"),
                                            #"errorset"(p(), #"t"),
                                            #"if"(isnoerr: #"val",
                                                  #"clear-input"(#"t"))));
        setqq(ulist(#"in", count), hin);
        //    (setq h (simp(meval hin))) ;; if you don't have errorset, try this.
        #"setq"(h: #"multiple-value-bind"(#"isnoerr"(#"val"),
                                          #"errorset"(#"meval"(#"hin"), #"t"),
                                          #"if"(isnoerr: #"val",
                                                (#"hold", #"hin"))));
        timesofar
         := // LTD: Function GET-INTERNAL-RUN-TIME not yet implemented.
            get-internal-run-time()
             - timesofar;
        //  this is not the same as mathematica but I find it more convenient
        //  this way. We've also implementing "Timing", if you prefer.
        if (#"true" == meval(#"$showtime"))
          (method (s, #rest args)
             apply(maybe-initiate-xp-printing,
                   method (xp, #rest args)
                     begin
                       pprint-newline+(unconditional: xp);
                       write-string++("time = ", xp, 0, 7);
                       using-format(xp, "~3,$", pop!(args));
                       write-string++(" secs.", xp, 0, 6);
                     end;
                     if (args) copy-sequence(args); end if;
                   end method,
                   s, args);
           end method)(#t, timesofar * timeunit);
        end if;
        if (h == #"exit" | (instance?(h, <list>) & head(h) == #"quit"))
          format-out("\nExited to Lisp\n");
          return(#t);
        else
          setqq(ulist(#"out", count), h);
          if (h == #"null")
            #f;
            //  don't print nil-valued answers
            else
            hs := list(#"set", ulist(#"out", count), h);
            disp(buildformat(hs));
          end if;
        end if;
        // LTD: Function SET not yet implemented.
        set(#"$line", count := count + 1);
      end while;
    end block;
  end fluid-bind;
end method tl;

//  this definition replaces the program in the parser file
define method mread1 ()
  if (member?(pc(), #(' ', '\t', '\012'), test: \=))
    rc();
    mread1();
  elseif (digit-char?(pc()))
    //  next character is a digit 0-9
    collect-integer(char-to-int(read-element(stream, nil)), 10);
    // radix 10 default
    //  for every alphabetic symbol, set up a hash table
    else
    chash(// LTD: Function READ-PRESERVING-WHITESPACE not yet implemented.
          read-preserving-whitespace(stream, #f, #"e-o-l"))
     | #"false";
  end if;
end method mread1;

//  enter a variable in the symbol table by making a hash
//  table as its value.
define variable symtab = make(<table>, test: \==, size: 150);

//  It's plausible to change this to use defstruct ... then
//  make every declared "symbol-table-entry" in lmath a structure
//  with (at least) the following data  (all now in same hashtable)
//   (a) value for the symbol  e.g.  a=3
//   (b) value for expressions with the symbol as head. e.g. a[45]=x+1
//   (c) value for the collected attributes of the symbol.
//        e.g. Attributes[a] ={Orderless, Protected, Listable}
//   (d) value for each of the attributes 
//   (e) value for function definition "built-in"  e.g. numeric cosine
//   (f) value for user-defined function rules e.g. a[x_]:= ...
//   (g) string for symbol printing (e.g. " + " for Plus)
//   (h) formatting information (though could be on "Format")
//   (i) left/right operator precedence information
//   (j) messages/ documentation
//   (k) package? context?
//  possible types are (a): symbol; (b): hash-table; (c): list; (d) bit-vector;
//  (e) lisp-function-value; (f) list? array? (g) string; (h) program?/
//  (i)  (two) integers
define method chash (m)
  fluid-bind (*package*
               = // LTD: Function FIND-PACKAGE not yet implemented.
                 find-package(#"mma"))
    if (~ instance?(m, <symbol>))
      m;
      //    ((null m)nil) do we need to check for nil or t? Maybe not.
      else
      begin
        let _that = #f;
        if (_that := symtab[m])
          _that;
          // either it's there or
          else
          symtab[m] := make(<table>, test: \=, size: 4);
        end if;
      end;
    end if;
    m;
  end fluid-bind;
end method chash;

//  the following stuff is make-shift.
define method head (h)
  if (instance?(h, <pair>)) head(h); else object-class(h); end if;
end method head;

//  the semantics are probably OK as long as only global properties
//  are being recorded.
//  SetAttribute puts info in two places so that the Attributes can
//  be gotten one at a time, and also collectively
//  define ClearAttribute similarly
define method setattribute (h, att, #key val = #"true")
  h := symtab[h];
  h[att] := val;
  h[#"attributes"] := add!(att, h[#"attributes"]);
  pair(#"list", h[#"attributes"]);
end method setattribute;

define method attributes (h)
  pair(#"list", symtab[h][#"attributes"]);
end method attributes;

//  Assignment statements treat the lhs with partial evaluation.
//  For a non-atomic Head, evaluate all the arguments, but not
//  the head. Presumably this should check attributes of the Head
//  like HoldAll, HoldFirst, etc. We don't do that yet.
//  we evaluate the lhs partially and then the rhs.
define method holdallp (h)
  member?(h, #(#"timing", #"setqq", #"setdelayed", #"if", #"quote"));
end method holdallp;

// for now
//  we'd like to have a Quote operator, but the repeated evaluation rule
//  makes it almost impossible to work unless we check for it specially..
//  alternatively, we can set *evalonce* to t, and (vastly)
//  change the semantics. Sometimes this vast change is no change at all....
define method holdfirstp (h) member?(h, #(#"set")); end method holdfirstp;

//  for now
//  evaluate args, depending on the hold-specs of the head
define method mevalargs (head, l)
  if (holdallp(head))
    l;
  elseif (holdfirstp(head))
    ucons(head(l), umapcar(meval, tail(l)));
  else
    umapcar(meval, l);
  end if;
end method mevalargs;

//  note that the name of this function conflicts with that
//  of the lisp function set, unless
//  (a) capitalization is observed  OR
//  (b) the package system is protecting it..
define method set (lhs, rhs)
  //  lhs=rhs
  //  the value associated with the lhs will be stored
  //  in the symbol table symtab, with the key h,
  //   which is either the head of the lhs,
  //  or the lhs itself.  That is  f=45 is stored in the
  //  hash table for f, under the indicator "f"
  //  and f[0]=7 is stored in the hash table for f, under
  //  the indicator (0).
  if (instance?(lhs, <symbol>))
    h := lhs;
  else
    lhs := mevalargs((h := head(lhs)), tail(lhs));
  end if;
  // (format t "Set ~s to ~s~%" h rhs)
  //  this stores the material in the hash table.
  //  QUESTION: M'ma doesn't do this, but we could, by storing
  //  stuff on a local environment... f[x_,y_]:=Block[{},x=y+1];
  //  what if (gethash h symtab) is a matrix, and this is a valid matrix
  //  setting?  Then we should try to store the value in the array.
  //  This is insufficient error checking but...
  if (instance?(h, <symbol>)
       & //  is value of h an array?
      matrix-p((fun := symtab[h][h]))
       & //  are subscripts integers in the right range?
      every?(method (n, r)
               (instance?(n, <integer>) & n > 0 & n <= r);
             end method,
             lhs, matrix-dimensions(fun)))
    //  ok, then set the value in the array. Sorry it took so long.
    matrix-a(fun)[head(lhs) - 1, second(lhs) - 1] := (rhs := meval(rhs));
  else
    // else
    symtab[h][lhs] := (rhs := meval(rhs));
  end if;
  //  Next, check for special variables which, when set, cause other
  //  things to happen. E.g. Precision= number means, in the
  //  bigfloat system, (bigfloat-init number) gets executed.
  if (h := funnyvars[h]) h(rhs); end if;
  rhs;
end method set;

//  there is another file  (nmatrix) that defines a matrix type..
//  this should be tied in to the matrix stuff from franz, perhaps.
//  also, we have to decide which bigfloat to use... mpfun or rjf's
//  old bfstuff.
define method matrix-p (x) #f; end method matrix-p;

//  for now, this will have to do.
define method setqq (lhs, rhs)
  //  lhs=rhs, but don't mevaluate either.
  h
   := if (not(instance?(lhs, <list>)))
        lhs;
      else
        begin let _ = head(lhs); (lhs := tail(lhs)); _; end;
      end if;
  symtab[h][lhs] := rhs;
  if (h := funnyvars[h]) h(rhs); end if;
  rhs;
end method setqq;

define method setdelayed (lhs, rhs)
  let spot = symtab[head(lhs)];
  let rs = element(spot, #"setdelayed", default: #"emptyruleset");
  let k = #f;
  if (rs == #"emptyruleset")
    spot[#"setdelayed"] := list(pair(lhs, rhs));
    // check to see if lhs is equal to one of the other items already
    // stored.
    elseif (k := cl-assoc(lhs, rs, test: \=))
    begin
      let _that = #f;
      if (_that := tail(k) = rhs)
        _that;
        //  just redefining same rule
        else
        format-out("Replacing previous rule %= \n", lhs);
        tail(k) := rhs;
      end if;
    end;
  else
    push!(pair(lhs, rhs), spot[#"setdelayed"]);
  end if;
  #"null";
end method setdelayed;

//  this assumes the value of a mathematica symbol is its lisp value
//  if it is simply a constant or non-hash-table. That means that
//  a lisp dynamically bound variable could be used to block access
//  to the globally bound variable of the same name.  Better not
//  use local-variable names like Sin or Cos unless you mean to
//  inhibit access to the global names.
define method meval-atom (h)
  if (env) h := sfind(env, h); elseif (nil); end if;
  //  if we find it here on the env stack, do we continue?
  //  For now, we continue evaluating..
  if (~ instance?(h, <symbol>))
    h;
  else
    begin
      let r = element(symtab, h, default: h);
      if (instance?(r, <table>))
        element(r, h, default: h);
        //  h is default if missing
        else
        r;
      end if;
    end;
  end if;
end method meval-atom;

//  look up the value of e on e's hashtable, key e
//  look up the value of e[10] on e's hashtable, key (e 10)
define method msymbol-value (h)
  let tabentry = #f;
  if (instance?(h, <symbol>)
       & instance?((tabentry := element(symtab, h, default: h)), <table>))
    tabentry[h];
  elseif (not(instance?(h, <list>)))
    h;
    //  an atom, not in the symbol table. Where did it come from?
    //  must be a cons
    elseif (constant?(head(h)))
    h;
  elseif (tabentry := symtab[head(h)])
    element(tabentry, h, default: h);
  else
    h;
  end if;
end method msymbol-value;

define method msymbol-function (h)
  symtab[h][#"setdelayed"];
end method msymbol-function;

//  is this going to have the right scope?
//  
//  must check for HoldAll, HoldFirst etc.
// ----end of makeshift definitions
define method mapply (h, args, expr, env)
  let fun = #f;
  // get info on evaluating arguments and do it to args
  args := mevalargs(h, args);
  if (constant?(h))
    expr;
    //  allows any lisp function, almost, to slip through
    //  check for constant values pre-stored
    elseif (~ instance?(h, <symbol>))
    expr := pair(h, args);
  elseif (~ symtab[h])
    expr := pair(h, args);
  elseif (let (val, found) = symtab[h][args];
    if (found) expr := val; else #f; end if)
    #t;
    //  check for array...
    elseif (instance?(h, <symbol>) & matrix-p((fun := symtab[h][h]))
             & //  is value of h an array
            every?(method (n, r) (n > 0 & n <= r); end method, args,
                   matrix-dimensions(fun)))
    expr := matrix-a(fun)[head(args) - 1, second(args) - 1];
    //  next check for user-defined  function
    elseif (fun := msymbol-function(h))
    expr := rulesetapply(h, fun, args);
    //  next check for built-in LISP function
    //  (clearly not something that Mathematica does)
    elseif (instance?(h, <symbol>)
             & // LTD: Function FBOUNDP not yet implemented.
               fboundp(h))
    //       (format t "~% applying a lisp function ~s" h)
    expr := apply(h, args);
  else
    expr := ucons(h, args);
  end if;
  //  what next?
  expr;
end method mapply;

#f;

define method rulesetapply (phead, rules, args)
  //  get attributes of phead and manipulate args
  //  for now, assume evaluate all args, ignore Hold* attributes
  args := mevalargs(phead, args);
  let ptr = stack-ptr(env);
  let isflat = flatp(phead);
  let isorderless = orderlessp(phead);
  let isfol = isflat & isorderless;
  let origfn = phead;
  let res = #f;
  if (isflat) #f; else phead := #"sequence"; end if;
  block (return)
    for (r = rules then cdr(rules), until empty?(r))
      let thisrule = head(r);
      let condit = #t;
      let lhs = head(thisrule);
      let rhs = tail(thisrule);
      //  Note: if the rule was
      //  f[a_,b_]:= g[a,b] /; a>b, the parsed result is
      //  (SetDelayed (f ..) (Condition (g a b) (Greater a b)))
      //  see if there is a Condition on the rhs of the rule
      //  e.g. (Condition (foo a b) (Greater a b))
      if (instance?(rhs, <pair>) & head(rhs) == #"condition")
        condit := third(rhs);
        // condit = (Greater a b)
        rhs := second(rhs);
      elseif (nil);
      end if;
      // rhs = (foo a b)
      // 	  (format t "~%lhs= ~s ~%rhs= ~s ~%condition =~s" lhs rhs condit)
      //  test for matching
      if (if (isorderless)
            mlistol(cdaar(r), args, 0, size(args), condit);
          else
            mlist(cdaar(r), args, #f, condit);
          end if)
        //  if the appropriate matcher succeeded, then
        //  with the environment env from match, evaluate rhs.
        res := meval(rhs);
        //  restore the environment
        stack-ptr(env) := ptr;
        //  get out of the do-loop
        return(res);
      elseif (nil);
      end if;
    finally
      //  no more rules to try -- return original
      ucons(origfn, args);
    end for;
  end block;
end method rulesetapply;

//  Major evaluation function for an expression
//  see Mathematica book p 568
define method meval-to-function (x) x; end method meval-to-function;

//  don't evaluate function name
define variable *evalonce* = #f;

//  should be t to make quote (etc etc) work
define method meval (e)
  let saved = e;
  e
   := if (not(instance?(e, <list>)))
        meval-atom(e);
        //  check off other constant expressions that don't evaluate.
        //  perhaps Messages?
        // ((patternobjectp e) e) .. What about Patterns?
        //  (mapply (car foo)(cdr foo)  foo) ==> foo  with no conses...
        else
        (res := mapply(meval-to-function(head(e)), tail(e), e, env));
      end if;
  //  note the 3rd arg to mapply, just in case you want to
  //  return the expression as the result without any change.
  //  next step --
  // 
  //  do we keep evaluating until there is no change???
  if (*evalonce*)
    e;
  elseif (e = saved)
    e;
    //
    else
    meval(e);
  end if;
end method meval;

//  Each global object X is associated with a hash table
//  and we can, for each, 
//  to get the value, do (gethash X X), (gethash 'rules X) etc.
//  Local bindings hide everything.
// Do we want to do infinite evaluation? 
// How do we keep single copies of items in memory?
// set up initial symbol-table entries for built-in stuff
//  should also set attributes
begin do(chash, built-in-syms); built-in-syms; end;

//  All system-level $-stuff can be initialized and stored this way
define method globinit (name, val)
  chash(name);
  //  just in case it isn't already there
  symtab[name][name] := val;
end method globinit;

globinit(#"$line", 1);

globinit(#"false", #f);

globinit(#"i", 0 + 1 * $i);

//  imaginary unit  I^2 = -1.
//  simple debugging tool: examine contents of symtable entry
define method showhash (x)
  do(method (key, val) format-out("\nkey=%=, val=%=", key, val); end method,
     key-sequence(x), x);
end method showhash;

//  Attributes that evaluation routines use:
//     - Flattened [associative, flatten out nested expressions]
//     - Orderless [commutative, put args in order]
//     - HoldFirst [don't evaluate first arg yet]
//     - HoldRest [only evaluate first arg now]
//     - HoldAll [don't evaluate any args now]
//     - Procedure [procedure to call to do actual evaluation]
//     - Default
// [default value for optional variables]
setattribute(#"plus", #"flattened");

setattribute(#"plus", #"orderless");

setattribute(#"plus", #"default", 0);

setattribute(#"times", #"flattened");

setattribute(#"times", #"orderless");

setattribute(#"times", #"default", 1);

setattribute(#"power", #"default", 1);

setattribute(#"and", #"holdall");

//  short-circuiting
setattribute(#"or", #"holdall");

setattribute(#"if", #"holdrest");

setattribute(#"condition", #"holdrest");

setattribute(#"set", #"holdfirst");

//  we don't use this -- Set is in Lisp
setattribute(#"setdelayed", #"holdall");

setattribute(#"upset", #"holdfirst");

setattribute(#"upsetdelayed", #"holdall");

setattribute(#"tagset", #"holdfirst");

setattribute(#"tagsetdelayed", #"holdall");

setattribute(#"replaceall", #"holdfirst");

setattribute(#"replacerepeated", #"holdrest");

setattribute(#"rule", #"holdfirst");

setattribute(#"ruledelayed", #"holdall");

//  convert all real numbers to exact rational numbers
define method real (a, b) a + b; end method real;

//  this works only for integer x,  x>0
define method decimalsin (x)
  ceiling(// LTD: Function INTEGER-LENGTH not yet implemented.
          integer-length(x)
           * 0.30102999566398114D0);
end method decimalsin;

//  handle %, %%, etc.
define method out (#rest n)
  symtab[#"out"][ucons(if (empty?(n))
                         count - 1;
                       elseif (negative?(n := head(n)))
                         count + n;
                       else
                         n;
                       end if,
                       #f)];
end method out;

define method simp (x) simp(x); end method simp;

//  rational simplification
define method rat (x) into-rat(x); end method rat;

//  leave the answer in rational form.
define method unrat (x) outof-rat(x); end method unrat;

//  convert the answer to list form.
//  convert u to a rational with a single polynomial numerator
//  and denominator.  That is (x+1)^2 will be multiplied out.
//  result in rational form.
define method ratexpand (u)
  fluid-bind (*expand* = #t)
    let x = into-rat(u);
    make-rat(numerator: make-fpe(fpe-expand(rat-numerator(x)), 1),
             denominator: make-fpe(fpe-expand(rat-denominator(x)), 1));
  end fluid-bind;
end method ratexpand;

//  pick out parts of an expression.  x[[y]] parses to Part[x,y].
//  (a+r+b^c) [[3,2]] is c.
// Generalizes somewhat in that (a+b^c)[[2,r]] returns (b^c)[[r]].
//  Does not handle negative part-numbers or lists of parts as
//  done in Mathematica. Also, won't decompose defstruct items
//  unless we do something about it for each structure...
define method part (u, #rest k) part1(u, k); end method part;

define method part1 (u, kl)
  if (empty?(kl))
    u;
  elseif (instance?(head(kl), <integer>) & size(u) <= head(kl))
    part1(u[head(kl)], tail(kl));
    //  leave unevaluated part if can't handle it.
    else
    ucons(#"part", ucons(u, kl));
  end if;
end method part1;

//  basic simplification of Times
define method times (#rest x)
  block (return-from-times)
    for (h in x)
      //  body
      if (instance?(h, <number>))
        nums := nums * h;
        //  collect CL numbers
        //  if you find a rat, break out !
        elseif (instance?(h, <rat>))
        return-from-times(reduce-rat(rat*, into-rat(head(x)), tail(x)));
      else
        push!(h, oths);
      end if;
    finally
      //  iterate with h running over all args
      // resultform
      if (1 = nums)
        if (empty?(oths))
          1;
        else
          ucons(#"times", uniq(reverse!(oths)));
        end if;
      elseif (empty?(oths))
        nums;
      else
        ucons(#"times", ucons(nums, uniq(reverse!(oths))));
      end if;
    end for;
  end block;
end method times;

//  this definition allows  a=c; to take effect and return Null, so no display
define method compoundexpression (#rest x)
  for (i = x then cdr(i), until empty?(i))
    //  evaluate each element in turn, return last.
    ans := meval(head(i));
  finally
    ans;
  end for;
end method compoundexpression;

define method plus (#rest x)
  block (return-from-plus)
    for (h in x)
      //  body
      if (instance?(h, <number>))
        inc!(nums, h);
        //  if a rat form, break out!
        elseif (instance?(h, <rat>))
        return-from-plus(reduce-rat(rat+, into-rat(head(x)), tail(x)));
      else
        push!(h, oths);
      end if;
    finally
      //  iterate with h running over all args
      // resultform
      if (zero?(nums))
        if (empty?(oths))
          0;
        else
          ucons(#"plus", uniq(reverse!(oths)));
        end if;
      elseif (empty?(oths))
        nums;
      else
        ucons(#"plus", ucons(nums, uniq(reverse!(oths))));
      end if;
    end for;
  end block;
end method plus;

define method power (b, e)
  if (instance?(b, <number>) & instance?(e, <number>))
    b ^ e;
  elseif (instance?(b, <rat>) & instance?(e, <integer>))
    into-rat(list(#"power", b, e));
  else
    ulist(#"power", b, e);
  end if;
end method power;

//  note: Timing is a HoldAll function... otherwise the evaluation
//  of the argument would come first, and the timing would
//  be of an already evaluated expression.
define method timing (x)
  let timeunit = 1.0 / $internal-time-units-per-second;
  let timesofar
      = // LTD: Function GET-INTERNAL-RUN-TIME not yet implemented.
        get-internal-run-time();
  let result = meval(x);
  list(#"list",
       apply(list, #"times",
             (// LTD: Function GET-INTERNAL-RUN-TIME not yet implemented.
              get-internal-run-time()
               - timesofar)
              * timeunit,
             #(#"second")),
       result);
end method timing;

