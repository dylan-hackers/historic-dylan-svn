//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Terms.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
// 
// 	Extensions to the term simplification routine are possible by
// 	defining the function "term-inference" in the DTP package.
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
define method nsimplify-terms (literal)
  // Procedurally attach to rewrite ground terms to canonical form
  block (return-from-nsimplify-terms)
    if (~ *use-procedural-attachments*)
      return-from-nsimplify-terms(literal);
    end if;
    let simple-terms = make(<deque>);
    block (return)
      for (term in literal.literal-terms)
        push-last(simple-terms,
                  if (groundp(term))
                    rewrite-ground-term(term);
                  else
                    term;
                  end if);
      finally
        literal.literal-terms := simple-terms;
        return(literal);
        simple-terms;
      end for;
    end block;
  end block;
end method nsimplify-terms;

// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
//  This list was copied from Epikit's list of attached functions, after
//  removing the following special cases:
// 	denotation, eval, execute, list, name
//  (The function eval is treated specially.)
//  All of these are arithmetic operations on numbers.
// Ground terms with these functions will be simplified by calling Lisp
define variable *attached-lisp-functions* =
  #(#"+", #"-", #"*", #"/", #"1+", #"1-", #"abs", #"acos", #"acosh", #"ash",
    #"asin", #"asinh", #"atan", #"atanh", #"boole", #"ceiling", #"cis",
    #"complex", #"conjugate", #"cos", #"cosh", #"decode-float",
    #"denominator", #"exp", #"expt", #"fceiling", #"ffloar", #"float",
    #"float-digits", #"float-precision", #"float-radix", #"float-sign",
    #"floor", #"fround", #"ftruncate", #"gcd", #"imagpart",
    #"integer-decode-float", #"integer-length", #"isqrt", #"lcm", #"log",
    #"logand", #"logandc1", #"logandc2", #"logcount", #"logeqv", #"logior",
    #"lognand", #"lognor", #"lognot", #"logorc1", #"logorc2", #"logxor",
    #"max", #"min", #"mod", #"numerator", #"phase", #"rational",
    #"rationalize", #"realpart", #"rem", #"round", #"scale-float", #"signum",
    #"sin", #"sinh", #"sqrt", #"tan", #"tanh", #"truncate");

// ----------------------------------------------------------------------------
define method rewrite-ground-term (term)
  if (not(instance?(term, <list>)))
    term;
  elseif (first(term) == #"eval")
    block (nil)
      apply(first(term), tail(term));
    exception (<error>)
      #f;
    end block;
  elseif (cl-find(first(term), *attached-lisp-functions*))
    //  Must be an arithmetic function, so error/NIL -> 0
    begin
      let new-term
          = block (nil)
              // LTD: Function EVAL not yet implemented.
              eval(term);
            exception (<error>)
              #f;
            end block;
      new-term | 0;
    end;
  elseif (// LTD: Function FBOUNDP not yet implemented.
          fboundp(#"term-inference"))
    term-inference(term);
  else
    term;
  end if;
end method rewrite-ground-term;

// ----------------------------------------------------------------------------
"eof";

