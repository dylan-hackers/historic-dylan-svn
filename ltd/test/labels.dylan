//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Labels.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
define class <label-structure> (<object>)
  slot label-structure-name, init-keyword: #"label-structure-name";
  slot label-structure-maximum, init-keyword: #"label-structure-maximum";
  slot label-structure-minimum, init-keyword: #"label-structure-minimum";
  slot label-structure-and-fn, init-keyword: #"label-structure-and-fn";
  slot label-structure-or-fn, init-keyword: #"label-structure-or-fn";
end class <label-structure>;

define class <label> (<object>)
  slot label-value, init-keyword: #"label-value";
  slot label-structure, init-keyword: #"label-structure";
end class <label>;

define method label-print-function (structure, stream, depth)
  format(stream, "<Label %S>", structure.label-value);
end method label-print-function;

// ----------------------------------------------------------------------------
define variable *label-structures* = #f;

// ----------------------------------------------------------------------------
define method get-label-structure (name)
  cl-find(name, *label-structures*, key: label-structure-name);
end method get-label-structure;

// ----------------------------------------------------------------------------
// LTD: No macros.
#"create-label-structure";

// ----------------------------------------------------------------------------
define method label-equal-p (label1, label2)
  if (empty?(label1) | empty?(label2))
    empty?(label1) & empty?(label2);
  else
    label1.label-value = label2.label-value
     & label1.label-structure == label2.label-structure;
  end if;
end method label-equal-p;

// ----------------------------------------------------------------------------
define method label-instance? (instance, general)
  // True IFF INSTANCE is more specific (or equal) to GENERAL
  if (empty?(general))
    #t;
  elseif (empty?(instance))
    #f;
  else
    instance.label-value = general.label-value
     & instance.label-structure == general.label-structure;
  end if;
end method label-instance?;

// ----------------------------------------------------------------------------
define method label-and (label1, label2)
  if (empty?(label1))
    label2;
  elseif (empty?(label2))
    label1;
  else
    begin
      let ls1 = label1.label-structure;
      let ls2 = label2.label-structure;
      if (ls1 & ls2 & ls1 == ls2)
        make-label(value: apply(ls1.label-structure-and-fn,
                                label1.label-value, label2.label-value),
                   structure: ls1);
      else
        make-label(value: #"error", structure: #f);
      end if;
    end;
  end if;
end method label-and;

// ----------------------------------------------------------------------------
// 
// 	First-order logic
// 
// 	Values: T (for true), NIL (for false)
define method fo-and (#rest labels)
  // LTD: Function EVAL not yet implemented.
  eval(pair(#"and", labels));
end method fo-and;

define method fo-or (#rest labels)
  // LTD: Function EVAL not yet implemented.
  eval(pair(#"or", labels));
end method fo-or;

create-label-structure(first-order, #t, #f, fo-and, fo-or);

// ----------------------------------------------------------------------------
// 
// 	Qualitative likelihoods
define variable *ql-values* =
  #(#"false", #"default-false", #"unknown", #"default-true", #"true");

define method ql-and (#rest labels)
  labels
   := map(method (x)
            find-key(*ql-values*, curry(\==, x.label-value));
          end method,
          labels);
  *ql-values*[apply(min, labels)];
end method ql-and;

define method ql-or (#rest labels)
  labels
   := map(method (x)
            find-key(*ql-values*, curry(\==, x.label-value));
          end method,
          labels);
  *ql-values*[apply(max, labels)];
end method ql-or;

create-label-structure(qualitative-likelihoods, true, false, ql-and, ql-or);

// ----------------------------------------------------------------------------
// 
// 	Fuzzy Logic
// 
// 	[From Charles Elkan, _The Paradoxical Success of Fuzzy Logic_,
// 	 AAAI-93, pp 698-703.]
// 
// 	1. Truth value t(A) of an assertion A: 0 <= t(A) <= 1
// 	2. t(A ^ B) = min{t(A),t(B)}
// 	3. t(A v B) = max{t(A),t(B)}
// 	4. t(- A) = 1 - t(A)
// 	5. t(A) = t(B) if A and B are logically equivalent
create-label-structure(fuzzy-logic, 1, 0, max, min);

// ----------------------------------------------------------------------------
// 
// 	Mycin Certainty Factors
// 
// (create-label-structure
//  mycin
//  1 -1 mycin-and mycin-or )
// 
// ----------------------------------------------------------------------------
"eof";

