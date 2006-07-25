//  -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: logic/dtp/aima-dtp.lisp
// ----------------------------------------------------------------------------
// 
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
// 
// 	Version history
//              2.10    Modified by Norvig to run in the AIMA system.
// 		2.09	Re-introduced forking conjunctions
// 		2.08	Merged answers and reduction answers
// 		2.07	Iteration, on subgoal and function depth
// 		2.06	Simplified conjunction solving (removed forward inf)
// 		2.05	TPTP Library tools
// 		2.04	General "view" tool on proofs and answers
// 		2.03	Reduction check at conjunct instead of subgoal
// 		2.02	Depth limits and iteration (Broken implementation)
// 		2.01	Corrected reduction inference combined with caching
// 		2.00	Subgoaling inference system
// 		1.00	Resolution-based inference system
// 
// 	Documentation Notes
// 
// 	Exported functions are intended to be called by users.  All other
// 	functions and symbols are intended to be internal.
// 
// 	Many files have a "public" and a "private" section.  Functions defined
// 	in the "private" section are ONLY called by functions within the same
// 	file.  All functions called by routines in other files are placed in
// 	the "public" section.
// 
// 	Function names: Both Common Lisp and Scheme conventions are
// 	occassionally followed.  Thus a suffix of "-P" or "?" indicates a
// 	predicate (testing for a condition).  A prefix of "N" or a suffix
// 	of "!" indicates a destructive function, which may alter its arguments.
"(in-package common-lisp-user)";

define module "DTP" use "COMMON-LISP"; end module DTP;

"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Options to change for new system installation
begin
  //  Trace feature
  //  Controls: Whether to include code and data structures for watching the
  //     inference in the middle of problem solving (and for examining
  //     proof spaces afterwards).  Most applications will want this option
  //     present, but some (e.g. an autonomous process) might wish the extra
  //     speed and smaller space, sacrificing user-friendlyness.
  *features* := add!(dtp-trace: *features*);
  //  Type feature
  //  Controls: Whether to declare data types of variables.  Some lisps can't
  //     handle this.
  *features* := add!(dtp-types: *features*);
end;

// ----------------------------------------------------------------------------
begin
  define variable *dtp-major-version* = 2;
  define variable *dtp-minor-version* = 10;
  //  aima change
  define variable *dtp-tracing-status* = "no tracing";
  define variable *dtp-typing-status* = "no types";
end;

begin
  define variable *dtp-version* =
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               begin
                 using-format(xp, "~D", pop!(args));
                 write-char++('.', xp);
                 using-format(xp, "~2,'0D", pop!(args));
                 write-string++(" [", xp, 0, 2);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-char++(',', xp);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-char++(']', xp);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(#f, *dtp-major-version*, *dtp-minor-version*,
                 *dtp-tracing-status*, *dtp-typing-status*);
  define module dtp export *dtp-version*; end module dtp;
end;

format-out("\nDTP version %S\n", *dtp-version*);

// ----------------------------------------------------------------------------
begin
  define variable *dtp-directory* = aima-file(#f, path: #("logic", "dtp"));
  //  aima change
  define variable *dtp-logic-directory* =
    aima-file(#f, path: #("logic", "dtp", "logic"));
  //  aima change
  define module dtp
    export *dtp-directory*, *dtp-logic-directory*;
  end module dtp;
end;

