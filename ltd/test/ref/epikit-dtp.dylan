//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Epikit-DTP.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
// 	Purpose		To help with replacing Epikit by DTP
"(in-package dtp)";

define module dtp
  export knownp, proval, remval, prologp, prologx, prologs, save, drop, empty, facts, contents, brf;
end module dtp;

// ----------------------------------------------------------------------------
define method knownp (fact, theory)
  //  matchp (DB can be more specific)
  "Actually only called with FACT of form (rel ?v1 ?v2 ...)";
  ~ empty?(active-theory-contents(theory, head(fact)));
end method knownp;

define method proval (fact, theory)
  // Actually, no term inference, only equality lookup
  fluid-bind (*theory* = theory)
    prove(list(#"=", fact, #"?x"), return-form: #"?x");
  end fluid-bind;
end method proval;

define method remval (fact, theory)
  drop-sentence-from-theory(list(#"=", fact, proval(fact, theory)),
                            theory-name: theory);
end method remval;

define method prologp (fact, theory)
  fluid-bind (*theory* = theory) prove(fact); end fluid-bind;
end method prologp;

define method prologx (expr, fact, theory)
  fluid-bind (*theory* = theory)
    prove(fact, return-form: expr);
  end fluid-bind;
end method prologx;

define method prologs (expr, fact, theory)
  fluid-bind (*theory* = theory)
    prove(fact, all-answers: #t, return-form: expr);
  end fluid-bind;
end method prologs;

define method save (fact, theory)
  //  samep
  save-sentence-in-theory(fact, theory-name: theory);
end method save;

define method drop (fact, theory)
  //  samep
  drop-sentence-from-theory(fact, theory-name: theory);
end method drop;

define method empty (theory) empty-theory(theory); end method empty;

define method facts (atom, theory)
  sentences-in(theory, with-atom: atom);
end method facts;

define method contents (theory) sentences-in(theory); end method contents;

define method brf (fact)
  let cnf = #f;
  cnf
   := map(method (dnf)
            pair(#"<=",
                 pair(first(dnf),
                      map(method (lit)
                            if (#"not" == first(lit))
                              second(lit);
                            else
                              list(#"not", lit);
                            end if;
                          end method,
                          tail(dnf))));
          end method,
          cnf(fact));
  if (tail(cnf)) pair(#"and", cnf); else first(cnf); end if;
end method brf;

// ----------------------------------------------------------------------------
"eof";

