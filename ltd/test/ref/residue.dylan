//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Residue.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Public
define method residue-answer (subgoal, assumable)
  if (literal-instance(assumable, subgoal.literal))
    make-answer(residue: list(subgoal.literal));
  end if;
end method residue-answer;

// ----------------------------------------------------------------------------
define method residue-merge (residue1, residue2)
  let new-res = concatenate(residue1, residue2);
  if (proof-consistency-check(*proof*))
    if ((proof-consistency-check(*proof*))(new-res))
      new-res;
    else
      #"not-a-residue";
    end if;
  else
    new-res;
  end if;
end method residue-merge;

// ----------------------------------------------------------------------------
define method residue-equal-p (residue1, residue2)
  set-equal(residue1, residue2, literal-equal-p);
end method residue-equal-p;

// ----------------------------------------------------------------------------
define method residue-instance? (instance, general)
  // True IFF the INSTANCE residue is more specific than (or equal to) GENERAL
  subset?(general, instance, test: literal-instance?);
end method residue-instance?;

// ----------------------------------------------------------------------------
"eof";

