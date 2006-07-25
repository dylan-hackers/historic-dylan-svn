//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Output.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

define module dtp export show, settings, possible-settings; end module dtp;

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
define method show (object :: <symbol>)
  format-out("\n");
  for (node in theory-contents(object))
    (formatter-1("~A~15T"))(#t, kb-node-id(node));
    print-clause-node(kb-node-clause(node), as-rule: #t);
    format-out("\n");
  end for;
  values();
end method show;

// ----------------------------------------------------------------------------
define method settings ()
  // Display (and someday change) all the user-setable variables
  let count = 0;
  for (section in *user-setable-variables*)
    format-out("%S\n", first(section));
    for (var in tail(section))
      (formatter-1("   ~2D: ~:(~A~)"))(#t, inc!(count), var);
      if (not(instance?(var, <list>)))
        (formatter-1("~40T ~S~%"))(#t, var);
      else
        format-out("\n       %=\n", var);
      end if;
    end for;
  end for;
  values();
end method settings;

define method possible-settings ()
  // Display all the possible values for user-setable variables
  let count = 0;
  let possibles = #f;
  for (section in *user-setable-variables*)
    format-out("%S\n", first(section));
    for (var in tail(section))
      (formatter-1("   ~2D: ~:(~A~)"))(#t, inc!(count), var);
      possibles := possible-values(var);
      if (possibles)
        if (instance?(possibles, <string>))
          (formatter-1("~40T ~A~%"))(#t, possibles);
        else
          (formatter-1("~%       ~:(~S~)~%"))(#t, possibles);
        end if;
      elseif (instance?(var, <integer>))
        (formatter-1("~40T A non-negative integer~%"))(#t);
      elseif (var == #t | var == #f)
        (formatter-1("~40T Nil or T~%"))(#t);
      elseif (instance?(var, <symbol>))
        (formatter-1("~40T A symbol~%"))(#t);
      else
        (formatter-1("~40T [Unknown, but currently ~S]~%"))(#t, var);
      end if;
    end for;
  end for;
  values();
end method possible-settings;

// ----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------
nil(#f, nil(#f, nil(#f)),
    nil(nil(#f),
        nil(nil(nil(#f), #()), nil(#f, " ~A->", nil(nil(#f))),
            nil(nil(#f), #f))));

// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------
// 
// ----------------------------------------------------------------------------
"eof";

