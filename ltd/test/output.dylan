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
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               begin
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 pprint-tab+(line: 15, 1, xp);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(#t, kb-node-id(node));
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
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   write-string++("   ", xp, 0, 3);
                   using-format(xp, "~2D", pop!(args));
                   write-string++(": ", xp, 0, 2);
                   begin
                     push-char-mode(xp, #"cap1");
                     fluid-bind (*print-escape* = #f)
                       write+(pop!(args), xp);
                     end fluid-bind;
                     pop-char-mode(xp);
                   end;
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(#t, inc!(count), var);
      if (not(instance?(var, <list>)))
        (method (s, #rest args)
           apply(maybe-initiate-xp-printing,
                 method (xp, #rest args)
                   begin
                     pprint-tab+(line: 40, 1, xp);
                     write-char++(' ', xp);
                     fluid-bind (*print-escape* = #t)
                       write+(pop!(args), xp);
                     end fluid-bind;
                     pprint-newline+(unconditional: xp);
                   end;
                   if (args) copy-sequence(args); end if;
                 end method,
                 s, args);
         end method)(#t, var);
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
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   write-string++("   ", xp, 0, 3);
                   using-format(xp, "~2D", pop!(args));
                   write-string++(": ", xp, 0, 2);
                   begin
                     push-char-mode(xp, #"cap1");
                     fluid-bind (*print-escape* = #f)
                       write+(pop!(args), xp);
                     end fluid-bind;
                     pop-char-mode(xp);
                   end;
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(#t, inc!(count), var);
      possibles := possible-values(var);
      if (possibles)
        if (instance?(possibles, <string>))
          (method (s, #rest args)
             apply(maybe-initiate-xp-printing,
                   method (xp, #rest args)
                     begin
                       pprint-tab+(line: 40, 1, xp);
                       write-char++(' ', xp);
                       fluid-bind (*print-escape* = #f)
                         write+(pop!(args), xp);
                       end fluid-bind;
                       pprint-newline+(unconditional: xp);
                     end;
                     if (args) copy-sequence(args); end if;
                   end method,
                   s, args);
           end method)(#t, possibles);
        else
          (method (s, #rest args)
             apply(maybe-initiate-xp-printing,
                   method (xp, #rest args)
                     begin
                       pprint-newline+(unconditional: xp);
                       write-string++("       ", xp, 0, 7);
                       begin
                         push-char-mode(xp, #"cap1");
                         fluid-bind (*print-escape* = #t)
                           write+(pop!(args), xp);
                         end fluid-bind;
                         pop-char-mode(xp);
                       end;
                       pprint-newline+(unconditional: xp);
                     end;
                     if (args) copy-sequence(args); end if;
                   end method,
                   s, args);
           end method)(#t, possibles);
        end if;
      elseif (instance?(var, <integer>))
        (method (s, #rest args)
           apply(maybe-initiate-xp-printing,
                 method (xp, #rest args)
                   begin
                     pprint-tab+(line: 40, 1, xp);
                     write-string++(" A non-negative integer", xp, 0, 23);
                     pprint-newline+(unconditional: xp);
                   end;
                   if (args) copy-sequence(args); end if;
                 end method,
                 s, args);
         end method)(#t);
      elseif (var == #t | var == #f)
        (method (s, #rest args)
           apply(maybe-initiate-xp-printing,
                 method (xp, #rest args)
                   begin
                     pprint-tab+(line: 40, 1, xp);
                     write-string++(" Nil or T", xp, 0, 9);
                     pprint-newline+(unconditional: xp);
                   end;
                   if (args) copy-sequence(args); end if;
                 end method,
                 s, args);
         end method)(#t);
      elseif (instance?(var, <symbol>))
        (method (s, #rest args)
           apply(maybe-initiate-xp-printing,
                 method (xp, #rest args)
                   begin
                     pprint-tab+(line: 40, 1, xp);
                     write-string++(" A symbol", xp, 0, 9);
                     pprint-newline+(unconditional: xp);
                   end;
                   if (args) copy-sequence(args); end if;
                 end method,
                 s, args);
         end method)(#t);
      else
        (method (s, #rest args)
           apply(maybe-initiate-xp-printing,
                 method (xp, #rest args)
                   begin
                     pprint-tab+(line: 40, 1, xp);
                     write-string++(" [Unknown, but currently ", xp, 0, 25);
                     fluid-bind (*print-escape* = #t)
                       write+(pop!(args), xp);
                     end fluid-bind;
                     write-char++(']', xp);
                     pprint-newline+(unconditional: xp);
                   end;
                   if (args) copy-sequence(args); end if;
                 end method,
                 s, args);
         end method)(#t, var);
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

