//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Textify.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
// 
// 	Note		This entire file is conditional on #+dtp-trace
"(in-package dtp)";

// ----------------------------------------------------------------------------
define generic textify (object, #key answer, #"#all-keys") ;

// ----------------------------------------------------------------------------
define method textify (object :: <proof>, #key #all-keys)
  format(*show-stream*, "Proof of %S:\n:", object.proof-query);
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <answer>, #key #all-keys)
  format(*show-stream*, "%S:\n", apply-answer(object));
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <dtp-subgoal>, #key answer = #f,
                       #"#all-keys")
  block (return-from-textify)
    if (~ answer)
      indent-line(s: *show-stream*);
      print-literal-node(object.literal);
      format(*show-stream*, "\n");
      return-from-textify(#f);
    end if;
    indent-line(s: *show-stream*);
    print-literal-node(literal-plug(object.literal,
                                    answer.answer-binding-list),
                       s: *show-stream*);
    for (bl in answer.answer-ae-binding-lists)
      format(*show-stream*, " or ");
      print-literal-node(literal-plug(object.literal, bl), s: *show-stream*);
    end for;
    format(*show-stream*, "\n");
  end block;
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <dtp-conjunction>, #key answer = #f,
                       #"#all-keys")
  block (return-from-textify)
    if (~ answer) return-from-textify(#f); end if;
    let lookup-justifications = #f;
    lookup-justifications
     := remove(answer-justification(answer), complement(l-justification-p));
    for (l-just in lookup-justifications)
      indent-line(s: *show-stream*);
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   begin
                     push-char-mode(xp, #"cap1");
                     fluid-bind (*print-escape* = #f)
                       write+(pop!(args), xp);
                     end fluid-bind;
                     pop-char-mode(xp);
                   end;
                   write-string++(": ", xp, 0, 2);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(*show-stream*, l-just.l-just-id);
      print-clause-node-as-rule(kb-node-clause(find-kb-node-with-id(l-just
                                                                    .l-just-id)),
                                s: *show-stream*);
      format(*show-stream*, "\n");
    end for;
  end block;
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <dtp-conjunct>, #key answer = #f,
                       #"#all-keys")
  #f;
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <r-justification>, #key #all-keys)
  indent-line(s: *show-stream*);
  format(*show-stream*, "Reduction with %S\n",
         object.r-just-ancestor-subgoal.literal);
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <res-justification>, #key #all-keys)
  indent-line(s: *show-stream*);
  format(*show-stream*, "Residue of %S\n", object.res-just-assumable);
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <l-justification>, #key #all-keys)
  indent-line(s: *show-stream*);
  (method (s, #rest args)
     apply(maybe-initiate-xp-printing,
           method (xp, #rest args)
             begin
               write-string++("Lookup of ", xp, 0, 10);
               begin
                 push-char-mode(xp, #"cap1");
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 pop-char-mode(xp);
               end;
               write-string++(": ", xp, 0, 2);
             end;
             if (args) copy-sequence(args); end if;
           end method,
           s, args);
   end method)(*show-stream*, object.l-just-id);
  let node = find-kb-node-with-id(object.l-just-id);
  if (empty?(node))
    for (gn in *proof*.proof-goal-nodes)
      if (object.l-just-id == gn.kb-node-id) node := gn; end if;
    end for;
  end if;
  if (node)
    print-clause-node-as-rule(node.kb-node-clause, s: *show-stream*);
  end if;
  format(*show-stream*, "\n");
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <c-justification>, #key #all-keys)
  // No output for a c-justification, because nodes below
  #f;
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <s-cache-justification>, #key #all-keys)
  indent-line(s: *show-stream*);
  format(*show-stream*, "Success cache lookup of ");
  print-literal-node(object.s-cache-just-literal, s: *show-stream*);
  format(*show-stream*, "\n");
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <f-cache-justification>, #key #all-keys)
  indent-line(s: *show-stream*);
  format(*show-stream*, "Failure cache lookup of ");
  print-literal-node(object.f-cache-just-literal, s: *show-stream*);
  format(*show-stream*, "\n");
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <sg-cache-justification>, #key #all-keys)
  indent-line(s: *show-stream*);
  format(*show-stream*, "Subgoal cache lookup of ");
  print-literal-node(object.sg-cache-just-subgoal.literal, s: *show-stream*);
  format(*show-stream*, "\n");
end method textify;

// ----------------------------------------------------------------------------
define method textify (object :: <justification>, #key #all-keys)
  indent-line(s: *show-stream*);
  format(*show-stream*, "Unknown justification: %S\n", object);
end method textify;

// ----------------------------------------------------------------------------
"eof";

