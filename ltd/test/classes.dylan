//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Classes.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
define class <dtp-object> (<object>); end class <dtp-object>;

// ----------------------------------------------------------------------------
define class <dtp-proof-node> (<dtp-object>); end class <dtp-proof-node>;

// ----------------------------------------------------------------------------
define class <dtp-subgoal> (<dtp-proof-node>)
  slot literal :: type-union(<literal-node>, singleton(#f)) = #f,
       init-keyword: #"literal";
  slot answers :: <list> = #f;
  slot conjuncts-to-propagate-to :: <list> = #f,
       init-keyword: #"propagate-to";
  // Used for residue
  slot assumables
        :: type-union(<list>, // LTD: Can't convert type specification.
                      eql(#"uninitialized"))
        = #"uninitialized";
  // List of conjunction nodes
  slot inferences
        :: type-union(<list>, // LTD: Can't convert type specification.
                      eql(#"uninitialized"))
        = #"uninitialized";
  // Conjunction nodes that are waiting for another subgoal
  slot blocked-conjunctions :: <list> = #f;
  //  Cached values (could be computed, but expensive)
  // Length of minimal path from root to this node
  slot depth :: type-union(singleton(#f), limited(<integer>, min: 0, max: \*))
        = #f,
       init-keyword: #"depth";
  // Used for reduction computation
  slot remaining-ancestor-subgoals
        :: type-union(<list>, // LTD: Can't convert type specification.
                      eql(#"uninitialized"))
        = #"uninitialized";
  // Upward pointer in proof space
  slot parent-subgoal :: type-union(singleton(#f), <dtp-subgoal>) = #f;
  // Upward pointer in proof space
  slot parent-conjunct :: type-union(singleton(#f), <dtp-conjunct>) = #f;
end class <dtp-subgoal>;

define method print-object (object :: <dtp-subgoal>, stream)
  begin
    format(stream, "#<DTP Subgoal ");
    if (literal-node-p(object.literal))
      print-literal-node(object.literal, s: stream);
    else
      format(stream, "?");
    end if;
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               let init = args;
               begin
                 write-string++(" with ", xp, 0, 6);
                 using-format(xp, "~D", pop!(args));
                 write-string++(" answer", xp, 0, 7);
                 if (~ (head(backup-in-list(1, init, args)) == 1))
                   write-char++('s', xp);
                 end if;
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(stream, size(object.answers));
    if (instance?(object.inferences, <list>))
      if (object.inferences)
        (method (s, #rest args)
           apply(maybe-initiate-xp-printing,
                 method (xp, #rest args)
                   let init = args;
                   begin
                     write-string++(" [", xp, 0, 2);
                     using-format(xp, "~D", pop!(args));
                     write-string++(" task", xp, 0, 5);
                     if (~ (head(backup-in-list(1, init, args)) == 1))
                       write-char++('s', xp);
                     end if;
                     write-string++(" pending]", xp, 0, 9);
                   end;
                   if (args) copy-sequence(args); end if;
                 end method,
                 s, args);
         end method)(stream, size(object.inferences));
      else
        format(stream, " [complete]");
      end if;
    end if;
    format(stream, ">");
  end;
end method print-object;

// ----------------------------------------------------------------------------
define class <dtp-conjunction> (<dtp-proof-node>)
  // List of conjuncts
  slot list :: <list> = #f, init-keyword: #"list";
  slot stack = #f;
  // Number of current conjunct
  slot stack-pointer :: limited(<integer>, min: -1, max: \*) = 0;
  // Conjuncts from 0 to here must not be backjumped over
  slot backtrack-pointer :: limited(<integer>, min: -1, max: \*) = -1;
  // Parent of NIL means this is a query-conjunction
  slot parent-subgoal :: type-union(<dtp-subgoal>, singleton(#f)) = #f,
       init-keyword: #"parent";
  // From inference
  slot binding-list :: <binding-list> = #f, init-keyword: #"binding-list";
  // From inference
  slot label :: type-union(<label>, singleton(#f)) = #f,
       init-keyword: #"label";
  // Only when summarizing for instance conjunction
  slot residue :: <list> = #f, init-keyword: #"residue";
  // Needed for disjunctive answers via answer extraction
  slot ae-binding-list :: <binding-list> = #f,
       init-keyword: #"ae-binding-list";
end class <dtp-conjunction>;

define method print-object (object :: <dtp-conjunction>, stream)
  begin
    format(stream, "#<DTP Conjunction");
    if (object.list)
      format(stream, " [%D]:", object.stack-pointer);
      for (conjunct in object.list)
        if (instance?(conjunct, <dtp-conjunct>))
          begin
            format(stream, " ");
            if (literal-node-p(conjunct.literal))
              print-literal-node(conjunct.literal, s: stream);
            else
              format(stream, "?");
            end if;
            format(stream, "/%D", conjunct.answer-count);
          end;
        end if;
      end for;
    end if;
    format(stream, ">");
  end;
end method print-object;

// ----------------------------------------------------------------------------
define class <dtp-forked-conjunction> (<dtp-conjunction>)
  // Don't backtrack past this conjunct
  slot top-conjunct :: <natural-number>;
end class <dtp-forked-conjunction>;

define method print-object (object :: <dtp-forked-conjunction>, stream)
  begin
    format(stream, "#<DTP F [%D] Conjunction", object.top-conjunct);
    if (object.list)
      format(stream, " [%D]:", object.stack-pointer);
      for (conjunct in object.list, count from 0)
        if (instance?(conjunct, <dtp-conjunct>))
          begin
            format(stream, " ");
            if (~ literal-node-p(conjunct.literal))
              format(stream, "?");
            elseif (count < object.top-conjunct)
              format(stream, "[");
              print-literal-node(literal-plug(conjunct.literal,
                                              conjunct.binding-list),
                                 s: stream);
              format(stream, "]");
            else
              print-literal-node(conjunct.literal, s: stream);
              format(stream, "/%D", conjunct.answer-count);
            end if;
          end;
        end if;
      end for;
    end if;
    format(stream, ">");
  end;
end method print-object;

// ----------------------------------------------------------------------------
define class <dtp-conjunct> (<dtp-proof-node>)
  slot literal :: type-union(<literal-node>, singleton(#f)) = #f,
       init-keyword: #"literal";
  // Must be initialized when first created
  slot parent-conjunction :: <dtp-conjunction>, init-keyword: #"parent";
  // Apply to literal, then search for subgoal
  slot binding-list :: <list> = #f;
  // Apply to answers from subgoal before valid
  slot transform-binding-list :: <list> = #f;
  slot answer-count :: limited(<integer>, min: 0, max: \*) = 0;
  slot subgoal = #"uninitialized";
  // Backjumping dependency analysis
  slot nogoods
        :: type-union(<list>, // LTD: Can't convert type specification.
                      eql(#"uninitialized"))
        = #"uninitialized";
end class <dtp-conjunct>;

define method print-object (object :: <dtp-conjunct>, stream)
  begin
    format(stream, "#<DTP Conjunct ");
    if (literal-node-p(object.literal))
      print-literal-node(object.literal, s: stream);
    else
      format(stream, "?");
    end if;
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               let init = args;
               begin
                 write-string++(" with ", xp, 0, 6);
                 using-format(xp, "~D", pop!(args));
                 write-string++(" answer", xp, 0, 7);
                 if (~ (head(backup-in-list(1, init, args)) == 1))
                   write-char++('s', xp);
                 end if;
                 write-char++('>', xp);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(stream, object.answer-count);
  end;
end method print-object;

// ----------------------------------------------------------------------------
"eof";

