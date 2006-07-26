//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Ordering.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

// ----------------------------------------------------------------------------
// 
// 	Public
// ----------------------------------------------------------------------------
define method norder-conjunctions (conjunctions)
  // Return all lookup answers first, then others
  if (*use-reordering*)
    partition(conjunctions, method (c) empty?(c.list); end method);
  else
    conjunctions;
  end if;
end method norder-conjunctions;

// ----------------------------------------------------------------------------
define method agenda-best ()
  // Return the first element on the agenda that doesn't violate a cutoff
  block (return)
    for (best-subgoal = select (*search-order*)
                          #"dfs"
                             => cl-find(complement(blocked-p),
                                        proof-subgoal-agenda(*proof*));
                          #"bfs"
                             => cl-find(complement(blocked-p),
                                        proof-subgoal-agenda(*proof*),
                                        from-end: #t);
                          otherwise
                             => #f;
                        end select then select (*search-order*)
                                        #"dfs"
                                         => cl-find(complement(blocked-p),
                                                    proof-subgoal-agenda(*proof*));
                                        #"bfs"
                                         => cl-find(complement(blocked-p),
                                                    proof-subgoal-agenda(*proof*),
                                                    from-end: #t);
                                        otherwise
                                         => #f;
                                        end select,
         until empty?(best-subgoal))
      if (~ good-subgoal-depth(best-subgoal))
        proof-subgoal-cutoff-occurred(*proof*) := #t;
        agenda-remove(best-subgoal);
      elseif (~ good-function-depth(best-subgoal))
        proof-function-cutoff-occurred(*proof*) := #t;
        agenda-remove(best-subgoal);
      else
        return(best-subgoal);
      end if;
    finally
      return(#f);
      #f;
    end for;
  end block;
end method agenda-best;

// ----------------------------------------------------------------------------
// 
// 	Private
// ----------------------------------------------------------------------------
define method good-subgoal-depth (subgoal)
  // T unless SUBGOAL is past valid depth limit
  block (return-from-good-subgoal-depth)
    if (~ (*use-subgoal-cutoffs* & proof-subgoal-depth-cutoff(*proof*)))
      return-from-good-subgoal-depth(#t);
    end if;
    subgoal.depth < proof-subgoal-depth-cutoff(*proof*);
  end block;
end method good-subgoal-depth;

// ----------------------------------------------------------------------------
define method good-function-depth (subgoal)
  // T unless SUBGOAL is past valid function limit
  block (return-from-good-function-depth)
    if (~ (*use-function-cutoffs* & proof-function-depth-cutoff(*proof*)))
      return-from-good-function-depth(#t);
    end if;
    function-depth(subgoal.literal) < proof-function-depth-cutoff(*proof*);
  end block;
end method good-function-depth;

// ----------------------------------------------------------------------------
define method user-choice (agenda)
  // Return some element from the agenda, selected by the user
  block (return-from-user-choice)
    format-out("\n");
    if (empty?(agenda))
      format-out("\n[Empty agenda, so no single step possible]\n");
      #f;
    elseif (empty?(tail(agenda)))
      format-out("\n[Solo agenda item, so choosing %S]\n", first(agenda));
      first(agenda);
    else
      for (count from 0 to 9, subgoal in agenda)
        format-out("%D: %S\n", count, subgoal);
      finally
        if (size(agenda) > 10) format-out("A: Show entire agenda\n"); end if;
        #f;
      end for;
      begin
        let choice = #f;
        for (until choice)
          format-out("Choice [0]? ");
          choice := read-line(*standard-input*, nil);
          if (size(choice) = 0)
            choice := 0;
          else
            choice
             := // LTD: Function READ-FROM-STRING not yet implemented.
                read-from-string(choice);
          end if;
          if (~ instance?(choice, <integer>))
            format-out("%S\n", agenda);
            choice := #f;
          elseif (choice >= 0 & choice < size(agenda))
            choice := agenda[choice];
          end if;
        finally
          return-from-user-choice(choice);
          #f;
        end for;
      end;
    end if;
  end block;
end method user-choice;

// ----------------------------------------------------------------------------
"eof";

