//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File gps-srch.lisp: Section 6.4 GPS based on explicit search
requires("gps", "search");

define method search-gps (start, goal, #key beam-width = 10)
  // Search for a sequence of operators leading to goal.
  find-all-if(action-p,
              beam-search(pair(#(#"start"), start),
                          method (state)
                            subset?(goal, state, test: \=);
                          end method,
                          gps-successors,
                          method (state)
                            cl-count-if(action-p, state)
                             + cl-count-if(method (con)
                                           ~ member-equal(con, state);
                                           end method,
                                           goal);
                          end method,
                          beam-width));
end method search-gps;

define method gps-successors (state)
  // Return a list of states reachable from this one using ops.
  map(method (op)
        concatenate(choose(complement(method (x)
                                        member-equal(x, op-del-list(op));
                                      end method),
                           state),
                    op-add-list(op));
      end method,
      applicable-ops(state));
end method gps-successors;

define method applicable-ops (state)
  // Return a list of all ops that are applicable now.
  find-all-if(method (op)
                subset?(op-preconds(op), state, test: \=);
              end method,
              *ops*);
end method applicable-ops;

