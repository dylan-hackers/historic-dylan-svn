//  -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;
//  Copyright 1992 Patrick H. Winston.  All rights reserved.
//  Version 1.1.1, copied from master file on 23 Apr 93       
//  
//  This software is licensed by Patrick H. Winston (licensor) for
//  instructional use with the textbooks ``Artificial Intelligence,'' by
//  Patrick H. Winston, and ``Lisp,'' by Patrick H. Winston and Berthold
//  K. P. Horn.  Your are free to make copies of this software and
//  modify it for such instructional use as long as:
//  1. You keep this notice intact.
//  2. You cause any modified files to carry a prominent notice stating
//     that you modified the files and the date of your modifications.
//  This software is licensed ``AS IS'' without warranty and the licensor
//  shall have no liability for any alleged defect or damages.
//  STRUCTURES AND ACCESSORS
// 
//   Purpose:	Capture the information needed for each node in the kd tree.
//
define class <node> (<object>)
  slot node-samples, init-keyword: #"node-samples";
  // The samples seen at the node.
  slot node-count, init-keyword: #"node-count";
  // The number of samples.
  slot node-dimension, init-keyword: #"node-dimension";
  // The dimension that the node looks at.
  slot node-left-max, init-keyword: #"node-left-max";
  // The maximum of the "small" samples.
  slot node-left-samples, init-keyword: #"node-left-samples";
  // The "small" samples or branch.
  slot node-right-min, init-keyword: #"node-right-min";
  // The minimum of the "large" samples.
  slot node-right-samples, init-keyword: #"node-right-samples";
end class <node>;

//  Access Functions for Records Stored in List of Samples
define method record-name (record) first(record); end method record-name;

define method record-attribute (record)
  second(record);
end method record-attribute;

define method record-position (record)
  nth-tail(record, 2);
end method record-position;

//  Constructor for an Unknown
// LTD: No macros.
#"make-record-for-unknown";

//  Access Functions for Discovered Answers
define method make-answer (record, distance2)
  list(record-attribute(record), distance2);
end method make-answer;

define method answer-attribute (answer)
  first(answer);
end method answer-attribute;

define method answer-distance2 (answer)
  second(answer);
end method answer-distance2;

//  IDENTIFICATION
define method identify (unknown, tree, #key count = 1,
                        weighting-function = \/)
  // 
  //   Purpose:	To guess an attribute using nearest neighbor idea.
  //   Arguments:	UNKNOWN: a record, made using MAKE-RECORD-FOR-UNKNOWN.
  // 		TREE: a KD tree.
  // 		COUNT: the number of nearest neighbors to be used.
  // 		WEIGHTING-FUNCTION: a function that establishes the way
  // 				    a neighbor's influence is diminished
  // 				    by distance.
  //   Returns:	The best guess for an unknown's unknown attribute.
  //
  let candidates = find-answers(unknown, tree, count);
  if (zero?(answer-distance2(first(candidates))))
    let winner = answer-attribute(first(candidates));
    format-out("\nThe winner is %S (exact match).", winner);
    winner;
  else
    let best-pair = tally-votes(candidates, weighting-function);
    let winner = first(best-pair);
    let score = second(best-pair);
    format-out("\nThe winner is %S with %S votes.", winner, score);
    winner;
  end if;
end method identify;

define method tally-votes (attribute-distance-pairs, weighting-function)
  // 
  //   Purpose:	Combines evidence when there are multiple nearest neighbors.
  //   Returns:	Attribute with the most votes.
  //
  let attribute-weight-pairs = #f;
  let attribute-score-pairs = #f;
  // Make a-list pairs in which first element is an object's attribute 
  // and the second element is the distance-determined influence of
  // that object attribute.
  attribute-weight-pairs
   := map(method (e)
            list(answer-attribute(e),
                 as(weighting-function(answer-distance2(e)), <float>));
          end method,
          attribute-distance-pairs);
  // Make a-list pairs in which first element is an attribute and the
  // second element is the sum of the influences of the objects
  // with that attribute:
  for (item in attribute-weight-pairs)
    let handle = cl-assoc(first(item), attribute-score-pairs);
    if (handle)
      inc!(second(handle), second(item));
    else
      push!(item, attribute-score-pairs);
    end if;
  end for;
  // Sort, with the most recommended attribute in front:
  attribute-score-pairs
   := sort!(attribute-score-pairs,
            test: method (x, y) second(x) > second(y); end method);
  // Pick the winner off the front:
  //
  nil(#f);
end method tally-votes;

//  FIND NEAREST NEIGHBORS
define method find-answers (unknown, tree, #key count = 1, level = 0)
  // 
  //   Purpose:	Find N nearest neighbors using KD tree.
  //   Returns:	N answer structures; each has a block slot and a distance slot.
  //   Remarks:	This procedure is complicated, in part, because
  // 		it has to deal with multiple nearest neighbors.
  //
  let dimension = tree.node-dimension;
  let projection = record-position(unknown)[dimension];
  let left-delta2 = delta2(projection, tree.node-left-max);
  let right-delta2 = delta2(projection, tree.node-right-min);
  let threshold-delta2 = #f;
  let winning-branch = #f;
  let losing-branch = #f;
  // Decide which branch has won and set variables accordingly:
  if (right-delta2 < left-delta2)
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               begin
                 pprint-newline+(fresh: xp);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-string++("Turn toward large numbers in dimension ", xp,
                                0, 39);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-string++(": ", xp, 0, 2);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-string++(" is closer to ", xp, 0, 14);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-string++(" than to ", xp, 0, 9);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-char++('.', xp);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(#t, indent(level), dimension, projection,
                 tree.node-right-min, tree.node-left-max);
    begin
      threshold-delta2 := left-delta2;
      winning-branch := tree.node-right-samples;
      losing-branch := tree.node-left-samples;
    end;
  else
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               begin
                 pprint-newline+(fresh: xp);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-string++("Turn toward small numbers in dimension ", xp,
                                0, 39);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-string++(": ", xp, 0, 2);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-string++(" is closer to ", xp, 0, 14);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-string++(" than to ", xp, 0, 9);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-char++('.', xp);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(#t, indent(level), dimension, projection, tree.node-left-max,
                 tree.node-right-min);
    begin
      threshold-delta2 := right-delta2;
      winning-branch := tree.node-left-samples;
      losing-branch := tree.node-right-samples;
    end;
  end if;
  let winning-answers = #f;
  let losing-answers = #f;
  let nearest-winning-distance2 = #f;
  // If the winning branch is a node, then find the
  // closest neighbor by calling FIND-ANSWERS recursively;
  // Otherwise the winning branch is NOT a node, and the winning
  // branch IS the closest neighbor:
  begin
    winning-answers
     := if (node-p(winning-branch))
          find-answers(unknown, winning-branch, count, level + 1);
        else
          list(make-answer(winning-branch,
                           distance2(unknown, winning-branch)));
        end if;
    nearest-winning-distance2
     := answer-distance2(first(copy-sequence(winning-answers,
                                             start: size(winning-answers)
                                                     - 1)));
  end;
  // At this point, FIND-ANSWERS needs to check the nearest answer
  // by comparing the actual distance between the unknown and
  // the nearest answer with the one-dimensional distance between
  // the unknown and the nearest answer on the wrong side of
  // the gap between the left and right groups; also, there
  // may not yet be enough answers:
  if (nearest-winning-distance2 <= threshold-delta2
       & size(winning-answers) >= count)
    // If the answer holds up, done:
    winning-answers;
  else
    // Indicate why there is more work to do:
    if (nearest-winning-distance2 <= threshold-delta2)
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   pprint-newline+(fresh: xp);
                   fluid-bind (*print-escape* = #f)
                     write+(pop!(args), xp);
                   end fluid-bind;
                   write-string++("Trying alternate branch because too few answers ",
                                  xp, 0, 48);
                   write-char++('[', xp);
                   fluid-bind (*print-escape* = #f)
                     write+(pop!(args), xp);
                   end fluid-bind;
                   write-string++(" <= ", xp, 0, 4);
                   fluid-bind (*print-escape* = #f)
                     write+(pop!(args), xp);
                   end fluid-bind;
                   write-string++("].", xp, 0, 2);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(#t, indent(level), size(winning-answers), count);
    else
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   pprint-newline+(fresh: xp);
                   fluid-bind (*print-escape* = #f)
                     write+(pop!(args), xp);
                   end fluid-bind;
                   write-string++("Trying other branch because worst answer ",
                                  xp, 0, 41);
                   write-string++("is not good enough [", xp, 0, 20);
                   fluid-bind (*print-escape* = #f)
                     write+(pop!(args), xp);
                   end fluid-bind;
                   write-string++(" > ", xp, 0, 3);
                   fluid-bind (*print-escape* = #f)
                     write+(pop!(args), xp);
                   end fluid-bind;
                   write-string++("].", xp, 0, 2);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(#t, indent(level), nearest-winning-distance2,
                   threshold-delta2);
    end if;
    // Establish best answers on the losing branch of the tree:
    losing-answers
     := if (node-p(losing-branch))
          find-answers(unknown, losing-branch, count, level + 1);
        else
          list(make-answer(losing-branch, distance2(unknown, losing-branch)));
        end if;
    // Combine all answers and sort:
    winning-answers
     := sort!(concatenate(winning-answers, losing-answers),
              test: method (x, y)
                      answer-distance2(x) < answer-distance2(y);
                    end method);
    // Prune if too many answers:
    winning-answers
     := copy-sequence(winning-answers, size(winning-answers) - 1);
    // Finally, announce and return the answers:
    //
    #f;
  end if;
end method find-answers;

//  CONSTRUCT TREE
define method grow-tree (samples, #key level = 1)
  // 
  //   Purpose:	Construct a KD tree from samples.
  //   Returns:	A tree node (a structure).
  //   Arguments:	Level set to 1 above to ensure comparison along y axis first.
  //
  if (size(samples) < 2)
    first(samples);
  else
    let dimension-count = size(record-position(first(samples)));
    let possibilities = #f;
    winner
     := make-node-for-samples(// Select dimension for comparison:
                              let (ignore, remainder)
                                  = floor/(level, dimension-count);
                                remainder, samples);
    // Increment level and recurse along both branches:
    inc!(level);
    winner.node-left-samples := grow-tree(winner.node-left-samples, level);
    winner.node-right-samples := grow-tree(winner.node-right-samples, level);
    winner;
  end if;
end method grow-tree;

define method make-node-for-samples (dimension, samples)
  // 
  //   Purpose:	Sorts samples along dimension supplied and creates a node.
  //   Returns:	A tree node (a structure).
  //
  samples
   := sort!(samples,
            test: method (x, y)
                    record-position(x)[dimension]
                                        < record-position(y)[dimension];
                  end method);
  let count = size(samples);
  let left-size = floor/(count, 2);
  let node
      = make-node(samples: samples, dimension: dimension, count: count,
                  left-max: record-position(samples[left-size
                                                     - 1])[dimension],
                                                           right-min: record-position(samples[left-size])[dimension],
                                                                                                          left-samples: copy-sequence(samples,
                                                                                                                                      size(samples)
                                                                                                                                       - 1),
                                                                                                          right-samples: nth-tail(samples,
                                                                                                                                  left-size));
  node;
end method make-node-for-samples;

//  DISPLAY TREE
define method display-tree (node, #key level = 0, branch)
  // 
  //   Purpose:	Display a tree using indentation to indicate level.
  //   Arguments:	The root node of the tree.
  //
  format-out("\n%SNode splits %S %S samples %S [%S %S %S]", indent(level),
             if (branch) branch; else "top"; end if, node.node-count,
             if (zero?(node.node-dimension))
               "vertically";
             else
               "horizontally";
             end if,
             node.node-left-max,
             as((node.node-left-max + node.node-right-min) / 2, <float>),
             node.node-right-min);
  if (node-p(node.node-left-samples))
    display-tree(node.node-left-samples, level + 1, "left branch");
  end if;
  if (node-p(node.node-right-samples))
    display-tree(node.node-right-samples, level + 1, "right branch");
  end if;
end method display-tree;

//  AUXILIARIES
define method distance2 (a, b)
  // 
  //   Purpose:	Computes distance between two records.
  //
  reduce1(\+, map(delta2, a-size, b-size));
end method distance2;

define method delta2 (x1, x2)
  let delta = x1 - x2;
  delta * delta;
end method delta2;

define method indent (depth)
  make(<string>, size: depth, fill: ' ');
end method indent;

define method one-no-matter-what (#rest ignore)
  1;
end method one-no-matter-what;

