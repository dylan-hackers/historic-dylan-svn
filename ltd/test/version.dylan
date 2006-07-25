//  Copyright 1994, Brown University, Providence, RI
//  See end of file for full copyright information
//  (in-package 'user)
//  This file includes all of the code for implementing the 
//  version spaces method described in the learning chapter.
//  The code also includes a solution to the exercise that 
//  prunes redundant concepts from the boundaries.
//  Abstract data types
//  A FEATURE is an attribute together with a value.
define method feature-attribute (feature)
  head(feature);
end method feature-attribute;

define method feature-value (feature) second(feature); end method feature-value;

//  A DIMENSION is an attribute together with a set of possible values.
define method make-dimension (attribute, values)
  list(attribute, values);
end method make-dimension;

define method dimension-attribute (dimension)
  head(dimension);
end method dimension-attribute;

define method dimension-values (dimension)
  second(dimension);
end method dimension-values;

//  An EXAMPLE is an identifier together with a set of features 
//  that describe the example and a class indicator.
define method make-example (id, features, class)
  list(id, features, class);
end method make-example;

define method example-id (example) head(example); end method example-id;

define method example-features (example)
  second(example);
end method example-features;

define method example-class (example) third(example); end method example-class;

//  The function FINDALL takes a list and a function and returns 
//  all items in the list such that the function returns non NIL 
//  when applied to the item.
define method findall (items, test)
  for (items = items then cdr(items), results = list() then list(),
       until empty?(items))
    if (test(head(items)))
      results := add!(head(items), results, test: \=);
    end if;
  finally
    results;
  end for;
end method findall;

//  REFINE the general and specific version-space boundaries.  In the
//  following, we provide code for modifying the boundaries in a version
//  space given a new training example. The code provided below is not
//  guaranteed to converge to a unique concept. In the exercises, you are
//  shown how to modify the code to implement the complete algorithm given
//  in the text. Refine takes a single training example and two lists of
//  concepts representing the general and specific boundaries. A
//  conjunctive concept is represented as a list of features. Refine
//  determines whether the training example is positive, yes, or not, no,
//  and then proceeds to, respectively, generalize the concepts in the
//  specific bounary or specialize the concepts in the general boundary.
define method refine (example, general, specific)
  if (example-class(example) == #"yes")
    simplify-specific(prune-general(example, general),
                      generalize-specific(example, specific));
  else
    simplify-general(specialize-general(example, general),
                     prune-specific(example, specific));
  end if;
end method refine;

//  Here is a simplified version of REFINE that produces boundaries
//  that include redundant and useless concepts.  This version does 
//  not guarantee convergence.  An example is within the boundaries
//  just in case it is consistent with one concept in the general 
//  boundary and one concept in the specific boundary such that 
//  the specific concept specializes the general concept.
//  (defun refine (example general specific) 
//    (if (eq (EXAMPLE-class example) 'yes)
//        (list general (generalize-specific example specific))
//      (list (specialize-general example general) specific)))
//  Include in the general boundary only those concepts that 
//  are consistent with the positive examples.
define method prune-general (example, general)
  findall(general,
          method (concept)
            consistent(example-features(example), concept);
          end method);
end method prune-general;

//  Include in the specific boundary only those concepts that are 
//  not consistent with the negative examples.
define method prune-specific (example, specific)
  findall(specific,
          method (concept)
            ~ consistent(example-features(example), concept);
          end method);
end method prune-specific;

//  Positive examples serve to generalize the specific boundary.
//  Generalizing the specific boundary consists of generalizing each 
//  concept in the boundary until it is consistent with the (positive) 
//  example.
define method generalize-specific (example, boundary)
  apply(concatenate!,
        map(method (c) aux-generalize-specific(example, c); end method,
            boundary));
end method generalize-specific;

define method aux-generalize-specific (example, concept)
  if (consistent(example-features(example), concept))
    list(concept);
  else
    generalize-specific(example, generalize(concept));
  end if;
end method aux-generalize-specific;

//  Negative examples serve to specialize the general boundary.
//  Specializing the general boundary consists of specializing 
//  each concept in the boundary until it is not consistent 
//  with the (negative) example.
define method specialize-general (example, boundary)
  apply(concatenate!,
        map(method (c) aux-specialize-general(example, c); end method,
            boundary));
end method specialize-general;

define method aux-specialize-general (example, concept)
  if (~ consistent(example-features(example), concept))
    list(concept);
  else
    specialize-general(example, specialize(concept));
  end if;
end method aux-specialize-general;

//  Determine whether a description is consistent with a given concept.  
//  A concept is consistent with an example if for each feature (conjunct)
//  in the concept, either the attribute of the feature is not mentioned
//  in the list of features describing the example, or, if it is
//  mentioned, the corresponding feature has the same attribute value. 
//  For instance, if an example corresponds to an office on the third 
//  floor, then any concept consistent with that example must either 
//  not mention a floor or indicate the third floor.
define method consistent (features, concept)
  every?(method (f)
           ~ assq(feature-attribute(f), features)
            | member?(f, features, test: \=);
         end method,
         concept);
end method consistent;

//  To generalize a concept you might add a disjunct or drop a conjunct.
//  Since we are restricted to conjunctions we only drop conjuncts.
//  For the restricted hypothesis space consisting of conjunctions of 
//  positive literals, generalization corresponds to removing features 
//  from a concept represented as a list of features.
define method generalize (concept)
  map(method (feature) remove(concept, feature, test: \=); end method,
      concept);
end method generalize;

//  To specialize a concept you add a conjunct or drop a disjunct.
//  Since we are restricted to conjunctions we only add conjuncts.
//  Specialization corresponds to adding features that are consistent 
//  with the concept. Both specialize and generalize return concepts 
//  that are, respectively, minimally more specific and minimally more
//  general than the concept provided as an argument to the function.
define method specialize (concept)
  map(method (feature) pair(feature, concept); end method,
      findall(features(),
              method (feature)
                consistent(list(feature), concept)
                 & ~ member?(feature, concept, test: \=);
              end method));
end method specialize;

//  Construct the set of all possible features.
define method features ()
  apply(concatenate!,
        map(method (dim)
              map(method (value)
                    list(dimension-attribute(dim), value);
                  end method,
                  dimension-values(dim));
            end method,
            dimensions()));
end method features;

//  Simplification for the special case of conjunctions of 
//  positive literals:
define method simplify-general (general, specific)
  //  Include only concepts that are generalizations of 
  //  some concept in the specific boundary.
  general
   := findall(general,
              method (concept1)
                any?(method (concept2)
                       more-specific(concept2, concept1)
                        | equivalent(concept1, concept2);
                     end method,
                     specific);
              end method);
  //  Eliminate any concepts that are specializations of other 
  //  concepts in the general boundary.
  general
   := findall(general,
              method (concept1)
                ~ any?(method (concept2)
                         more-specific(concept1, concept2);
                       end method,
                       general);
              end method);
  list(general, specific);
end method simplify-general;

define method simplify-specific (general, specific)
  //  Include only concepts that are specializations of 
  //  some concept in the general boundary.
  specific
   := findall(specific,
              method (concept1)
                any?(method (concept2)
                       more-specific(concept1, concept2)
                        | equivalent(concept1, concept2);
                     end method,
                     general);
              end method);
  //  Eliminate any concepts that are generalizations of other 
  //  concepts in the specific boundary.
  specific
   := findall(specific,
              method (concept1)
                ~ any?(method (concept2)
                         more-specific(concept2, concept1);
                       end method,
                       specific);
              end method);
  list(general, specific);
end method simplify-specific;

define method more-specific (concept1, concept2)
  concept1 = #(#"or")
   | (subset?(concept2, concept1, test: \=)
       & ~ subset?(concept1, concept2, test: \=));
end method more-specific;

define method equivalent (concept1, concept2)
  subset?(concept2, concept1, test: \=)
   & subset?(concept1, concept2, test: \=);
end method equivalent;

//  Example from the text
define method dimensions ()
  list(make-dimension(#"status", #(#"faculty", #"staff")),
       make-dimension(#"floor", #(#"four", #"five")),
       make-dimension(#"dept", #(#"cs", #"ee")));
end method dimensions;

define method classes () #(#"yes", #"no"); end method classes;

//  Versions takes a set of training examples and a pair of initial
//  boundaries and refines the boundaries by processing each training 
//  example in turn.
define method versions (examples, boundaries)
  begin
    do(method (example)
         boundaries := refine(example, first(boundaries), second(boundaries));
         format-out("Example: %S\nGeneral: %S\nSpecific: %S\n", example,
                    first(boundaries), second(boundaries));
       end method,
       examples);
    examples;
  end;
end method versions;

//  Test function.
begin
  let examples
      = list(make-example(412,
                          #(#(#"status", #"faculty"), #(#"floor", #"four"),
                            #(#"dept", #"cs")),
                          #"yes"),
             make-example(509,
                          #(#(#"status", #"staff"), #(#"floor", #"five"),
                            #(#"dept", #"cs")),
                          #"no"),
             make-example(517,
                          #(#(#"status", #"faculty"), #(#"floor", #"five"),
                            #(#"dept", #"cs")),
                          #"yes"),
             make-example(507,
                          #(#(#"status", #"faculty"), #(#"floor", #"five"),
                            #(#"dept", #"ee")),
                          #"no"));
  let boundaries = list(list(list()), map(example-features, examples));
  define method test () versions(examples, boundaries); end method test;
end;

//  Copyright 1994, Brown University, Providence, RI
//  Permission to use and modify this software and its documentation
//  for any purpose other than its incorporation into a commercial
//  product is hereby granted without fee.  Permission to copy and
//  distribute this software and its documentation only for
//  non-commercial use is also granted without fee, provided, however
//  that the above copyright notice appear in all copies, that both
//  that copyright notice and this permission notice appear in
//  supporting documentation, that the name of Brown University not
//  be used in advertising or publicity pertaining to distribution
//  of the software without specific, written prior permission, and
//  that the person doing the distribution notify Brown University
//  of such distributions outside of his or her organization. Brown
//  University makes no representations about the suitability of this
//  software for any purpose. It is provided "as is" without express
//  or implied warranty.  Brown University requests notification of
//  any modifications to this software or its documentation.
// 
//  Send the following redistribution information
// 
//  	Name:
//  	Organization:
//  	Address (postal and/or electronic):
// 
//  To:
//  	Software Librarian
//  	Computer Science Department, Box 1910
//  	Brown University
//  	Providence, RI 02912
// 
//  		or
// 
//  	brusd@cs.brown.edu
// 
//  We will acknowledge all electronic notifications.
"eof";

