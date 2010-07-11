module: topic-resolver
synopsis: This file arranges a topic hierarchy.

/**
Synopsis: Arrange a topic hierarchy.

There can only be one final topic hierarchy, but it is built from fragmentary
parent-child relationships created by:
   - The table of contents file,
   - Section directives,
   - VI directives in quotes,
   - Generic function/method relationships,
   - Fixed child topics created by header styles.

The magic is in combining these fragments into one overall tree and identifying
errors. One error that should NOT be a concern is the use of an ambiguous title
in one of the above. All ambiguity should have been resolved.
**/
define method arrange-topics (topics :: <sequence>, tocs :: <sequence>)
=> (tree :: <ordered-tree>)
   let trees = concatenate-as(<stretchy-vector>,
                              tocs.toc-arrangement,
                              topics.vi-arrangement,
                              topics.header-arrangement,
                              topics.free-arrangements,
                              topics.generic-function-arrangement,
                              topics.section-directive-arrangement);
   // log-object("Topic trees", trees);
   
   // Combine topics that are == in different trees into one tree. Topics are
   // == if a topic reference leading to a parent/child relationship was
   // successfully resolved.
   let remaining-trees = trees;
   let arranged-trees :: <vector> = vector();
   for (tree in remaining-trees)
      // Each iteration takes the arranged-trees collection, splits it into
      // overlapping and non-overlapping parts, combines the overlapping parts,
      // saves the non-overlapping parts, and puts them both in a new vector.
      // I would use partition to do the split, but that returns lists (which
      // add to the front) and I want to be able to preserve the original order
      // as much as possible, and also the add wouldn't work because it would
      // make a new list that I can't re-bind to.
      let overlapping-trees = make(<stretchy-vector>);
      let spare-trees = make(<stretchy-vector>);
      for (ct in arranged-trees)
         add!(if (overlapping?(tree, ct)) overlapping-trees else spare-trees end,
              ct)
      end for;
      let combined-tree = reduce(combine-trees, tree, overlapping-trees);
      arranged-trees := apply(vector, combined-tree, spare-trees);
   end for;

   // Pick out the main table of contents tree and the other unrooted trees.
   local method rooted? (tree :: <ordered-tree>) => (rooted? :: <boolean>)
            tree[tree.root-key].topic = #f
         end method;
   let contents-tree :: <ordered-tree> = find-element(arranged-trees, rooted?);
   debug-assert(find-element(arranged-trees, rooted?, skip: 1) = #f,
         "More than one root; tables of content not merged");

   // Move trees that haven't been placed anywhere in particular to default
   // locations. These default locations may or may not be in contents-tree, but
   // all trees should be in contents-tree at the end.
   let remaining-trees :: <list>
         = as(<list>, choose(curry(\~==, contents-tree), arranged-trees));
   while (~remaining-trees.empty?)
      reparent-tree-at-default(remaining-trees.head, contents-tree, remaining-trees.tail);
      remaining-trees := remaining-trees.tail;
   end while;
   
   // Sort the topics.
   sort-tree!(contents-tree, stable: #t);
   
   // Discard <arranged-topic>s and keep only <topic>s.
   for (arr-topic :: <arranged-topic> keyed-by k in contents-tree)
      contents-tree[k] := arr-topic.topic;
   end for;
   
   contents-tree
end method;


/// Synopsis: Move a tree to a default location.
define method reparent-tree-at-default
   (tree :: <ordered-tree>, contents-tree :: <ordered-tree>, other-trees :: <list>)
=> ()
   let root-topic :: <topic> = tree[tree.root-key].topic;
   let better-parent-ids = default-parent-ids(root-topic);
   let parent-tree :: false-or(<ordered-tree>) = #f;
   let parent-key :: false-or(<ordered-tree-key>) = #f;

   // Search all-trees for potential parents and record best of them.
   // Better-parent-ids is in descending order of awesomeness, so as we find a
   // possible parent, we can easily ignore all later less-awesome parents.
   // Note that the IDs here may be actual IDs or fully qualified names in ID form.
   for (target-tree in add(other-trees, contents-tree),
         until: better-parent-ids.empty?)
      unless (tree == target-tree)
         for (arranged-topic keyed-by target-key in target-tree,
               until: better-parent-ids.empty?)
            let target-topic = arranged-topic.topic;
            if (target-topic)
               let parent-list-key = find-key
                     (better-parent-ids, rcurry(matching-id?, target-topic));
               if (parent-list-key)
                  parent-tree := target-tree;
                  parent-key := target-key;
                  better-parent-ids := copy-sequence
                        (better-parent-ids, end: parent-list-key);
               end if
            end if
         end for
      end unless
   end for;
   
   // If we can't find anything better, just put in top level of content.
   unless (parent-tree & parent-key)
      parent-tree := contents-tree;
      parent-key := contents-tree.root-key;
   end unless;

   replace-subtree!(parent-tree, tree, from: parent-key.next-inf-key)
end method;


/// Synopsis: Returns default parent IDs for a topic, best first.
define generic default-parent-ids (topic :: <topic>) => (ids :: <sequence>);

define method default-parent-ids (topic :: <library-doc>)
=> (ids :: <sequence>)
   if (topic.existent-api?)
      list(":Libraries")
   else
      #()
   end if
end method;

define method default-parent-ids (topic :: <module-doc>)
=> (ids :: <sequence>)
   if (topic.existent-api?)
      let lib-name = topic.fully-qualified-name.enclosing-qualified-name;
      list(format-to-string(":Modules(%s)", lib-name).standardize-id,
           ":Modules",
           lib-name.qualified-name-as-id)
   else
      #()
   end if
end method;

define method default-parent-ids (topic :: <class-doc>)
=> (ids :: <sequence>)
   if (topic.existent-api?)
      let mod-name = topic.fully-qualified-name.enclosing-qualified-name;
      let lib-name = mod-name.enclosing-qualified-name;
      list(format-to-string(":Classes(%s)", mod-name).standardize-id,
           format-to-string(":Bindings(%s)", mod-name).standardize-id,
           format-to-string(":Classes(%s)", lib-name).standardize-id,
           format-to-string(":Bindings(%s)", lib-name).standardize-id,
           ":Classes",
           ":Bindings",
           mod-name.qualified-name-as-id)
   else
      #()
   end if
end method;

define method default-parent-ids (topic :: <variable-doc>)
=> (ids :: <sequence>)
   if (topic.existent-api?)
      let mod-name = topic.fully-qualified-name.enclosing-qualified-name;
      let lib-name = mod-name.enclosing-qualified-name;
      list(format-to-string(":Variables(%s)", mod-name).standardize-id,
           format-to-string(":Bindings(%s)", mod-name).standardize-id,
           format-to-string(":Variables(%s)", lib-name).standardize-id,
           format-to-string(":Bindings(%s)", lib-name).standardize-id,
           ":Variables",
           ":Bindings",
           mod-name.qualified-name-as-id)
   else
      #()
   end if
end method;

define method default-parent-ids (topic :: <function-doc>)
=> (ids :: <sequence>)
   if (topic.existent-api? & topic.topic-type ~= #"method")
      let mod-name = topic.fully-qualified-name.enclosing-qualified-name;
      let lib-name = mod-name.enclosing-qualified-name;
      list(format-to-string(":Functions(%s)", mod-name).standardize-id,
           format-to-string(":Bindings(%s)", mod-name).standardize-id,
           format-to-string(":Functions(%s)", lib-name).standardize-id,
           format-to-string(":Bindings(%s)", lib-name).standardize-id,
           ":Functions",
           ":Bindings",
           mod-name.qualified-name-as-id)
   else
      #()
   end if
end method;

define method default-parent-ids (topic :: <macro-doc>)
=> (ids :: <sequence>)
   if (topic.existent-api?)
      let mod-name = topic.fully-qualified-name.enclosing-qualified-name;
      let lib-name = mod-name.enclosing-qualified-name;
      list(format-to-string(":Macros(%s)", mod-name).standardize-id,
           format-to-string(":Bindings(%s)", mod-name).standardize-id,
           format-to-string(":Macros(%s)", lib-name).standardize-id,
           format-to-string(":Bindings(%s)", lib-name).standardize-id,
           ":Macros",
           ":Bindings",
           mod-name.qualified-name-as-id)
   else
      #()
   end if
end method;

define method default-parent-ids (topic :: <unbound-doc>)
=> (ids :: <sequence>)
   if (topic.existent-api?)
      let mod-name = topic.fully-qualified-name.enclosing-qualified-name;
      let lib-name = mod-name.enclosing-qualified-name;
      list(format-to-string(":Unbound(%s)", mod-name).standardize-id,
           format-to-string(":Bindings(%s)", mod-name).standardize-id,
           format-to-string(":Unbound(%s)", lib-name).standardize-id,
           format-to-string(":Bindings(%s)", lib-name).standardize-id,
           ":Unbound",
           ":Bindings",
           mod-name.qualified-name-as-id)
   else
      #()
   end if
end method;

define method default-parent-ids (topic :: <topic>) => (ids :: <sequence>)
   #()
end method;


define method matching-id? (test-id :: <string>, topic :: <topic>)
=> (matching? :: <boolean>)
   test-id = topic.id
end method;

define method matching-id? (test-id :: <string>, topic :: <api-doc>)
=> (matching? :: <boolean>)
   test-id = topic.id | test-id = topic.fully-qualified-name.qualified-name-as-id
end method;


/// Synopsis: Node of a fragmentary hierarchy containing the topic at that
/// location and the reason for its positioning there.
define class <arranged-topic> (<source-location-mixin>)
   slot topic :: false-or(<topic>),
      required-init-keyword: #"topic";
   slot type :: one-of(#"toc-file", #"section-directive", #"vi-directive",
                       #"generic-family", #"topic-style", #"none"),
      required-init-keyword: #"type";
end class;

define method print-object (o :: <arranged-topic>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "arranged by %s ", o.type);
      if (o.topic)
         pprint-newline(#"fill", s);
         format(s, "in %s ", o.source-location);
         pprint-newline(#"fill", s);
         format(s, "{topic title \"%s\", id %s}", o.topic.title, o.topic.id);
      end if;
   end printing-logical-block;
end method;

/// A heterogenous collection of topics, all children of the same parent,
/// are ordered as follows:
///   1. By table of contents, in the order specified.
///   2. By VI directives in quotes, in the order encountered in the parent
///      topic.
///   3. Children created with subtopic styling, in the order encountered
///      in the source code.
///   4. Alphabetically by title.
define method \< (arr1 :: <arranged-topic>, arr2 :: <arranged-topic>)
=> (arr1-sorts-before? :: <boolean>)
   let ranked-types =  #[#"toc-file", #"vi-directive", #"topic-style"];
   let arr1-rank = position(ranked-types, arr1.type) | ranked-types.size;
   let arr2-rank = position(ranked-types, arr2.type) | ranked-types.size;
   case
      arr1-rank < arr2-rank => #t;
      arr1-rank > arr2-rank => #f;
      arr1-rank = arr2-rank =>
         case
            arr1.type = #"toc-file" =>
               // Keep relative order from toc-arrangement; this is a stable sort,
               // so returning #f will do the trick.
               #f;
            arr1.type = #"vi-directive" =>
               // Use order from parent topic.
               arr1.source-location < arr2.source-location;
            arr1.type = #"topic-style" =>
               // Keep relative order.
               #f;
            otherwise =>
               // Alphabetical.
               when (arr1.topic & arr2.topic)
                  let str1 = arr1.topic.title.stringify-title;
                  let str2 = arr2.topic.title.stringify-title;
                  str1 < str2
               end when;
         end case;
   end case;
end method;


/// Synopsis: Make a series of trees where the only element in a tree is a topic.
define method free-arrangements (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   map(method (topic)
          let arranged-topic =
               make(<arranged-topic>, topic: topic, type: #"none");
          make(<ordered-tree>, root: arranged-topic)
       end, topics)
end method;


/// Synopsis: Determines parent/child relationships according to the table of
/// contents files.
/// Discussion: Each input toc tree has a root of #f and other nodes are
/// <topic-ref>s. Output trees are <arranged-topic>s with the root node's topic
/// being #f and other nodes' topics being the <topic-ref>s topic. There will
/// always be at least one tree to provide the root for the documentation.
define method toc-arrangement (tocs :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   let toc-root = make(<arranged-topic>, topic: #f, type: #"toc-file");
   if (tocs.empty?)
      vector(make(<ordered-tree>, root: toc-root))
   else
      map(method (toc :: <ordered-tree>) => (topics :: <ordered-tree>)
             let tree = make(<ordered-tree>, root: toc-root);
             for (toc-ref :: false-or(<topic-ref>) keyed-by k in toc)
                when (toc-ref)
                   let arranged-topic =
                         make(<arranged-topic>, topic: toc-ref.target,
                              type: #"toc-file",
                              source-location: toc-ref.source-location);
                   tree[k] := arranged-topic;
                end when;
             end for;
             tree
          end method,
          tocs);
   end if;
end method;


/// Synopsis: Determines parent/child relationships according to "Section:"
/// directives, represented by topics' 'parent' slot. The directive indicates
/// a parent.
define method section-directive-arrangement (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   let sectioned-topics = choose(parent, topics);
   let trees = make(<stretchy-vector>);
   for (topic in sectioned-topics)
      let arranged-parent = make(<arranged-topic>, topic: topic.parent.target,
                                 type: #"section-directive",
                                 source-location: topic.parent.source-location);
      let arranged-child = make(<arranged-topic>, topic: topic,
                                type: #"section-directive",
                                source-location: topic.parent.source-location);
      let tree = make-parent-child-tree(arranged-parent, arranged-child);
      trees := add!(trees, tree);
   end for;
   trees
end method;


/// Synopsis: Determines parent/child relationships according to "vi" options
/// of quotations represented as <vi-xref> objects. The option indicates a
/// child.
define method vi-arrangement (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   let trees = make(<stretchy-vector>);
   for (parent-topic in topics)
      visit-vi-xrefs(parent-topic,
            method (xref :: <vi-xref>, #key setter)
               let child-topic = xref.target;
               let arranged-parent = make(<arranged-topic>, topic: parent-topic,
                                          type: #"vi-directive",
                                          source-location: xref.source-location);
               let arranged-child = make(<arranged-topic>, topic: child-topic,
                                         type: #"vi-directive",
                                         source-location: xref.source-location);
               let tree = make-parent-child-tree(arranged-parent, arranged-child);
               trees := add!(trees, tree);
            end method);
   end for;
   trees
end method;


/// Synopsis: Determines relationships by generic function and method relations.
/// The generic function is the parent of its methods.
/// Discussion: This will result in one tree per documented generic.
define method generic-function-arrangement (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   let trees = make(<stretchy-vector>);
   let function-topics
         = choose(method (topic :: <topic>) => (use? :: <boolean>)
                     instance?(topic, <function-doc>)
                           & topic.topic-type = #"method"
                           & topic.fully-qualified-name.true?
                  end, topics);
   let generic-topics
         = choose(method (topic :: <topic>) => (use? :: <boolean>)
                     instance?(topic, <generic-doc>)
                           & topic.fully-qualified-name.true?
                  end, topics);
   for (generic-topic in generic-topics)
      let arranged-children = make(<stretchy-vector>);
      for (function-topic in function-topics)
         let generic-name = function-topic.fully-qualified-name.enclosing-qualified-name;
         if (generic-name = generic-topic.fully-qualified-name)
            arranged-children := add!(arranged-children,
                  make(<arranged-topic>, type: #"generic-family",
                       source-location: generic-topic.source-location,
                       topic: function-topic))
         end if
      end for;
      unless (arranged-children.empty?)
         let arranged-parent = make(<arranged-topic>, type: #"generic-family",
               source-location: generic-topic.source-location, topic: generic-topic);
         let tree = apply(make-parent-child-tree, arranged-parent, arranged-children);
         trees := add!(trees, tree)
      end unless
   end for;
   trees
end method;


/// Synopsis: Determines relationships by header style in a comment block,
/// represented by topics' fixed-parent slot.
define method header-arrangement (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   // TODO
   #()
end method;


define function make-parent-child-tree (parent, #rest children)
=> (tree :: <ordered-tree>)
   let tree = make(<ordered-tree>, root: parent);
   let root = tree.root-key;
   for (child in children)
      tree[root.next-inf-key] := child;
   end for;
   tree
end function;


/// Synopsis: Returns whether any members of 'tree-1' are in 'tree-2'.
define function overlapping? (tree-1 :: <ordered-tree>, tree-2 :: <ordered-tree>)
=> (yes? :: <boolean>)
   tree-1 == tree-2 | ~common-element-keys(tree-1, tree-2).empty?
end function;


/// Synopsis: Combines two trees that have at least one common element.
define function combine-trees (tree-1 :: <ordered-tree>, tree-2 :: <ordered-tree>)
=> (combined-tree :: <ordered-tree>)
   if (tree-1 == tree-2)
      tree-1
   else
      let common-key-pairs = common-element-keys(tree-1, tree-2);
      debug-assert(~common-key-pairs.empty?);
      
      // log-object("Combining", tree-1);
      // log-object("and", tree-2);
      
      // Find which tree can contain the other, and merge other tree into it. We
      // find it by finding the highest common node on each tree. The <topic> of
      // the highest common node on tree-1 should be identical to the <topic> of
      // the highest common node on tree-2. One of the two should be root of its
      // respective tree; there shouldn't be any parent nodes that aren't also
      // common.
      
      let (tree-1-anchor, tree-2-anchor) =
            block (found-anchor)
               for (p in common-key-pairs)
                  if (p.head = tree-1.root-key | p.tail = tree-2.root-key)
                     found-anchor(p.head, p.tail)
                  end if;
               end for;
               
               // Neither is root key; there is an error which we will catch
               // later. Use arbitrary key pair.
               values(common-key-pairs.first.head, common-key-pairs.first.tail) 
            end block;
      
      let (dest-tree, dest-tree-anchor, orig-tree, orig-tree-anchor) =
            case
               tree-1-anchor = tree-1.root-key =>
                  values(tree-2, tree-2-anchor, tree-1, tree-1-anchor);
               tree-2-anchor = tree-2.root-key =>
                  values(tree-1, tree-1-anchor, tree-2, tree-2-anchor);
               otherwise =>
                  let tree-1-arr = tree-1[tree-1-anchor];
                  let tree-2-arr = tree-2[tree-2-anchor];
                  let conflicting-locations =
                        vector(tree-1-arr.source-location,
                               tree-2-arr.source-location);
                  conflicting-locations-in-tree(
                        location: tree-1-arr.topic.source-location,
                        arranger-locations: conflicting-locations.item-string-list |
                                            "various locations");
            end case;
      
      // Combine trees.
      
      let combined-tree = merge-arranged-topics!(dest-tree, dest-tree-anchor,
                                                 orig-tree, orig-tree-anchor);
      
      // Check for duplicate topics at different locations.
      
      let keys = combined-tree.key-sequence;
      for (key in keys)
         let matching-keys = list();
         for (match-key in keys)
            if (combined-tree[key].topic == combined-tree[match-key].topic)
               matching-keys := add!(matching-keys, match-key);
            end if;
         end for;
         if (matching-keys.size > 1)
            let topic = combined-tree[key].topic;
            let arranger-locations = map(
                  method (key) combined-tree[key].source-location end,
                  matching-keys);
            conflicting-locations-in-tree(
                  location: topic.source-location,
                  arranger-locations: arranger-locations.item-string-list |
                                      "various locations");
         end if;
      end for;

      combined-tree
   end if;
end function;


/// Synopsis: Returns the keys of common elements in two trees.
/// Values:
///   common-keys - A sequence of pairs, each pair contains the keys in 'tree-1'
///                 and 'tree-2' that contain a common element. If no common
///                 elements, returns empty sequence.
define function common-element-keys
   (tree-1 :: <ordered-tree>, tree-2 :: <ordered-tree>)
=> (common-keys :: <sequence>)
   let common-keys = make(<stretchy-vector>);
   for (arr-topic-1 keyed-by key-1 in tree-1)
      for (arr-topic-2 keyed-by key-2 in tree-2)
         if (arr-topic-1.topic == arr-topic-2.topic)
            common-keys := add!(common-keys, pair(key-1, key-2))
         end if;
      end for;
   end for;
   common-keys
end function;


/// Synopsis: Combine two <arranged-topic>s containing identical <topic>s.
define function merge-arranged-topics!
   (dest-tree :: <ordered-tree>, dest-tree-key :: <ordered-tree-key>,
    orig-tree :: <ordered-tree>, orig-tree-key :: <ordered-tree-key>)
=> (dest-tree :: <ordered-tree>)
   debug-assert(dest-tree[dest-tree-key].topic == orig-tree[orig-tree-key].topic,
                "Merging arranged topics that aren't the same");
   
   // When two different arrangement methods put a topic under the same parent,
   // which arrangement method trumps the other? The arrangement method drives
   // the final table-of-contents sort order. The topics are the same, but keep
   // the trumping arrangement method.
   if (orig-tree[orig-tree-key] < dest-tree[dest-tree-key])
      dest-tree[dest-tree-key] := orig-tree[orig-tree-key]
   end if;

   // Merge or copy original's children to destination.

   for (orig-child-key in orig-tree-key.inf-key-sequence)
      let merged-orig-to-dest? =
            block (merged)
               // Do we need to merge from this orig child into a dest child?
               for (dest-child-key in dest-tree-key.inf-key-sequence)
                  if (dest-tree[dest-child-key].topic ==
                      orig-tree[orig-child-key].topic)
                     // Yes, merge them.
                     merge-arranged-topics!(dest-tree, dest-child-key,
                                            orig-tree, orig-child-key);
                     merged(#t);
                  end if;
               end for;
               merged(#f);
            end block;
      // If we didn't merge, we still need to copy this orig child to dest.
      if (~merged-orig-to-dest?)
         let orig-child-tree = copy-tree(orig-tree, from: orig-child-key);
         let dest-child-key = dest-tree-key.next-inf-key;
         replace-subtree!(dest-tree, orig-child-tree, from: dest-child-key);
      end if;
   end for;   
   dest-tree
end function;
