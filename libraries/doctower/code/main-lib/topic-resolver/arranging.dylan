module: topic-resolver
synopsis: This file arranges a topic hierarchy.

/**
Synopsis: Arrange a topic hierarchy.

There can only be one final topic hierarchy, but it is built from fragmentary
parent-child relationships created by:
   - The table of contents file,
   - Section directives,
   - ToC directives in quotes,
   - List Of directives (requires everything else to be arranged),
   - Generic function/method relationships,
   - Fixed child topics created by header styles.

The magic is in combining these fragments into one overall tree and
identifying errors.

One error is to use an ambiguous title in one of the above. Ambiguity in
non-hierarchy-building directives is fine, but cannot be tolerated while
building the hierarchy, since the hierarchy determines which identically-named
topics need to be merged with each other. Ambiguity is indicated by a
'<target-placeholder>' in one of the above and is detected by the individual
arrangers.

--- Conditions: ---
  <need-locations>     - Signaled if link is cannot be resolved to topic. Handler
                         should return possible topics for given link target.
  <user-visible-error> - Signaled if user error encountered.
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
   let combined-trees = vector();
   let uncombined-trees = trees;
   for (tree in uncombined-trees)
      // I would use partition here, but that returns lists (which add to the
      // front) and I want to be able to preserve the original order as much
      // as possible, and also the add wouldn't work because add makes a new
      // list that I can't re-bind to.
      let overlapping-trees = make(<stretchy-vector>);
      let other-trees = make(<stretchy-vector>);
      for (ct in combined-trees)
         add!(if (overlapping?(tree, ct)) overlapping-trees else other-trees end,
              ct)
      end for;
      let combined-tree = reduce(combine-trees, tree, overlapping-trees);
      combined-trees := apply(vector, combined-tree, other-trees);
   end for;
   
   // log-object("Combined trees", combined-trees);
   
   // Combine the disjoint trees into a single tree with #f as the root topic
   // by adding unrooted trees as top-level children of rooted tree.
   let (rooted-trees, unrooted-trees) =
         partition(method (tree) tree[tree.root-key].topic = #f end,
                   combined-trees);
   debug-assert(rooted-trees.size = 1, "Tables of content not merged");
   let arranged-tree = rooted-trees.first;
   let rk = arranged-tree.root-key;
   for (t in unrooted-trees)
      replace-subtree!(arranged-tree, t, from: rk.next-inf-key)
   end for;

   // Sort the topics.
   sort-tree!(arranged-tree, stable: #t);
   // log-object("Sorted toc", arranged-tree);
   
   // Discard <arranged-topic>s and keep only <topic>s.
   for (arr-topic :: <arranged-topic> keyed-by k in arranged-tree)
      arranged-tree[k] := arr-topic.topic;
   end for;
   
   arranged-tree
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
///   2. By ToC directives in quotes, in the order encountered in the parent
///      topic.
///   3. Children created with subtopic styling, in the order encountered
///      in the source code.
///   4. Alphabetically by title.
define method \< (arr1 :: <arranged-topic>, arr2 :: <arranged-topic>)
=> (less-than :: <boolean>)
   let type1 :: <symbol> = arr1.type;
   let type2 :: <symbol> = arr2.type;
   case
      type1 = #"toc-file" => #t ;
      type2 = #"toc-file" => #f ;
      type1 = #"vi-directive" & type2 = #"vi-directive" =>
         arr1.source-location < arr2.source-location;
      type1 = #"vi-directive" => #t ;
      type2 = #"vi-directive" => #f ; 
      type1 = #"topic-style" => #t ;
      type2 = #"topic-style" => #f ;
      otherwise =>
         when (arr1.topic & arr2.topic)
            let str1 = arr1.topic.title.stringify-title;
            let str2 = arr2.topic.title.stringify-title;
            str1 < str2
         end when;
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
             for (i keyed-by k in toc)
                when (i)
                   ensure-resolved-to-topic(i.target);
                   let arranged-topic =
                         make(<arranged-topic>, topic: i.target, type: #"toc-file",
                              source-location: i.source-location);
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
      ensure-resolved-to-topic(topic.parent.target);
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
            method (xref, #key setter)
               when (instance?(xref, <vi-xref>))
                  let child-topic = ensure-resolved-to-topic(xref.target);
                  let arranged-parent = make(<arranged-topic>, topic: parent-topic,
                                             type: #"vi-directive",
                                             source-location: xref.source-location);
                  let arranged-child = make(<arranged-topic>, topic: child-topic,
                                            type: #"vi-directive",
                                            source-location: xref.source-location);
                  let tree = make-parent-child-tree(arranged-parent, arranged-child);
                  trees := add!(trees, tree);
               end when;
            end method);
   end for;
   trees
end method;


/// Generic Function: visit-vi-xrefs
/// Synopsis: Visits a <topic> and its nested elements that can contain <vi-xref>
/// objects.
///
/// Arguments:
///   element     - The <markup-element> to visit.
///   operation   - A <function> on 'element'.
///   #rest keys  - A set of keys passed to 'operation'.

define collection-recursive slot-visitor visit-vi-xrefs
   <bold>,	          text;
   <cite>,            text;
   <class-doc>,	    content, shortdesc, keywords-section;
   <code-phrase>,	    text;
   <defn-list>,	    items;
   <emphasis>,	       text;
   <footnote>,	       content;
   <function-doc>,	 content, shortdesc, args-section, vals-section, conds-section;
   <italic>,	       text;
   <macro-doc>,       content, shortdesc, args-section, vals-section;
   <note>,	          content;
   <ordered-list>,	 items;
   <paragraph>,       content;
   <section>,         content;
   <simple-table>,	 headings, items;
   <term-style>,	    text;
   <term>,	          text;
   <vi-xref>,	       ;
   <topic>,           content, shortdesc;
   <underline>,	    text;
   <unordered-list>,	 items;
end slot-visitor;


/// Synopsis: Determines relationships by generic function and method relations.
/// The generic function is the parent of its methods.
/// Discussion: This will result in one tree per documented generic.
define method generic-function-arrangement (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   let generic-topics = choose(rcurry(instance?, <generic-doc>), topics);

   local method arrange-methods (gen-topic :: <generic-doc>)
         => (tree :: <ordered-tree>)
            apply(make-parent-child-tree,
                  make(<arranged-topic>, topic: gen-topic,
                       type: #"generic-family"),
                  gen-topic.arranged-method-topics)
         end method,
            
         method arranged-method-topics (gen-topic :: <generic-doc>)
         => (arranged :: <sequence>)
            map(rcurry(arranged-method-topic, gen-topic), gen-topic.method-topics)
         end method,

         method arranged-method-topic
            (meth-topic :: <function-doc>, gen-topic :: <generic-doc>)
         => (arranged :: <arranged-topic>)
            make(<arranged-topic>, topic: meth-topic, type: #"generic-family")
         end method;
   
   map(arrange-methods, generic-topics);
end method;


/// Synopsis: Determines relationships by header style in a comment block,
/// represented by topics' fixed-parent slot.
define method header-arrangement (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   // TODO
   #()
end method;


/// Generic function: ensure-resolved-to-topic
/// Synopsis: Returns topic that link is resolved to.
/// Conditions:
///   <need-locations>     - Signaled if link cannot be resolved to topic. Handler
///                          should return possible topics for given link target.
///   <user-visible-error> - Signaled if link is not resolved to topic.

define method ensure-resolved-to-topic (link :: <target-placeholder>)
=> (target :: <topic>)
   let locations = signal(make(<need-locations>, specifier: link.target));
   ambiguous-title-in-link(location: link.source-location,
                           target-text: link.target,
                           topic-locations: locations.item-string-list |
                                            "various locations");
end method;

define method ensure-resolved-to-topic (link :: <topic>)
=> (target :: <topic>)
   link
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


/// Synopsis: Returns the keys of common elements in two trees, or #f if no
/// common element.
/// Values:
///   common-keys - A sequence of pairs, each pair contains the keys in 'tree-1'
///                 and 'tree-2' that contain a common element.
define function common-element-keys
   (tree-1 :: <ordered-tree>, tree-2 :: <ordered-tree>)
=> (common-keys :: <sequence>)
   let common-keys = make(<stretchy-vector>);
   let tree-1-keys = tree-1.key-sequence;
   let tree-2-keys = tree-2.key-sequence;
   for (key-1 in tree-1-keys)
      for (key-2 in tree-2-keys)
         let arr-topic-1 :: <arranged-topic> = tree-1[key-1];
         let arr-topic-2 :: <arranged-topic> = tree-2[key-2];
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
   // the final table-of-contents sort order.
   if (orig-tree[orig-tree-key] < dest-tree[dest-tree-key])
      dest-tree[dest-tree-key] := orig-tree[orig-tree-key]
   end if;

   // Merge or copy original's children to destination.

   // log-object("Merging", orig-tree);
   // log-object("into", dest-tree);
   // log("key %= into %=", orig-tree-key, dest-tree-key);
   for (orig-child-key in orig-tree-key.inf-key-sequence)
      let merged-orig-to-dest? =
            block (merged)
               for (dest-child-key in dest-tree-key.inf-key-sequence)
                  // log("Checking need to merge from child %= into %=",
                  //     orig-child-key, dest-child-key);
                  if (dest-tree[dest-child-key].topic ==
                      orig-tree[orig-child-key].topic)
                     // log("Recursing");
                     merge-arranged-topics!(dest-tree, dest-child-key,
                                            orig-tree, orig-child-key);
                     // log("Recurse done");
                     merged(#t);
                  end if;
               end for;
               merged(#f);
            end block;
      if (~merged-orig-to-dest?) // ...then we need to copy orig to dest
         let orig-child-tree = copy-tree(orig-tree, from: orig-child-key);
         let dest-child-key = dest-tree-key.next-inf-key;
         // log("Copying orig %= to dest %=", orig-child-key, dest-child-key);
         replace-subtree!(dest-tree, orig-child-tree, from: dest-child-key);
      end if;
   end for;   
   dest-tree
end function;