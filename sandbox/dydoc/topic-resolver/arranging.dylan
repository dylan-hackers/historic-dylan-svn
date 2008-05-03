module: topic-resolver
synopsis: This file arranges a topic hierarchy.

/// Synopsis: Arrange a topic hierarchy.
///
/// There can only be one final topic hierarchy, but it is built from fragmentary
/// parent-child relationships created by:
///   - The table of contents file,
///   - Section directives,
///   - ToC directives in quotes,
///   - List Of directives,
///   - Generic function/method relationships,
///   - Fixed child topics created by header styles.
///
/// The magic is in combining these fragments into one overall tree and
/// identifying errors.
///
/// One error is to use an ambiguous title in one of the above. Ambiguity in
/// non-hierarchy-building directives is fine, but cannot be tolerated while
/// building the hierarchy, since the hierarchy determines which identically-named
/// topics need to be merged with each other. Ambiguity is indicated by a
/// '<target-placeholder>' in one of the above and is detected by the individual
/// arrangers.
///
define method arrange-topics (topics :: <sequence>) => (tree :: <ordered-tree>)
   // TODO: Streamline this. Visitor pattern can consolidate all these separate
   // functions so each topic is visited only once?
   let trees = concatenate(topics.toc-arrangement,
                           topics.section-directive-arrangement,
                           topics.toc-xref-arrangement,
                           topics.generic-function-arrangement,
                           topics.header-arrangement);
   log-object("Topic trees", map(topic, trees.first));
   make(<ordered-tree>);
end method;


/// Synopsis: Node of a fragmentary hierarchy containing the topic at that
/// location and the provenance of its positioning there.
define class <arranged-topic> (<object>)
   slot topic :: <topic>,
      required-init-keyword: #"topic";
   slot type :: one-of(#"toc-file", #"section-directive", #"toc-directive",
                       #"generic-family", #"topic-style"),
      required-init-keyword: #"type";
   slot source :: false-or(<interm-element>) = #f,
      init-keyword: #"source";
end class;

define method initialize (obj :: <arranged-topic>, #key type, source)
   next-method();
   unless ((type = #"toc-file" & ~source) | (type ~= #"toc-file" & source))
      error("Creating <arranged-topic> with invalid type/source combo")
   end unless;
end method;

define method \= (obj-1 :: <arranged-topic>, obj-2 :: <arranged-topic>)
=> (equal? :: <boolean>) 
   obj-1.topic = obj-2.topic
end method;


/// Synopsis: Determines parent/child relationships according to the table of
/// contents file.
/// Discussion: This will only result in one tree, but it is put in a sequence
/// for solidarity with the others.
define method toc-arrangement (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   #()
end method;


/// Synopsis: Determines parent/child relationships according to "Section:"
/// directives, represented by topics' 'parent' slot. The directive indicates
/// a parent.
define method section-directive-arrangement (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   let sectioned-topics = choose(parent, topics);
   let trees = list();
   for (topic in sectioned-topics)
      ensure-resolved-to-topic(topic.parent);
      let arranged-parent = make(<arranged-topic>, topic: topic.parent,
                                 type: #"section-directive", source: topic);
      let arranged-child = make(<arranged-topic>, topic: topic,
                                type: #"section-directive", source: topic);
      let tree = make-parent-child-tree(arranged-parent, arranged-child);
      trees := add!(trees, tree);
   end for;
   trees
end method;


/// Synopsis: Determines parent/child relationships according to "toc" options
/// of quotations represented as <toc-xref> objects. The option indicates a
/// child.
define method toc-xref-arrangement (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   let trees = list();
   for (parent-topic in topics)
      visit-toc-xrefs(parent-topic,
            method (xref)
               when (instance?(xref, <toc-xref>))
                  let child-topic = ensure-resolved-to-topic(xref.target);
                  let arranged-parent = make(<arranged-topic>, topic: parent-topic,
                                             type: #"toc-directive", source: xref);
                  let arranged-child = make(<arranged-topic>, topic: child-topic,
                                            type: #"toc-directive", source: xref);
                  let tree = make-parent-child-tree(arranged-parent, arranged-child);
                  trees := add!(trees, tree);
               end when;
            end method);
   end for;
   trees
end method;


/// Synopsis: Determines relationships by generic function and method relations.
/// The generic function is the parent of its methods.
/// Discussion: This will result in one tree per documented generic.
define method generic-function-arrangement (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   #()
end method;


/// Synopsis: Determines relationships by header style in a comment block,
/// represented by topics' fixed-parent slot.
define method header-arrangement (topics :: <sequence>)
=> (trees :: <sequence> /* of <ordered-tree> */)
   #()
end method;


/// Generic function: ensure-resolved-to-topic
/// Synopsis: Returns topic that link is resolved to.
/// Conditions: Signals condition if link is not resolved to topic.

define inline method ensure-resolved-to-topic (link :: <target-placeholder>)
=> (target :: <topic>)
   // TODO: Include list of possible topics, possibly in handler that has
   // that information available.
   error("Topic \"%s\" is ambiguous in link at %s",
         link.target, link.element-source)
end method;

define inline method ensure-resolved-to-topic (link :: <topic>)
=> (target :: <topic>)
   link
end method;


define inline function make-parent-child-tree (parent, #rest children)
=> (tree :: <ordered-tree>)
   let tree = make(<ordered-tree>, root: parent);
   for (child in children)
      tree[tree.root-key.next-inf-key] := child;
   end for;
   tree
end function;
   