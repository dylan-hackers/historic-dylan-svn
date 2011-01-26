module: ordered-tree
synopsis: A tree structure with children in order.

// TODO: Implement gaps in a series of children, to properly support remove-key!.

/// Synopsis: A tree that allows unlimited child nodes, numbered from 0.
/// Make Keywords:
///   fill: - An instance of <object> specifying the initial value for any
///           implicitly-created elements. The default value is #f.
///   root: - An instance of <object> specifying the value of the root node.
///           If omitted, the tree has no root node upon creation.
define class <ordered-tree>
      (<stretchy-collection>, <mutable-explicit-key-collection>)
   keyword #"root", type: <object>;
   constant slot fill-value :: <object> = #f, init-keyword: #"fill";
   slot root :: false-or(<ordered-tree-node>) = #f;
   slot cached-size :: <integer> = 0;
   slot change-count :: <integer> = 0;
   virtual slot size :: <integer>;
end class;


define method initialize
      (tree :: <ordered-tree>, #key root: root-val = unsupplied())
   if (supplied?(root-val))
      tree[tree.root-key] := root-val
   end if
end method;


define method size (tree-coll :: <ordered-tree>) => (sz :: <integer>)
   cached-size(tree-coll)
end method;


//// Key and key methods ////

define class <ordered-tree-key> (<object>)
   // 'i' is first inferior key, 's' is successor. An empty string is root.
   constant slot path :: <string> = "", init-keyword: #"path";
   constant slot tree :: <ordered-tree>, required-init-keyword: #"tree";

   // Cached node. See 'node'.
   instance slot cached-node :: false-or(<ordered-tree-node>) = #f;
   instance slot cached-change-count :: false-or(<integer>) = #f;
   constant virtual slot node :: false-or(<ordered-tree-node>);
end class;


define inline method \= (key1 :: <ordered-tree-key>, key2 :: <ordered-tree-key>)
=> (equal? :: <boolean>)
   ordered-tree-key-test(key1, key2)
end method;

define method key-test (tree-coll :: <ordered-tree>) => (test :: <function>)
   ordered-tree-key-test
end method;

define method root-key (tree-coll :: <ordered-tree>) => (root :: <ordered-tree-key>)
   make(<ordered-tree-key>, tree: tree-coll)
end method;

define method key-depth (key :: <ordered-tree-key>) => (depth :: <integer>)
   reduce(method (n, e) if (e = 'i') n + 1 else n end if end method,
          0, key.path)
end method;

define method sup-key (key :: <ordered-tree-key>)
=> (superior :: false-or(<ordered-tree-key>))
   if (key.path.size > 0)
      debug-assert(key.path.first = 'i');
      let last-inf-idx = find-last-key(key.path, curry(\=, 'i'));
      let sup-key-path = copy-sequence(key.path, end: last-inf-idx);
      make (<ordered-tree-key>, tree: key.tree, path: sup-key-path)
   else
      #f
   end if
end method;

define method inf-key-sequence (key :: <ordered-tree-key>)
=> (inferiors :: <sequence>)
   if (key.node & key.node.inferior-count > 0)
      let inf-count = key.node.inferior-count;
      let paths = make(<vector>, size: key.node.inferior-count);
      for (i from 0 below inf-count)
         paths[i] := concatenate("i", make(<string>, size: i, fill: 's'));
      end for;
      map(method (p) make(<ordered-tree-key>, tree: key.tree,
                          path: concatenate(key.path, p)) end,
          paths)
   else
      #[]
   end if
end method;

define method next-inf-key (key :: <ordered-tree-key>)
=> (inferior :: false-or(<ordered-tree-key>))
   let inf-count = if (key.node) key.node.inferior-count else 0 end;
   let next-inf-path =
         concatenate(key.path, "i", make(<string>, size: inf-count, fill: 's'));
   make (<ordered-tree-key>, tree: key.tree, path: next-inf-path);
end method;

define method succ-key (key :: <ordered-tree-key>)
=> (succ :: false-or(<ordered-tree-key>))
   if (key.path.size > 0)
      let longer-path = concatenate(key.path, "s");
      make(<ordered-tree-key>, tree: key.tree, path: longer-path)
   else
      #f
   end if
end method;

define method pred-key (key :: <ordered-tree-key>)
=> (pred :: false-or(<ordered-tree-key>))
   if (key.path.size > 0 & key.path.last = 's')
      let shorter-path = copy-sequence(key.path, end: key.path.size - 1);
      make(<ordered-tree-key>, tree: key.tree, path: shorter-path)
   else
      #f
   end if
end method;


//// Collection methods ////

define method copy-tree
   (tree-coll :: <ordered-tree>,
    #key from :: <ordered-tree-key> = root-key(tree-coll))
=> (tree :: <ordered-tree>)
   let from = translate-key(from, tree-coll);
   unless (from.node)
      error("Key not found in collection")
   end unless;
   let fill = tree-coll.fill-value;
   let new-tree = make(<ordered-tree>, fill: fill, root: fill);
   replace-node(new-tree, new-tree.root, from.node);
   new-tree
end method;


define method replace-subtree!
   (dest :: <ordered-tree>, src :: <ordered-tree>,
    #key from :: <ordered-tree-key> = root-key(dest))
=> (dest :: <ordered-tree>)
   let from = translate-key(from, dest);
   let dest-node = from.node;
   let src-node = src.root;
   if (dest-node | src-node)
      if (src-node)
         replace-node(dest, from.maybe-new-node, src.root);
      else
         remove-node-at-key(from);
      end if;
      dest.change-count := dest.change-count + 1;
   end if;
   dest
end method;


define method sort-tree!
   (tree-coll :: <ordered-tree>,
    #key test :: <function> = \<, stable :: <boolean> = #f)
=> (tree-coll :: <ordered-tree>)
   local method test-nodes
            (node-1 :: <ordered-tree-node>, node-2 :: <ordered-tree-node>)
         => (less-than :: <boolean>)
            test(node-1.value, node-2.value)
         end method,

         method sort-children (parent-node :: false-or(<ordered-tree-node>))
         => ()
            when (parent-node)
               let children = make(<stretchy-vector>);
               for (next-node = parent-node.first-inferior
                    then next-node.successor, while: next-node)
                  add!(children, next-node);
               end for;
               if (~children.empty?)
                  children := sort!(children, test: test-nodes, stable: stable);
                  parent-node.first-inferior := children.first;
                  for (i from 0 below children.size - 1)
                     children[i].successor := children[i + 1];
                  end for;
                  children.last.successor := #f;
               end if;
               sort-children(parent-node.first-inferior);
               sort-children(parent-node.successor);
            end when;
         end method;
   
   sort-children(tree-coll.root);
   tree-coll.change-count := tree-coll.change-count + 1;
   tree-coll
end method;


define method element
   (tree-coll :: <ordered-tree>, key :: <ordered-tree-key>,
    #key default = unsupplied())
=> (elem :: <object>)
   let key = translate-key(key, tree-coll);
   let elem-node = key.node;
   if (elem-node)
      elem-node.value
   else
      if (unsupplied?(default)) error("Key not found in collection") else default end if;
   end if
end method;


/// This method creates the keyed node and all intermediate nodes required to 
/// connect it to the tree. The intermediate nodes are filled with the 'fill:'
/// [qv <ordered-tree>/:keywords] argument to 'make(singleton(<ordered-tree>))'.
define method element-setter
   (new-elem, tree-coll :: <ordered-tree>, key :: <ordered-tree-key>)
=> (new-elem)
   let key = translate-key(key, tree-coll);
   let (node, new-node?) = key.maybe-new-node;
   if (new-node?)
      tree-coll.change-count := tree-coll.change-count + 1;
   end if;
   node.value := new-elem
end method;


define method forward-iteration-protocol
   (tree-coll :: <ordered-tree>)
=> (initial-state :: <object>, limit :: <object>, next-state :: <function>,
      finished-state? :: <function>, current-key :: <function>,
      current-element :: <function>, current-element-setter :: <function>,
      copy-state :: <function>);
   values(fip-init-state(tree-coll),
          fip-limit-obj(tree-coll),
          fip-next-state,
          fip-finished?,
          fip-curr-key,
          fip-curr-elem,
          fip-curr-setter,
          fip-copy-state)
end method;


//// Key test ////

define function ordered-tree-key-test
   (key1 :: <ordered-tree-key>, key2 :: <ordered-tree-key>)
=> (same :: <boolean>)
   key1.path = key2.path
end function;


//// Node class ////

define class <ordered-tree-node> (<object>)
   slot first-inferior :: false-or(<ordered-tree-node>) = #f;
   slot successor :: false-or(<ordered-tree-node>) = #f; // Always #f for root.
   slot value :: <object>, init-keyword: #"value";
end class;

define function inferior-count (node :: <ordered-tree-node>)
   for (child-node = node.first-inferior then child-node.successor,
        i from 0, until: ~child-node)
   finally i end for
end function;


define function replace-node
   (dest-tree :: <ordered-tree>, dest-node :: <ordered-tree-node>,
    src-node :: false-or(<ordered-tree-node>))
=> ()

   local method populate-children (dest-node, src-node) => ()
            let src-child = src-node.first-inferior;
            if (src-child)
               let dest-child = make(<ordered-tree-node>, value: src-child.value);
               dest-node.first-inferior := dest-child;
               dest-tree.cached-size := dest-tree.cached-size + 1;
               populate-successors(dest-child, src-child);
               populate-children(dest-child, src-child);
            end if;
         end method,
   
         method populate-successors (dest-node, src-node) => ()
            let src-succ = src-node.successor;
            if (src-succ)
               let dest-succ = make(<ordered-tree-node>, value: src-succ.value);
               dest-node.successor := dest-succ;
               dest-tree.cached-size := dest-tree.cached-size + 1;
               populate-children(dest-succ, src-succ);
               populate-successors(dest-succ, src-succ);
            end if;
         end method;
            
   dest-node.value := src-node.value;
   populate-children (dest-node, src-node);
   dest-tree.change-count := dest-tree.change-count + 1;
end function;

define function remove-node-at-key
   (key :: <ordered-tree-key>)
=> ()
   let tree-coll = key.tree;
   let succ-node = key.node.successor;
   let pred-node = key.pred-key.node;
   let sup-node = key.sup-key.node;
   case
      ~sup-node =>
         tree-coll.root := #f;
         tree-coll.cached-size := 0;
      ~pred-node =>
         sup-node.first-inferior := succ-node;
         tree-coll.cached-size := tree-coll.cached-size - 1;
      otherwise =>
         pred-node.successor := succ-node;
         tree-coll.cached-size := tree-coll.cached-size - 1;
   end case;
end function;


define function maybe-new-node
   (key :: <ordered-tree-key>)
=> (node :: <ordered-tree-node>, new-node? :: <boolean>)
   if (key.cache-valid? & key.cached-node)
      values (key.cached-node, #f)
   else
      let tree-coll = key.tree;
      let new-node? = #f;
      local method make-new-node () => (node)
               tree-coll.cached-size := tree-coll.cached-size + 1;
               new-node? := #t;
               make(<ordered-tree-node>, value: tree-coll.fill-value)
            end method;
   
      let node = tree-coll.root | (tree-coll.root := make-new-node());
      for (i in key.path)
         node := select (i)
                    'i' => node.first-inferior |
                           (node.first-inferior := make-new-node());
                    's' => node.successor |
                           (node.successor := make-new-node());
                 end select;
      end for;
      cache-node(key, node);
      values (node, new-node?);
   end if;
end function;


//// Key cache ////

define method translate-key (key :: <ordered-tree-key>, new-tree :: <ordered-tree>)
=> (new-key :: <ordered-tree-key>)
   if (key.tree == new-tree)
      key
   else
      make(<ordered-tree-key>, tree: new-tree, path: copy-sequence(key.path))
   end if;
end method;


/// Synopsis: Updates cache if necessary and returns node.
/// Values:
///   node - #f indicates node does not exist.
/// Discussion: This method returns the cached node associated with the key.
/// If cached-change-count < tree.change-count, cache is invalidated. A change
/// to a node's value doesn't count as a real change.
///
define method node (key :: <ordered-tree-key>)
=> (node :: false-or(<ordered-tree-node>))
   if (key.cache-valid?)
      key.cached-node
   else
      let node = key.tree.root;
      for (i in key.path, until: ~node)
         node := select (i)
                    'i' => node.first-inferior;
                    's' => node.successor;
                 end select;
      end for;
      cache-node (key, node);
   end if;
end method;

define function cache-valid? (key :: <ordered-tree-key>) => (valid? :: <boolean>)
   let valid? =
         key.cached-change-count & key.tree.change-count <= key.cached-change-count;
   if (~valid?) key.cached-change-count := #f end;
   valid?
end function;

define function cache-node (key :: <ordered-tree-key>, node) => (node)
   key.cached-node := node;
   key.cached-change-count := key.tree.change-count;
   node;
end function;


//// Iteration protocol ////

define class <fip-state> (<object>)
   // State is current node followed by more nodes to do. It starts with the
   // root node; when the stack empties we are done. Stack should always have
   // nothing or a valid node on top.
   constant slot nodes :: <deque> = make(<deque>), init-keyword: #"nodes";
   constant slot keys :: <deque> = make(<deque>), init-keyword: #"keys";
end class;

define function fip-init-state
   (tree-coll :: <ordered-tree>)
=> (init-state :: <fip-state>)
   let state = make(<fip-state>);
   let root-node = tree-coll.root;
   if (root-node)
      push(state.nodes, root-node);
      push(state.keys, "");
   end if;
   state
end function;

define function fip-limit-obj (tree-coll :: <ordered-tree>) => (limit-obj)
end function;

// State progresses by popping current node and pushing its successor and inferior
// nodes. We never revisit popped nodes.
define function fip-next-state
   (tree-coll :: <ordered-tree>, state :: <fip-state>)
=> (new-state :: <fip-state>)
   debug-assert(~fip-finished?(tree-coll, state, #f), "Iteration has finished");
   let node = state.nodes.pop;
   let key = state.keys.pop;
   if (node.successor)
      push(state.nodes, node.successor);
      push(state.keys, concatenate(key, "s"));
   end if;
   if (node.first-inferior)
      push(state.nodes, node.first-inferior);
      push(state.keys, concatenate(key, "i"));
   end if;
   state
end function;

define function fip-finished?
   (tree-coll :: <ordered-tree>, state :: <fip-state>, limit)
=> (finished? :: <boolean>)
   // Finished when no nodes left to consider.
   state.nodes.empty?
end function;

define function fip-curr-key
   (tree-coll :: <ordered-tree>, state :: <fip-state>)
=> (key :: <ordered-tree-key>)
   debug-assert(~fip-finished?(tree-coll, state, #f), "Iteration has finished");
   make(<ordered-tree-key>, tree: tree-coll, path: state.keys.first)
end function;

define function fip-curr-elem
   (tree-coll :: <ordered-tree>, state :: <fip-state>)
=> (elem :: <object>)
   debug-assert(~fip-finished?(tree-coll, state, #f), "Iteration has finished");
   state.nodes.first.value
end function;

define function fip-curr-setter
   (val, tree-coll :: <ordered-tree>, state :: <fip-state>)
=> (val :: <object>)
   debug-assert(~fip-finished?(tree-coll, state, #f), "Iteration has finished");
   state.nodes.first.value := val
end function;

define function fip-copy-state
   (tree-coll :: <ordered-tree>, state :: <fip-state>)
=> (dup-state :: <fip-state>)
   let nodes-copy = state.shallow-copy;
   let keys-copy = state.shallow-copy;
   make(<fip-state>, nodes: nodes-copy, keys: keys-copy)
end function;


define method print-object (o :: <ordered-tree>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "tree %=", o.root);
   end printing-logical-block;
end method;

define method print-object (o :: <ordered-tree-node>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "node %=", o.value);
      if (o.first-inferior)
         write(s, ": ");
         pprint-indent(#"current", 0, s);
         pprint-newline(#"fill", s);
         format(s, "%=", o.first-inferior);
      end if;
   end printing-logical-block;
   if (o.successor)
      write(s, ", ");
      pprint-newline(#"linear", s);
      format(s, "%=", o.successor);
   end if;
end method;

define method print-object (o :: <ordered-tree-key>, s :: <stream>) => ()
   format(s, "%=", o.path);
end method;
