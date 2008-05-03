module: ordered-tree
synopsis: A tree structure with children in order.

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
      (tree :: <ordered-tree>, #key root: root-val = $unsupplied)
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


define method key-test (tree-coll :: <ordered-tree>) => (test :: <function>)
   ordered-tree-key-test
end method;

define method root-key (tree-coll :: <ordered-tree>) => (root :: <ordered-tree-key>)
   make(<ordered-tree-key>, tree: tree-coll)
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
   if (key.node)
      let inf-range = range(below: key.node.inferior-count);
      let suffixes = map(method (count) make(<string>, size: count, fill: 's') end,
                         inf-range);
      let paths = map(curry(concatenate, key, "i"), suffixes);
      map (method (path) make(<ordered-tree-key>, path: path, tree: key.tree) end,
           paths)
   else
      #()
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


define method translate-key (key :: <ordered-tree-key>, new-tree :: <ordered-tree>)
=> (new-key :: <ordered-tree-key>)
   if (key.tree == new-tree)
      key
   else
      make(<ordered-tree-key>, tree: new-tree, path: shallow-copy(key.path))
   end if;
end method;


//// Collection methods ////

define method copy-tree
   (tree-coll :: <ordered-tree>,
   #key from :: <ordered-tree-key> = root-key(tree-coll))
=> (tree :: <ordered-tree>)
   // TODO
   error("Not implemented")
end method;


define method replace-subtree!
   (dest :: <ordered-tree>, src :: <ordered-tree>,
   #key from :: <ordered-tree-key>)
=> (dest :: <ordered-tree>)
   // TODO
   error("Not implemented")
end method;


define method element
   (tree-coll :: <ordered-tree>, key :: <ordered-tree-key>, #key default)
=> (elem :: <object>)
   let key = translate-key(key, tree-coll);
   let elem-node = key.node;
   if (elem-node) elem-node.value else default end if
end method;

/// This method creates the keyed node and all intermediate nodes required to 
/// connect it to the tree. The intermediate nodes are filled with the 'fill:'
/// [qv <ordered-tree>/keywords] argument to 'make(singleton(<ordered-tree>))'.
define method element-setter
   (new-elem, tree-coll :: <ordered-tree>, key :: <ordered-tree-key>)
=> (new-elem)

   local method make-new-node () => (node)
      tree-coll.cached-size := tree-coll.cached-size + 1;
      make(<ordered-tree-node>, value: tree-coll.fill-value)
   end method;
   
   // Can't use cached node because we can have cached a node that was later
   // removed and, if so, we need to construct the entire path to it.
   let key = translate-key(key, tree-coll);
   let node = tree-coll.root | (tree-coll.root := make-new-node());
   for (i in key.path)
      node := select (i)
                 'i' => node.first-inferior |
                        (node.first-inferior := make-new-node());
                 's' => node.successor |
                        (node.successor := make-new-node());
              end select;
   end for;
   tree-coll.change-count := tree-coll.change-count + 1;
   cache-node (key, node);
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


//// Key cache ////

/// Synopsis: Updates cache if necessary and returns node.
/// Values:
///   node - #f indicates node does not exist.
/// Discussion: This method returns the cached node associated with the key.
/// If cached-change-count < tree.change-count, cache is invalidated. A change
/// to a node's value doesn't count as a real change.
define function node (key :: <ordered-tree-key>)
=> (node :: false-or(<ordered-tree-node>))
   if (key.cache-valid?)
      key.cached-node;
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
end function;

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


//// Node class ////

define class <ordered-tree-node> (<object>)
   slot first-inferior :: false-or(<ordered-tree-node>) = #f;
   slot successor :: false-or(<ordered-tree-node>) = #f; // Always #f for root.
   slot value :: <object>, init-keyword: #"value";
end class;

define function inferior-count (node :: <ordered-tree-node>)
   for (succ-node = node.first-inferior then node.successor, i = 0 then i + 1,
        until: ~succ-node)
   finally i end for
end function;


//// Iteration protocol ////

define class <fip-state> (<object>)
   // Stack has nodes to do. It starts with the root node; when the stack
   // empties we are done. Stack should always have nothing or a valid node
   // on top.
   constant slot nodes :: <deque> = make(<deque>), init-keyword: #"nodes";
   constant slot keys :: <deque> = make(<deque>), init-keyword: #"keys";
end class;

define function fip-init-state
   (tree-coll :: <ordered-tree>)
=> (init-state :: <fip-state>)
   let state = make(<fip-state>);
   if (tree-coll.root)
      push(state.nodes, tree-coll.root);
      push(state.keys, "");
   end if;
   state
end function;

define function fip-limit-obj (tree-coll :: <ordered-tree>) => (limit-obj)
end function;

// State progresses by popping a node and pushing its successor and inferior
// nodes. We never revisit popped nodes. Skip #f nodes.
define function fip-next-state
   (tree-coll :: <ordered-tree>, state :: <fip-state>)
=> (new-state :: <fip-state>)
   if (~state.nodes.empty?)
      debug-assert(~state.keys.empty?);
      let node = state.nodes.pop;
      let key = state.keys.pop;
      if (node)
         push(state.nodes, node.successor);
         push(state.nodes, node.first-inferior);
         push(state.keys, concatenate(key, "s"));
         push(state.keys, concatenate(key, "i"));
      end if;
   end if;
   
   // Having a #f node on top is an invalid state; would cause current-element
   // to fail. Get rid of them.
   while (~state.nodes.empty? & state.nodes.first = #f)
      state.nodes.pop;
      state.keys.pop;
   end while;
   
   state
end function;

define function fip-finished?
   (tree-coll :: <ordered-tree>, state :: <fip-state>, limit)
=> (finished? :: <boolean>)
   state.nodes.empty?
end function;

define function fip-curr-key
   (tree-coll :: <ordered-tree>, state :: <fip-state>)
=> (key :: <ordered-tree-key>)
   debug-assert(~state.keys.empty?);
   make(<ordered-tree-key>, tree: tree-coll, path: state.keys.first)
end function;

define function fip-curr-elem
   (tree-coll :: <ordered-tree>, state :: <fip-state>)
=> (elem :: <object>)
   debug-assert(~state.nodes.empty?);
   state.nodes.first.value
end function;

define function fip-curr-setter
   (val, tree-coll :: <ordered-tree>, state :: <fip-state>)
=> (val :: <object>)
   debug-assert(~state.nodes.empty?);
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
   format(s, "{tree %=}", o.root);
end method;

define method print-object (o :: <ordered-tree-node>, s :: <stream>) => ()
   format(s, "{node %=: ", o.value);
   if (o.first-inferior)
      format(s, "%=", o.first-inferior);
   end if;
   format(s, "}");
   if (o.successor)
      format(s, ", %=", o.successor);
   end if;
end method;
