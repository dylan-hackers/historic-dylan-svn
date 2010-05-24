module: dylan-translator
synopsis: Tracks API representations generated from parser tokens. Also contains
          code for library/module dependency graph management.


//
// Context
//


define class <context> (<object>)
   virtual slot context-library :: false-or(<library>), setter: #f;
   virtual slot context-module :: false-or(<module>), setter: #f;
   slot context-name :: false-or(<source-name>) = #f;
   
   slot library-graph :: <graph> = make(<graph>);
   slot module-graph :: <graph> = make(<graph>);

   /// True if changes need to be propogated. Specifically, if a library's
   /// local or exported module names change, or a module's local or exported
   /// binding names change. This happens when
   ///
   ///   - An alias is added to a definition
   ///   - An additional name is exported from a namespace
   ///   - A new definition is added to a namespace
   ///
   /// A definition that is outdated and has a replacement does not, in itself,
   /// cause changes that need propogating. Replacements are "how" [em] changes
   /// get propogated.
   slot changed? :: <boolean> = #f;

   /// Libraries. Keyed by name, a <string>.
   slot library-definitions = make(<case-insensitive-skip-list>);

   /// Keys are <definition>s to be replaced. Corresponding elements are the
   /// replacement.
   slot replacements :: <table> = make(<table>);
   
   /// Tokens defining a namespace.
   slot definers :: <table> = make(<table>);
end class;


define method context-library (context :: <context>) => (lib :: false-or(<library>))
   element(context.library-definitions, context.context-name.library-name, default: #f)
end method;


define method context-module (context :: <context>) => (mod :: false-or(<module>))
   let lib = context.context-library;
   lib & element(lib.definitions, context.context-name.module-name, default: #f)
end method;


/// Synopsis: Sets up a current library or module context.
///
/// Scoped module and bindings names need to know what library and/or module
/// they are in. When creating the initial round of API representations, that
/// information is supplied through the <context> object's 'context-library',
/// 'context-module', and 'context-name' slots, set with this macro.
define macro with-context-name
   { with-context-name ?:name (?:expression) ?:body end }
      => {  let saved-context = ?name;
            block()
               ?name.context-name := ?expression;
               ?body
            cleanup
               ?name := saved-context
            end block }

   { with-context-name (?:expression) ?:body end }
      => { with-context-name ?=context (?expression) ?body end }
end macro;


//
// Usage graph
//


define sealed class <graph> (<object>)
   slot nodes :: <stretchy-vector> = make(<stretchy-vector>);
   slot edges :: <stretchy-vector> = make(<stretchy-vector>);
end;


define class <node> (<object>)
   constant slot graph, required-init-keyword: #"graph";
   slot outgoing-edges :: <stretchy-vector> = make(<stretchy-vector>);
   slot incoming-edges :: <stretchy-vector> = make(<stretchy-vector>);
   slot tracking :: <object> = #f;
   slot value :: <object>, required-init-keyword: #"value";
end;


define method initialize (n :: <node>, #key)
   next-method();
   n.graph.nodes := add!(n.graph.nodes, n);
end;


define class <edge> (<object>)
   constant slot graph, required-init-keyword: #"graph";
   constant slot edge-source :: <node>, required-init-keyword: #"source";
   constant slot edge-target :: <node>, required-init-keyword: #"target";
   slot edge-label :: false-or(<string>) = #f, init-keyword: #"label";
   slot value :: <object> = #f, init-keyword: #"value";
end;


define method initialize (e :: <edge>, #key)
   next-method();
   e.graph.edges := add!(e.graph.edges, e);
   e.edge-source.outgoing-edges := add!(e.edge-source.outgoing-edges, e);
   e.edge-target.incoming-edges := add!(e.edge-target.incoming-edges, e);
end;


define function connect
   (source :: <node>, target :: <node>, #rest init-args, #key label)
=> ()
   unless (source.graph == target.graph)
      error("source and target have to be in the same graph");
   end;
   apply(make, <edge>, graph:, source.graph, source:, source, target:, target,
         init-args);
end;


define function disconnect (source :: <node>, target :: <node>) => ()
   unless (source.graph == target.graph)
      error("source and target have to be in the same graph");
   end;
   let e = choose(compose(curry(\=, target), edge-target),
                  source.outgoing-edges);
   if (e.size == 1)
      remove-edge(e[0]);
   elseif (e.size == 0)
      error("tried to disconnect %= from %=, which were not connected",
            source, target);
   else
      error("multiple edges from %= to %=", source, target);
   end;
end;


define function remove-edge (edge :: <edge>) => ()
   let source = edge.edge-source;
   let target = edge.edge-target;
   source.outgoing-edges := remove!(source.outgoing-edges, edge);
   target.incoming-edges := remove!(target.incoming-edges, edge);
   source.graph.edges := remove!(source.graph.edges, edge);
end;


define function degree (n :: <node>) => (int :: <integer>)
   n.outgoing-edges.size + n.incoming-edges.size;
end;


define function remove-node (n :: <node>) => ()
   for (edge in n.outgoing-edges.copy-sequence)
      remove-edge(edge);
      if (edge.edge-target.incoming-edges.size == 0)
         remove-node(edge.edge-target);
      end;
   end;
   if (n.degree == 0)
      remove!(n.graph.nodes, n);
   end;
end;


define function find-node (graph :: <graph>, pred :: <function>, #key failure = #f)
=> (res :: <object>)
   find-element(graph.nodes, pred, failure: failure);
end;


define function find-node-by-value
   (graph :: <graph>, val :: <object>, #key test = \==, failure = #f)
=> (res :: <object>)
   find-element(graph.nodes, compose(rcurry(test, val), value), failure: failure);
end;


define function find-target-node-by-edge-label
   (source :: <node>, label :: <string>, #key test = \=, failure = #f)
=> (target :: <object>)
   let edge = find-element(source.outgoing-edges, 
         compose(rcurry(test, label), edge-label), failure: #f);
   if (edge)
      edge.edge-target
   else
      failure
   end if
end;

define function choose-nodes (graph :: <graph>, pred :: <function>)
=> (seq :: <collection>)
   choose(pred, graph.nodes)
end;


define function sorted-graph (graph :: <graph>) => (sorted-nodes :: <sequence>)
   let sorted-nodes = make(<list>);
   visit-depth-first(graph, always(#f), 
         method (node :: <node>) => ()
            sorted-nodes := add!(sorted-nodes, node)
         end);
   sorted-nodes
end function;


define function visit-depth-first
   (graph :: <graph>, enter :: <function>, leave :: <function>)
=> ()
   do(curry(tracking-setter, #"white"), graph.nodes);
   
   local method visit (n :: <node>)
            n.tracking := #"grey";
            enter(n);
            for (next in map(edge-target, n.outgoing-edges))
               if (next.tracking == #"white")
                  visit(next);
               end;
            end;
            n.tracking := #"black";
            leave(n);
         end;
   
   for (n in graph.nodes)
     if (n.tracking == #"white")
       visit(n);
     end;
   end;
end;


define function acyclic? (graph :: <graph>) => (res :: <boolean>)
   // can be done in O(n + e), is currently O(n + e) + O(e)
   let i = graph.nodes.size;
   let top-count = make(<object-table>);
   visit-depth-first(graph, always(#f),
                     method (x)
                        top-count[x] := i;
                        i := i - 1;
                     end);
   block (ret)
      for (edge in graph.edges)
         if (top-count[edge.edge-source] > top-count[edge.edge-target])
            ret(#f);
         end;
      end;
      #t;
   end;
end;


define method visit-cycles (graph :: <graph>, visit-cycle :: <function>) => ()
   let stack :: <deque> = make(<deque>);
   do(curry(tracking-setter, 0), graph.nodes);
  
   local method traverse (node :: <node>) => ()
            push(stack, node);
            let initial-stack-depth = stack.size;
            node.tracking := initial-stack-depth;
    
            let children = map(edge-target, node.outgoing-edges);
            do(curry(traverse-child, node), children);

            if (node.tracking = initial-stack-depth)
               let cycle = make(<list>);
               for (cycle-node = pop(stack) then pop(stack),
                    until: cycle-node == node)
                  // Add nodes other than the current one.
                  cycle := add!(cycle, cycle-node);
                  cycle-node.tracking := #f;
               finally
                  // Add the current node.
                  cycle := add!(cycle, cycle-node);
                  cycle-node.tracking := #f;
               end for;

               if (cycle.size > 1)
                  visit-cycle(cycle);
               end if;
            end if;
         end method,
         
         method traverse-child (node :: <node>, child :: <node>) => ()
            if (child.tracking = 0)
               traverse(child);
            end if;

            let child-index = child.tracking;
            let node-index = node.tracking;
            if (child-index)
               if (node-index)
                  node.tracking := min(node-index, child-index);
               else
                  node.tracking := child-index;
               end if;
            end if;
         end method;
            
   for (node in graph.nodes)
      if (node.tracking = 0)
         traverse(node);
      end if;
   end for;
end method;


define method print-object (o :: <graph>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "graph ");
      pprint-newline(#"fill", s);
      format(s, "%= ", o.nodes);
      pprint-newline(#"fill", s);
      format(s, "%=", o.edges);
   end printing-logical-block
end method;

define method print-object (o :: <node>, s :: <stream>) => ()
   format(s, "{node %=}", o.value);
end method;

define method print-object (o :: <edge>, s :: <stream>) => ()
   format(s, "{edge %= from %= to %=}", o.edge-label, o.edge-source, o.edge-target)
end method;
