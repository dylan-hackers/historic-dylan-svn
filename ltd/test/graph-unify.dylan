// ----------------------------------------------------------------------------
// 		            GRAPH UNIFICATION
// 			   "graph-unify.lisp"
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// 		            GRAPH UNIFICATION
// 			   "graph-unify.lisp"
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// 
// The main calls are:
// 
// 	(graph-unify d1 d2)		unifies two graph structures
// 	(tree->graph s-expression)	creates a tree-like graph
// 	(print->graph graph-expr)	creates a graph
// 
// See examples at the end of this file.
// 
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------
//  DAG-STRUCT
// ----------------------------------------------------------------------
//  This section contains structure definitions for graphs, nodes, and arcs.
//  Also included are print functions, constructor functions, 
//  de-constructor functions, copier functions, accessor functions,
//  and modifier functions.
//  STRUCTURES
define class <graph-node> (<object>)
  slot graph-node-class, init-keyword: #"graph-node-class";
  slot graph-node-arcs, init-keyword: #"graph-node-arcs";
  slot graph-node-mark, init-keyword: #"graph-node-mark";
  slot graph-node-mfset, init-keyword: #"graph-node-mfset";
end class <graph-node>;

define class <arc> (<object>)
  slot arc-label, init-keyword: #"arc-label";
  slot arc-destination, init-keyword: #"arc-destination";
end class <arc>;

//  NODE-CLASSES
define constant *variable* = #"var";

define constant *fail* = #"fail";

//  PRINT FUNCTIONS
define method print-graph-node (n, #rest ignore)
  print(graph->tree(n), *standard-output*);
end method print-graph-node;

//  (princ (graph->print n)))    will display reentrancy
define method print-arc (a, #rest ignore)
  format(*standard-output*, "#<Graph-Arc, LABEL: %S>", a.arc-label);
end method print-arc;

//  MEMORY MANAGEMENT
define variable *graph-node-pool* = #f;

define variable *graph-arc-pool* = #f;

//  CONSTRUCTORS
define method create-graph-node (#key class = *variable*, arcs = #f,
                                 mark = #f, mfset = #f)
  let n = pop!(*graph-node-pool*) | make-graph-node();
  n.graph-node-class := class;
  n.graph-node-arcs := arcs;
  n.graph-node-mark := mark;
  n.graph-node-mfset := mfset | list(n);
  n;
end method create-graph-node;

define method create-arc (#key label = #f, destination = #f)
  let n = pop!(*graph-arc-pool*) | make-arc();
  n.arc-label := label;
  n.arc-destination := destination;
  n;
end method create-arc;

define method create-null-graph ()
  create-graph-node();
end method create-null-graph;

//  DE-CONSTRUCTORS
define method dispose-graph-node (node)
  if (~ member?(node, *graph-node-pool*))
    push!(node, *graph-node-pool*);
  end if;
end method dispose-graph-node;

define method dispose-arc (arc)
  if (~ member?(arc, *graph-arc-pool*)) push!(arc, *graph-arc-pool*); end if;
end method dispose-arc;

define method dispose-graph (node)
  let list13156 = nodes-in-graph(node);
  begin do(dispose-graph-node, list13156); list13156; end;
end method dispose-graph;

//  COPIERS
define method copy-graph-node (node)
  create-graph-node(class: node.graph-node-class, arcs: node.graph-node-arcs,
                    mark: node.graph-node-mark, mfset: node.graph-node-mfset);
end method copy-graph-node;

define method copy-arc (arc)
  create-arc(label: arc.arc-label, destination: arc.arc-destination);
end method copy-arc;

define method copy-graph (node)
  let n1 = nodes-in-graph(node);
  begin
    do(method (n) n.graph-node-mark := create-null-graph(); end method, n1);
    n1;
  end;
  begin
    do(method (n)
         n.graph-node-mark.graph-node-class
          := // LTD: Function COPY-TREE not yet implemented.
             copy-tree(n.graph-node-class);
         n.graph-node-mark.graph-node-mark := #f;
         n.graph-node-mark.graph-node-mfset
          := if (mf-root-class?(n))
               map(graph-node-mark, n.graph-node-mfset);
             else
               n.graph-node-mfset.graph-node-mark;
             end if;
         n.graph-node-mark.graph-node-arcs
          := map(method (a)
                   create-arc(label: a.arc-label,
                              destination: a.arc-destination.graph-node-mark);
                 end method,
                 n.graph-node-arcs);
       end method,
       n1);
    n1;
  end;
  node.graph-node-mark;
end method copy-graph;

//  ACCESSORS
define method graph-node-arc (node, label)
  cl-find(label, node.graph-node-arcs,
          key: method (a) a.arc-label; end method);
end method graph-node-arc;

define method graph-node-subnode (node, label)
  arc-destination(graph-node-arc(node, label));
end method graph-node-subnode;

define method graph-node-arc-labels (node)
  map(arc-label, node.graph-node-arcs);
end method graph-node-arc-labels;

//  MODIFIERS
define method add-arc (node, arc)
  if (~ graph-node-arc(node, arc.arc-label))
    push!(arc, node.graph-node-arcs);
  end if;
end method add-arc;

define method add-arc-in-order (node, arc)
  if (~ graph-node-arc(node, arc.arc-label))
    node.graph-node-arcs
     := cl-merge(<list>, list(arc), node.graph-node-arcs,
                 method (x, y)
                   as(<string>, x.arc-label) < as(<string>, y.arc-label);
                 end method);
  end if;
end method add-arc-in-order;

// ----------------------------------------------------------------------
//  DAG-FNS
// ----------------------------------------------------------------------
//  This section contains various functions over dag structures.
// 
//  Function MARK-GRAPH
// 
//  Takes a root node of a graph and a marker.  Sets the mark field of 
//  every node in the graph equal to the marker.  Uses a gensym'd 
//  temporary marker name.
define method mark-graph (node, mark)
  let marker = generate-symbol(#"string"("MARKER-"));
  mark-graph-1(node, marker);
  mark-graph-1(node, mark);
end method mark-graph;

define method mark-graph-1 (node, sym)
  if (~ (node.graph-node-mark == sym))
    node.graph-node-mark := sym;
    let list13156 = node.graph-node-arcs;
    begin
      do(method (a) mark-graph-1(a.arc-destination, sym); end method,
         list13156);
      list13156;
    end;
  end if;
end method mark-graph-1;

// 
//  Function DEPTH-FIRST-TRAVERSAL
// 
//  Takes a root node of a graph and returns a list of nodes in the
//  graph.  Assumes that all nodes begin with MARK = NIL.
define method depth-first-traversal (node)
  node.graph-node-mark := #t;
  if (empty?(node.graph-node-arcs))
    list(node);
  else
    pair(node,
         apply(concatenate!,
               map(method (n)
                     if (empty?(n.graph-node-mark))
                       depth-first-traversal(n);
                     else
                       #f;
                     end if;
                   end method,
                   map(arc-destination, node.graph-node-arcs))));
  end if;
end method depth-first-traversal;

// 
//  Function NODES-IN-GRAPH
// 
//  Takes a root node of a graph and returns a list of all nodes in
//  the graph.  Uses a standard marking procedure to avoid traversing the
//  same portion of the graph more than once.
define method nodes-in-graph (node)
  mark-graph(node, #f);
  depth-first-traversal(node);
end method nodes-in-graph;

// ----------------------------------------------------------------------
//  DAG-PRINT
// ----------------------------------------------------------------------
//  This section contains functions to read and write arbitrary graphs.
//  Graphs are coded as lists with variables to mark reentrancy.  Thus,
//  graph structures can effectively (1) be printed on the the screen, 
//  and (2) be written to files and read back in.
define variable *dag-variables* =
  #(#"$0000", #"$0001", #"$0002", #"$0003", #"$0004", #"$0005", #"$0006",
    #"$0007", #"$0008", #"$0009", #"$0010", #"$0011", #"$0012", #"$0013",
    #"$0014", #"$0015", #"$0016", #"$0017", #"$0018", #"$0019", #"$0020",
    #"$0021", #"$0022", #"$0023", #"$0024", #"$0025", #"$0026", #"$0027",
    #"$0028", #"$0029", #"$0030", #"$0031", #"$0032", #"$0033", #"$0034",
    #"$0035", #"$0036", #"$0037", #"$0038", #"$0039", #"$0040", #"$0041",
    #"$0042", #"$0043", #"$0044", #"$0045", #"$0046", #"$0047", #"$0048",
    #"$0049", #"$0050", #"$0051", #"$0052", #"$0053", #"$0054", #"$0055",
    #"$0056", #"$0057", #"$0058", #"$0059");

//  Function GRAPH->TREE
// 
//  Takes a dag-structure and returns a tree in list format.  Loses
//  reentrancy of dag (copies are made).
define method graph->tree (d)
  if (empty?(d.graph-node-arcs))
    d.graph-node-class;
  else
    map(method (a)
          list(a.arc-label, graph->tree(a.arc-destination));
        end method,
        d.graph-node-arcs);
  end if;
end method graph->tree;

//  Function GRAPH-NODE-TYPE
// 
//  Return the type of an graph node stored in tree (s-expression) format.
define method graph-node-type (t1)
  if (not(instance?(t1, <list>)))
    #"atomic";
  else
    select (car(t1))
      #"*or*"
         => if (not(instance?(second(t1), <list>)))
              #"atomic-disjunction";
            else
              #"complex-disjunction";
            end if;
      #"*not*"
         => if (not(instance?(second(t1), <list>)))
              #"atomic-negation";
            else
              #"complex-negation";
            end if;
      #"*mult*"
         => if (not(instance?(second(t1), <list>)))
              #"atomic-multiple-value";
            else
              #"complex-multiple-value";
            end if;
      otherwise
         => #"complex";
    end select;
  end if;
end method graph-node-type;

//  Function TREE->GRAPH
// 
//  Takes a tree in list format and returns 
//  a dag-structure.  The structure will of course have no reentrancy.
define method tree->graph (t1)
  let k = graph-node-type(t1);
  if (member?(k,
              list(atomic: #"atomic-disjunction",
                   atomic-negation: #"atomic-multiple-value")))
    create-graph-node(class: t1, arcs: #f);
  elseif (k == #"complex")
    begin
      let n = create-graph-node(class: *variable*, arcs: #f);
      begin
        do(method (a)
             add-arc-in-order(n,
                              create-arc(label: head(a),
                                         destination: tree->graph(second(a))));
           end method,
           t1);
        t1;
      end;
      n;
    end;
  elseif (k == #"complex-disjunction")
    create-graph-node(class: *variable*,
                      arcs: pair(#"*or*",
                                 map(method (n) tree->graph(n); end method,
                                     tail(t1))));
  elseif (k == #"complex-negation")
    create-graph-node(class: *variable*,
                      arcs: pair(#"*not*",
                                 map(method (n) tree->graph(n); end method,
                                     tail(t1))));
  elseif (k == #"complex-multiple-value")
    create-graph-node(class: *variable*,
                      arcs: pair(#"*mult*",
                                 map(method (n) tree->graph(n); end method,
                                     tail(t1))));
  end if;
end method tree->graph;

//  Function GRAPH->PRINT
// 
//  Takes a dag-structure and returns a dag coded in list format.
//  For each node in the dag-structure, there is a corresponding element
//  in the list.  Each element has the form:
// 
//       (<node-variable> <node-class> <node-subnodes>)
//  
//  The order of elements corresponds to the order of a depth-first
//  traversal of the dag-structure.  If the arcs of the nodes are
//  ordered lexicographically, each dag-structure will have a well-defined
//  canonical list-format code.
define method graph->print (d)
  let n = nodes-in-graph(d);
  begin
    do(method (n1, dv) n1.graph-node-mark := list(dv); end method, n,
       *dag-variables*);
    n;
  end;
  begin
    do(method (p)
         p.graph-node-mark
          := concatenate(p.graph-node-mark, list(p.graph-node-class),
                         list(map(method (a)
                                    list(a.arc-label,
                                         head(a.arc-destination
                                              .graph-node-mark));
                                  end method,
                                  p.graph-node-arcs)));
       end method,
       n);
    n;
  end;
  map(graph-node-mark, n);
end method graph->print;

//  Function PRINT->GRAPH
// 
//  Takes a dag coded in list format and returns a dag-structure.
//  (The list format code described above is decoded back into a 
//  structure).  This function orders the arcs leaving a node 
//  lexicographically, so that exactly the same structure will appear
//  no matter how many times it is coded and decoded.
define method print->graph (dp)
  let n = map(method (n1) create-graph-node(mark: n1); end method, dp);
  begin
    do(method (p)
         p.graph-node-mfset := #f;
         p.graph-node-class := second(p.graph-node-mark);
         let list13156 = third(p.graph-node-mark);
         begin
           do(method (a)
                add-arc-in-order(p,
                                 create-arc(label: first(a),
                                            destination: cl-find(second(a),
                                                                 n,
                                                                 key: method (x)
                                                                      head(x
                                                                           .graph-node-mark);
                                                                      end method)));
              end method,
              list13156);
           list13156;
         end;
       end method,
       n);
    n;
  end;
  head(n);
end method print->graph;

// ----------------------------------------------------------------------
//  DAG-MFSET
// ----------------------------------------------------------------------
//  This section contains functions for performing disjoint set operations
//  on dag nodes.
//  Function MF-ROOT-CLASS?
// 
//  Takes a node and returns T if the node is the root of its equivalence
//  class tree.
define method mf-root-class? (n)
  instance?(n.graph-node-mfset, <list>);
end method mf-root-class?;

//  Function MF-FIND
// 
//  Performs the FIND operation for UNION-FIND disjoint sets.  Given
//  a node in a dag-structure, it returns another node, namely the root 
//  of the equivalence class tree for the input node.  After the FIND,
//  the tree is made more shallow by adjustment of pointers to the root.
define method mf-find (x)
  for (q1 = nil then nil, t1 = x then x, until mf-root-class?(t1))
    push!(t1, q1);
    t1 := t1.graph-node-mfset;
  finally
    //  do path compression
    begin
      #();
      begin do(method (n) n.graph-node-mfset := t1; end method, q1); q1; end;
      t1;
    end;
  end for;
end method mf-find;

//  Function MF-UNION
// 
//  Performs the UNION operation for UNION-FIND disjoint sets.  Given
//  two nodes in dag-structures, it joins their equivalence class trees
//  and returns the new root.  Smaller trees are merged into larger ones,
//  helping keep balance.
define method mf-union (x, y)
  let x1 = mf-find(x);
  let y1 = mf-find(y);
  if (x1 == y1)
    //  already in the same equivalence class
    x1;
  else
    if (size(x1.graph-node-mfset) < size(y1.graph-node-mfset))
      y1.graph-node-mfset
       := concatenate!(y1.graph-node-mfset, x1.graph-node-mfset);
      x1.graph-node-mfset := y1;
      y1;
    else
      x1.graph-node-mfset
       := concatenate!(x1.graph-node-mfset, y1.graph-node-mfset);
      y1.graph-node-mfset := x1;
      x1;
    end if;
  end if;
end method mf-union;

//  Function MF-INIT
// 
//  Takes the root of a dag-structure and initializes the graph for
//  UNION-FIND operations.  Each node is essentially placed into a
//  singleton equivalence class.
define method mf-init (x)
  let list13156 = nodes-in-graph(x);
  begin
    do(method (n) n.graph-node-mfset := list(n); end method, list13156);
    list13156;
  end;
end method mf-init;

//  Function CREATE-RESULT-GRAPH
// 
//  Takes a dag-structure in which some of the nodes may have been
//  UNION'd together.  Returns a new dag-structure in which nodes in
//  the same equivalence classes have been merged together into single
//  nodes.  All of the nodes in the result graph are allocated anew.
define method create-result-graph (classes)
  begin
    do(method (n)
         let list13156 = n.graph-node-arcs;
         begin
           do(method (a)
                a.arc-destination := mf-find(a.arc-destination);
              end method,
              list13156);
           list13156;
         end;
       end method,
       classes);
    classes;
  end;
  head(classes);
end method create-result-graph;

define method create-result-graph-1 (d)
  let nodes = nodes-in-graph(d);
  let classes = remove(nodes, complement(mf-root-class?));
  let res = create-result-graph(classes);
  let list13156 = set-difference(nodes, classes);
  begin do(dispose-graph-node, list13156); list13156; end;
  res;
end method create-result-graph-1;

define method create-result-graph-2 (d1, d2)
  let nodes
      = begin
          let n1 = nodes-in-graph(d1);
          let n2 = nodes-in-graph(d2);
          concatenate(list(head(n1), head(n2)), tail(n1), tail(n2));
        end;
  let classes = remove(nodes, complement(mf-root-class?));
  let res = create-result-graph(classes);
  let list13156 = set-difference(nodes, classes);
  begin do(dispose-graph-node, list13156); list13156; end;
  res;
end method create-result-graph-2;

// ----------------------------------------------------------------------
//  DAG-UNIFY
// ----------------------------------------------------------------------
//  This section contains functions for unifying two dag-structures.
//  Function CARRY-LABELS
// 
//  Adds the arcs of n1 to n2.
define method carry-labels (n1, n2)
  let list13156 = n1.graph-node-arcs;
  begin do(method (l) add-arc(n2, l); end method, list13156); list13156; end;
end method carry-labels;

//  Functions for testing if a class is atomic or disjunctive, etc.
//
define method atomic-class (c)
  not(instance?(c, <list>));
end method atomic-class;

define method disj-class (c)
  list(c) & head(c) == #"*or*";
end method disj-class;

define method neg-class (c) list(c) & head(c) == #"*not*"; end method neg-class;

define method mult-class (c)
  list(c) & head(c) == #"*mult*";
end method mult-class;

//  Function UNIFY-CLASSES
// 
//  Unifies two classes.  The labels may be atomic, disjunctive, negated, 
//  or multiple.
define method unify-classes (c1, c2)
  if (c1 == *variable*)
    c2;
  elseif (c2 == *variable*)
    c1;
  elseif (atomic-class(c1) & atomic-class(c2))
    if (c1 == c2) c1; else *fail*; end if;
  elseif (atomic-class(c1) & disj-class(c2))
    if (member?(c1, tail(c2))) c1; else *fail*; end if;
  elseif (atomic-class(c1) & neg-class(c2))
    if (member?(c1, tail(c2))) *fail*; else c1; end if;
  elseif (atomic-class(c1) & mult-class(c2))
    pair(head(c2), union(list(c1), tail(c2)));
  elseif (disj-class(c1) & atomic-class(c2))
    if (member?(c2, tail(c1))) c2; else *fail*; end if;
  elseif (disj-class(c1) & disj-class(c2))
    begin
      let new = intersection(tail(c1), tail(c2));
      if (empty?(new))
        *fail*;
      elseif (empty?(tail(new)))
        head(new);
      else
        pair(head(c1), new);
      end if;
    end;
  elseif (disj-class(c1) & neg-class(c2))
    begin
      let new = set-difference(tail(c1), tail(c2));
      if (empty?(new))
        *fail*;
      elseif (empty?(tail(new)))
        head(new);
      else
        pair(head(c1), new);
      end if;
    end;
  elseif (disj-class(c1) & mult-class(c2))
    *fail*;
  elseif (neg-class(c1) & atomic-class(c2))
    if (member?(c2, tail(c1))) *fail*; else c2; end if;
  elseif (neg-class(c1) & disj-class(c2))
    begin
      let new = set-difference(tail(c2), tail(c1));
      if (empty?(new))
        *fail*;
      elseif (empty?(tail(new)))
        head(new);
      else
        pair(head(c2), new);
      end if;
    end;
  elseif (neg-class(c1) & neg-class(c2))
    pair(head(c1), union(tail(c1), tail(c2)));
  elseif (neg-class(c1) & mult-class(c2))
    *fail*;
  elseif (mult-class(c1) & atomic-class(c2))
    pair(head(c1), union(list(c2), tail(c1)));
  elseif (mult-class(c1) & disj-class(c2))
    pair(head(c1), pair(c2, tail(c1)));
  elseif (mult-class(c1) & neg-class(c2))
    *fail*;
  elseif (mult-class(c1) & mult-class(c2))
    pair(head(c1), union(tail(c1), tail(c2)));
  end if;
end method unify-classes;

//  Function GRAPH-UNIFY
// 
//  Unifies two graphs.  Congruence closure algorithm, runs in 
//  O(n log n) time, where n is the number of nodes in the input graphs.
define method graph-unify (d1, d2)
  mf-init(d1);
  mf-init(d2);
  let e1 = copy-graph(d1);
  let e2 = copy-graph(d2);
  block (return)
    for (pairs = list(cons(e1, e2)) then list(cons(e1, e2)),
         current = nil then nil, u = nil then nil, v = nil then nil,
         newclass = nil then nil, w = nil then nil, until empty?(pairs))
      current := pop!(pairs);
      u := mf-find(head(current));
      v := mf-find(tail(current));
      newclass := unify-classes(u.graph-node-class, v.graph-node-class);
      if (newclass == *fail*) return(*fail*); end if;
      if (~ (u.graph-node-class = *variable*) & ~ empty?(v.graph-node-arcs)
           | (~ (v.graph-node-class = *variable*)
               & ~ empty?(u.graph-node-arcs)))
        return(*fail*);
      end if;
      w := mf-union(u, v);
      w.graph-node-class := newclass;
      if (w == v) carry-labels(u, v); else carry-labels(v, u); end if;
      let list13156
          = intersection(graph-node-arc-labels(u), graph-node-arc-labels(v));
      begin
        do(method (l)
             push!(pair(graph-node-subnode(u, l), graph-node-subnode(v, l)),
                   pairs);
           end method,
           list13156);
        list13156;
      end;
    finally
      create-result-graph-2(e1, e2);
    end for;
  end block;
end method graph-unify;

// ----------------------------------------------------------------------
// 
//  Examples
// 
// ----------------------------------------------------------------------
define variable g1 = #f;

define variable g2 = #f;

define variable g3 = #f;

define variable g4 = #f;

define variable g5 = #f;

define variable g6 = #f;

define variable g7 = #f;

define variable g8 = #f;

define variable g9 = #f;

define variable g10 = #f;

define variable g11 = #f;

define variable g12 = #f;

define variable g13 = #f;

define variable g14 = #f;

g1 := tree->graph(#(#(#"a", 1), #(#"b", 2)));

g2 := tree->graph(#(#(#"b", 2), #(#"c", 3)));

g3 := tree->graph(#(#(#"b", 3), #(#"d", 4)));

//  (graph-unify g1 g2) --> ((a 1) (b 2) (c 3))
//  (graph-unify g1 g3) --> FAIL
g4 := tree->graph(#(#(#"a", #(#"*or*", 1, 2, 3)), #(#"b", 5)));

g5 := tree->graph(#(#(#"a", #(#"*or*", 2, 3, 4)), #(#"b", 5)));

//  (graph-unify g4 g5) --> ((a (*OR* 2 3)) (b 5))
g6 := tree->graph(#(#(#"a", #(#"*not*", 1, 3)), #(#"b", 5)));

g7 := tree->graph(#(#(#"a", #(#"*or*", 1, 2, 3)), #(#"b", 5)));

g8 := tree->graph(#(#(#"a", #(#"*mult*", 1, 2, 3)), #(#"b", 5)));

g9 := tree->graph(#(#(#"a", #(#"*mult*", 2, 3, 4)), #(#"b", 5)));

//  (graph-unify g8 g9) --> ((a (*MULT* 4 1 2 3)) (b 5))
g10
 := tree->graph(#(#(#"a", #(#(#"b", #(#(#"c", 4), #(#"d", 6))))), #(#"e", 7)));

g11
 := tree->graph(#(#(#"a", #(#(#"b", #(#(#"d", 6))))), #(#"e", 7), #(#"f", 9)));

//  (graph-unify g10 g11) --> ((a ((b ((c 4) (d c)))) (e 7) (f 9))
g12
 := print->graph(#(#(#"$0000", #"var",
                     #(#(#"a", #"$0001"), #(#"b", #"$0001"))),
                   #(#"$0001", #"var", #())));

g13
 := print->graph(#(#(#"$0000", #"var",
                     #(#(#"a", #"$0001"), #(#"b", #"$0002"))),
                   #(#"$0001", 4, #()), #(#"$0002", 4, #())));

g14
 := print->graph(#(#(#"$0000", #"var",
                     #(#(#"a", #"$0001"), #(#"b", #"$0002"))),
                   #(#"$0001", 4, #()), #(#"$0002", 5, #())));

//  (graph-unify g12 g13) --> ((a 4) (b 4))
//  (graph-unify g12 g14) --> FAIL
//  (graph->print (graph-unify g12 g13)) --> 
// 			(($0000 VAR ((a $0001) (b $0001) ($0001 4 nil))
// ----------------------------------------------------------------------
// 
//  NATURAL LANGUAGE EXAMPLE
// 
// ----------------------------------------------------------------------
define variable np-graph = #f;

define variable vp-graph = #f;

define variable constituents = #f;

define variable s-to-np-vp-rule = #f;

//  Graph representing the noun phrase "the man".  The category arc has
//  the value NP, and the head arc contains information about determiner,  
//  root, and agreement.
np-graph
 := print->graph(#(#(#"$0001", #"var",
                     #(#(#"category", #"$0002"), #(#"head", #"$0003"))),
                   #(#"$0002", #"np", #()),
                   #(#"$0003", #"var",
                     #(#(#"det", #"$0004"), #(#"root", #"$0005"),
                       #(#"agreement", #"$0006"))),
                   #(#"$0004", #"the", #()), #(#"$0005", #"man", #()),
                   #(#"$0006", #"singular", #())));

//  Graph representing the verb phrase "kills bugs".
vp-graph
 := print->graph(#(#(#"$0007", #"var",
                     #(#(#"category", #"$0008"), #(#"head", #"$0009"))),
                   #(#"$0008", #"vp", #()),
                   #(#"$0009", #"var",
                     #(#(#"root", #"$0010"), #(#"tense", #"$0011"),
                       #(#"agreement", #"$0012"), #(#"object", #"$0013"))),
                   #(#"$0010", #"kill", #()), #(#"$0011", #"present", #()),
                   #(#"$0012", #"singular", #()),
                   #(#"$0013", #"var",
                     #(#(#"category", #"$0014"), #(#"head", #"$0015"))),
                   #(#"$0014", #"np", #()),
                   #(#"$0015", #"var",
                     #(#(#"root", #"$0016"), #(#"agreement", #"$0017"))),
                   #(#"$0016", #"bug", #()), #(#"$0017", #"plural", #())));

//  Graph tying NP-GRAPH and VP-GRAPH into a single constituent graph with
//  arcs labeled CONSTIT1 and CONSTIT2.
constituents
 := create-graph-node(arcs: list(create-arc(label: #"constit1",
                                            destination: np-graph),
                                 create-arc(label: #"constit2",
                                            destination: vp-graph)));

//  Graph representing the augmented context-free grammar rule S -> NP VP,
//  enforcing subject-object number agreement, and building the resulting 
//  structure for a sentence.
s-to-np-vp-rule
 := print->graph(#(#(#"$0000", #"var",
                     #(#(#"constit1", #"$0001"), #(#"constit2", #"$0002"),
                       #(#"build", #"$0003"))),
                   #(#"$0001", #"var",
                     #(#(#"category", #"$0004"), #(#"head", #"$0007"))),
                   #(#"$0002", #"var",
                     #(#(#"category", #"$0005"), #(#"head", #"$0008"))),
                   #(#"$0003", #"var",
                     #(#(#"category", #"$0006"), #(#"head", #"$0008"))),
                   #(#"$0004", #"np", #()), #(#"$0005", #"vp", #()),
                   #(#"$0006", #"s", #()),
                   #(#"$0007", #"var", #(#(#"agreement", #"$0009"))),
                   #(#"$0008", #"var",
                     #(#(#"subject", #"$0007"), #(#"agreement", #"$0009"),
                       #(#"mood", #"$0010"))),
                   #(#"$0009", #"var", #()), #(#"$0010", #"declarative", #())));

//  Unify rule with constituents...
// 
//  (graph-unify s-to-np-vp-rule constituents)
//  Unify rule with constituents, and retrieve result...
// 
//  (graph-node-subnode (graph-unify s-to-np-vp-rule constituents) 'build)
//  (graph-unify s-to-np-vp-rule constituents) ->
// 
//  ((BUILD
//    ((CATEGORY S)
//     (HEAD
//      ((TENSE PRESENT) (ROOT KILL)
//       (OBJECT
//        ((CATEGORY NP) (HEAD ((AGREEMENT PLURAL) (ROOT BUG)))))
//       (AGREEMENT SINGULAR) (MOOD DECLARATIVE)
//       (SUBJECT ((ROOT MAN) (DET THE) (AGREEMENT SINGULAR)))))))
//   (CONSTIT1
//    ((CATEGORY NP) (HEAD ((ROOT MAN) (DET THE) (AGREEMENT SINGULAR)))))
//   (CONSTIT2
//    ((CATEGORY VP)
//     (HEAD
//      ((TENSE PRESENT) (ROOT KILL)
//       (OBJECT
//        ((CATEGORY NP) (HEAD ((AGREEMENT PLURAL) (ROOT BUG)))))
//       (AGREEMENT SINGULAR) (MOOD DECLARATIVE)
//       (SUBJECT ((ROOT MAN) (DET THE) (AGREEMENT SINGULAR))))))))
//  (graph-node-subnode (graph-unify s-to-np-vp-rule constituents) 'build) ->
// 
//  ((CATEGORY S)
//   (HEAD
//    ((TENSE PRESENT) (ROOT KILL)
//     (OBJECT ((CATEGORY NP) (HEAD ((AGREEMENT PLURAL) (ROOT BUG)))))
//     (AGREEMENT SINGULAR) (MOOD DECLARATIVE)
//     (SUBJECT ((ROOT MAN) (DET THE) (AGREEMENT SINGULAR))))))
"eof";

