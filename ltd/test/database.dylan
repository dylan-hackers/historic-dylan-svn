//  -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-
// ----------------------------------------------------------------------------
// 
// 	File		Database.Lisp
// 	System		Don's Theorem Prover
// 
// 	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
"(in-package dtp)";

define module dtp
  export empty-theory, make-theory-from-sentences, save-sentence-in-theory, drop-sentence-from-theory, sentences-in;
end module dtp;

// ----------------------------------------------------------------------------
// List of THEORY structures
define variable *kb* = #f;

define variable *id-index* = make(<table>, test: \==);

// ----------------------------------------------------------------------------
define class <theory> (<object>)
  slot theory-name, init-keyword: #"theory-name";
  slot theory-nodes = #f, init-keyword: #"theory-nodes";
  slot theory-indexed-nodes = #f, init-keyword: #"theory-indexed-nodes";
end class <theory>;

define class <node-index> (<object>)
  slot node-index-key, init-keyword: #"node-index-key";
  slot node-index-nodes = #f, init-keyword: #"node-index-nodes";
end class <node-index>;

// ----------------------------------------------------------------------------
define method theory-print-function (structure, stream, depth)
  (formatter-1("<Theory ~A with ~D node~:P>"))(stream,
                                               structure.theory-name,
                                               size(structure.theory-nodes));
end method theory-print-function;

define method node-index-print-function (structure, stream, depth)
  (formatter-1("<Index ~A with ~D node~:P>"))(stream,
                                              structure.node-index-key,
                                              size(structure
                                                   .node-index-nodes));
end method node-index-print-function;

// ----------------------------------------------------------------------------
define method reset-database ()
  *id-index* := make(<table>, test: \==);
  *kb* := #f;
end method reset-database;

// ----------------------------------------------------------------------------
define method empty-theory (theory-name)
  let theory = get-theory-structure(theory-name);
  if (theory)
    for (node in theory.theory-nodes) unindex-id(kb-node-id(node)); end for;
    *kb* := remove(*kb*, theory);
    theory-name;
  end if;
end method empty-theory;

// ----------------------------------------------------------------------------
define method all-kb-theories ()
  map(theory-name, *kb*);
end method all-kb-theories;

// ----------------------------------------------------------------------------
define method get-theory-structure (theory-name)
  cl-find(theory-name, *kb*, key: theory-name);
end method get-theory-structure;

// ----------------------------------------------------------------------------
define method active-theory-contents (theory, index-on)
  let _acc = #();
  for (theory-name in included-active-theory-names(theory))
    _acc := concatenate(_acc, theory-contents(theory-name, index-on));
  finally
    _acc;
  end for;
end method active-theory-contents;

// ----------------------------------------------------------------------------
define method theory-contents (theory-name, #key index = #f)
  let theory = get-theory-structure(theory-name);
  if (empty?(theory))
    #f;
  elseif (index)
    begin
      let ni = #f;
      ni := cl-find(index, theory.theory-indexed-nodes, key: node-index-key);
      if (ni) ni.node-index-nodes; end if;
    end;
  else
    theory.theory-nodes;
  end if;
end method theory-contents;

// ----------------------------------------------------------------------------
define method make-theory-from-nodes (nodes, theory-name)
  empty-theory(theory-name);
  add-to-end(make-theory(name: theory-name, nodes: nodes,
                         indexed-nodes: make-index(nodes)),
             *kb*);
  begin
    do(method (node) index-id(kb-node-id(node), node); end method, nodes);
    nodes;
  end;
  theory-name;
end method make-theory-from-nodes;

// ----------------------------------------------------------------------------
define method save-node-in-theory (node, theory-name)
  let old-theory = get-theory-structure(theory-name);
  if (old-theory)
    old-theory.theory-indexed-nodes
     := make-index(list(node), old-theory.theory-indexed-nodes);
    add-to-end(node, old-theory.theory-nodes);
    index-id(kb-node-id(node), node);
  else
    make-theory-from-nodes(list(node), theory-name);
  end if;
  kb-node-id(node);
end method save-node-in-theory;

// ----------------------------------------------------------------------------
define method drop-node-from-theory (node, theory-name)
  unindex-id(kb-node-id(node));
  theory-name.theory-nodes := remove(theory-name.theory-nodes, node);
  for (index in theory-name.theory-indexed-nodes)
    index.node-index-nodes := remove(index.node-index-nodes, node);
  end for;
  theory-name.theory-indexed-nodes
   := remove(theory-name.theory-indexed-nodes, complement(node-index-nodes));
  kb-node-id(node);
end method drop-node-from-theory;

// ----------------------------------------------------------------------------
define method make-index (nodes, #key indices = #f)
  // Return a list of index nodes with a key for each relation in NODES
  block (return)
    for (node in nodes)
      for (literal in clause-literals(kb-node-clause(node)),
           relation = literal-relation(literal) then literal-relation(literal),
           index = cl-find(relation, indices,
                           key: node-index-key) then cl-find(relation,
                                                             indices,
                                                             key: node-index-key))
        if (index)
          add-to-end-if-new(node, index.node-index-nodes);
        else
          add-to-end(make-node-index(key: relation, nodes: list(node)),
                     indices);
        end if;
      end for;
    finally
      return(indices);
      #f;
    end for;
  end block;
end method make-index;

// ----------------------------------------------------------------------------
define method find-kb-node-with-id (id)
  *id-index*[id];
end method find-kb-node-with-id;

define method index-id (id, node)
  if (*id-index*[id])
    error("Trying to remap id %S from %S to %S", id, *id-index*[id], node);
  end if;
  *id-index*[id] := node;
end method index-id;

define method unindex-id (id)
  remove-key!(*id-index*, id);
end method unindex-id;

// ----------------------------------------------------------------------------
define method last-id-count (theory-name)
  let theory = get-theory-structure(theory-name);
  let id = #f;
  let str = #f;
  if (theory)
    id
     := kb-node-id(first(begin
                           let s14663 = theory.theory-nodes;
                           copy-sequence(s14663, start: size(s14663) - 1);
                         end));
    str := as(<string>, id);
    str := copy-sequence(str, size(as(<string>, theory-name)) + 1);
    values(// LTD: Function READ-FROM-STRING not yet implemented.
           read-from-string(str));
  else
    0;
  end if;
end method last-id-count;

// ----------------------------------------------------------------------------
// 
// 	Logic -> Nodes
// ----------------------------------------------------------------------------
define method make-theory-from-sentences (theory-name, sentence-label-pairs)
  let cnf-label-pairs = #f;
  let literal-lists = #f;
  let nodes = #f;
  block (return)
    cnf-label-pairs
     := map(method (slp)
              let label = tail(slp);
              map(method (s) pair(s, label); end method,
                  sentence-to-cnf(head(slp)));
            end method,
            sentence-label-pairs);
    cnf-label-pairs := apply(concatenate, cnf-label-pairs);
    literal-lists
     := map(method (x)
              list(map(list-to-literal, head(x)), tail(x));
            end method,
            cnf-label-pairs);
    nodes
     := block (return)
          let literal-list = #f;
          let label = #f;
          let g15984 :: <list> = literal-lists;
          block (return)
            let count :: <real> = 1;
            block (return)
              let g15985 = list(#f);
              let g15986 = g15985;
              block (return)
                let loop-not-first-time = #f;
                block (return)
                  local method go-end-loop ()
                          return-from-nil(tail(g15985));
                        end method go-end-loop,
                        method go-next-loop ()
                          if (not(pair?(g15984))) go-end-loop(); end if;
                          let loop-desetq-temp = head(g15984);
                          (literal-list := head(loop-desetq-temp));
                          (loop-desetq-temp := tail(loop-desetq-temp));
                          (label := head(loop-desetq-temp));
                          (g15984 := tail(g15984));
                          if (loop-not-first-time)
                            (count := count + 1);
                          else
                            (loop-not-first-time := #t);
                          end if;
                          (tail(g15986)
                            := (g15986
                                 := list(make-kb-node(id: make-new-id(theory-name,
                                                                      count),
                                                      clause: make-clause-node(literals: literal-list,
                                                                               label: label)))));
                          go-next-loop();
                          go-end-loop();
                        end method go-next-loop;
                  go-next-loop();
                end block;
              end block;
            end block;
          end block;
        end block;
    make-theory-from-nodes(nodes, theory-name);
    theory-name;
  end block;
end method make-theory-from-sentences;

// ----------------------------------------------------------------------------
define method save-sentence-in-theory (sentence,
                                       #key theory-name = *theory*,
                                       label = #f)
  let cnf = sentence-to-cnf(sentence);
  let literal-lists
      = map(method (x) map(list-to-literal, x); end method, cnf);
  let _acc = make(<deque>);
  for (literal-list in literal-lists,
       count from last-id-count(theory-name) + 1,
       id = make-new-id(theory-name, count) then make-new-id(theory-name,
                                                             count),
       new-node = make-kb-node(id: id,
                               clause: make-clause-node(literals: literal-list,
                                                        label: label)) then make-kb-node(id: id,
                                                                                         clause: make-clause-node(literals: literal-list,
                                                                                                                  label: label)))
    push-last(_acc, id);
    save-node-in-theory(new-node, theory-name);
  finally
    _acc;
  end for;
end method save-sentence-in-theory;

// ----------------------------------------------------------------------------
define method drop-sentence-from-theory (sentence,
                                         #key theory-name = *theory*,
                                         test = \=)
  // Locate the node(s) in the theory corresponding to SENTENCE and remove them
  let theory = get-theory-structure(theory-name);
  let success = make(<deque>);
  block (return)
    for (while theory, dnf in sentence-to-cnf(sentence))
      push-last(success,
                block (return)
                  for (node in theory.theory-nodes)
                    if (clause-list-equal-p(kb-node-clause(node),
                                            dnf,
                                            test: test))
                      drop-node-from-theory(node, theory);
                      return(kb-node-id(node));
                    end if;
                  end for;
                end block);
    finally
      return(remove(success, #f));
      success;
    end for;
  end block;
end method drop-sentence-from-theory;

// ----------------------------------------------------------------------------
define method sentences-in (theory-name, #key with-atom = #f)
  // Return list of sentences in theory named THEORY-NAME
  let sentences = make(<deque>);
  block (return)
    for (node in theory-contents(theory-name),
         clause = kb-node-clause(node) then kb-node-clause(node))
      push-last(sentences, simplify-dnf(pair(#"or", clause-to-list(clause))));
    finally
      if (with-atom)
        return(remove(sentences,
                      complement(method (x)
                                   tree-find(with-atom, x);
                                 end method)));
      else
        return(sentences);
      end if;
      sentences;
    end for;
  end block;
end method sentences-in;

// ----------------------------------------------------------------------------
"eof";

