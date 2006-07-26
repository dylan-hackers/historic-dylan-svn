//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File waltz.lisp: Line-labeling using Waltz filtering.
// A diagram is a list of vertexes.
define class <diagram> (<object>)
  slot diagram-vertexes, init-keyword: #"diagram-vertexes";
end class <diagram>;

define class <vertex> (<object>)
  slot vertex-name :: <atom> = #f, init-keyword: #"vertex-name";
  slot vertex-type :: one-of(#"l", #"y", #"w", #"t") = #"l",
       init-keyword: #"vertex-type";
  slot vertex-neighbors :: <list> = #f, init-keyword: #"vertex-neighbors";
  //  of vertex
  slot vertex-labelings :: <list> = #f, init-keyword: #"vertex-labelings";
end class <vertex>;

//  of lists of (member + - L R)))))
define method ambiguous-vertex-p (vertex)
  // A vertex is ambiguous if it has more than one labeling.
  number-of-labelings(vertex) > 1;
end method ambiguous-vertex-p;

define method number-of-labelings (vertex)
  size(vertex.vertex-labelings);
end method number-of-labelings;

define method impossible-vertex-p (vertex)
  // A vertex is impossible if it has no labeling.
  empty?(vertex.vertex-labelings);
end method impossible-vertex-p;

define method impossible-diagram-p (diagram)
  // An impossible diagram is one with an impossible vertex.
  any?(impossible-vertex-p, diagram.diagram-vertexes);
end method impossible-diagram-p;

define method possible-labelings (vertex-type)
  // The list of possible labelings for a given vertex type.
  //  In these labelings, R means an arrow pointing away from 
  //  the vertex, L means an arrow pointing towards it.
  select (vertex-type)
    (#"l")
       => #(#(#"r", #"l"), #(#"l", #"r"), #(#"+", #"r"), #(#"l", #"+"),
            #(#"-", #"l"), #(#"r", #"-"));
    (#"y")
       => #(#(#"+", #"+", #"+"), #(#"-", #"-", #"-"), #(#"l", #"r", #"-"),
            #(#"-", #"l", #"r"), #(#"r", #"-", #"l"));
    (#"t")
       => #(#(#"r", #"l", #"+"), #(#"r", #"l", #"-"), #(#"r", #"l", #"l"),
            #(#"r", #"l", #"r"));
    (#"w")
       => #(#(#"l", #"r", #"+"), #(#"-", #"-", #"+"), #(#"+", #"+", #"-"));
    otherwise
       => #f;
  end select;
end method possible-labelings;

define method print-labelings (diagram)
  // Label the diagram by propagating constraints and then
  //   searching for solutions if necessary.  Print results.
  show-diagram(diagram, "~&The initial diagram is:");
  every?(propagate-constraints, diagram.diagram-vertexes);
  show-diagram(diagram, "~2&After constraint propagation the diagram is:");
  let solutions
      = if (impossible-diagram-p(diagram))
          #f;
        else
          search-solutions(diagram);
        end if;
  let n = size(solutions);
  if (~ (n = 1))
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               let init = args;
               begin
                 multiple-newlines1(xp, fresh: 2);
                 write-string++("There are ", xp, 0, 10);
                 using-format(xp, "~r", pop!(args));
                 write-string++(" solution", xp, 0, 9);
                 if (~ (head(backup-in-list(1, init, args)) == 1))
                   write-char++('s', xp);
                 end if;
                 write-char++(':', xp);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(#t, n);
    begin do(show-diagram, solutions); solutions; end;
  end if;
  values();
end method print-labelings;

define method propagate-constraints (vertex)
  // Reduce the labelings on vertex by considering neighbors.
  //   If we can reduce, propagate the constraints to each neighbor.
  let old-num = number-of-labelings(vertex);
  vertex.vertex-labelings := consistent-labelings(vertex);
  if (~ impossible-vertex-p(vertex))
    if (number-of-labelings(vertex) < old-num)
      every?(propagate-constraints, vertex.vertex-neighbors);
    end if;
    #t;
  end if;
end method propagate-constraints;

define method consistent-labelings (vertex)
  // Return the set of labelings that are consistent with neighbors.
  let neighbor-labels
      = map(method (neighbor) labels-for(neighbor, vertex); end method,
            vertex.vertex-neighbors);
  //  Eliminate labelings that don't have all lines consistent
  //  with the corresponding line's label from the neighbor.
  //  Account for the L-R mismatch with reverse-label.
  find-all-if(method (labeling)
                every?(// LTD: Can't convert complex function MEMBER.
                       member, map(reverse-label, labeling), neighbor-labels);
              end method,
              vertex.vertex-labelings);
end method consistent-labelings;

define method search-solutions (diagram)
  // Try all labelings for one ambiguous vertex, and propagate.
  let v = cl-find-if(ambiguous-vertex-p, diagram.diagram-vertexes);
  if (empty?(v))
    list(diagram);
  else
    apply(concatenate!,
          map(method (v-labeling)
                let diagram2 = make-copy-diagram(diagram);
                let v2 = find-vertex(v.vertex-name, diagram2);
                v2.vertex-labelings := list(v-labeling);
                if (propagate-constraints(v2))
                  search-solutions(diagram2);
                else
                  #f;
                end if;
              end method,
              v.vertex-labelings));
  end if;
end method search-solutions;

define method labels-for (vertex, from)
  // Return all the labels for the line going to vertex.
  let pos = find-key(vertex.vertex-neighbors, curry(\==, from));
  map(method (labeling) labeling[pos]; end method, vertex.vertex-labelings);
end method labels-for;

define method reverse-label (label)
  // Account for the fact that one vertex's right is another's left.
  select (label) #"l" => #"r"; #"r" => #"l"; otherwise => label; end select;
end method reverse-label;

define method find-vertex (name, diagram)
  // Find the vertex in the given diagram with the given name.
  cl-find(name, diagram.diagram-vertexes, key: vertex-name);
end method find-vertex;

define method print-vertex (vertex, stream, depth)
  // Print a vertex in the short form.
  format(stream, "%S/%d", vertex.vertex-name, number-of-labelings(vertex));
  vertex;
end method print-vertex;

define method show-vertex (vertex, #key stream = #t)
  // Print a vertex in a long form, on a new line.
  format(stream, "\n   %S %d:", vertex, vertex.vertex-type);
  let list92543 = vertex.vertex-neighbors;
  begin
    do(method (neighbor, labels)
         (method (s, #rest args)
            apply(maybe-initiate-xp-printing,
                  method (xp, #rest args)
                    begin
                      write-char++(' ', xp);
                      fluid-bind (*print-escape* = #f)
                        write+(pop!(args), xp);
                      end fluid-bind;
                      fluid-bind (*print-escape* = #f)
                        write+(pop!(args), xp);
                      end fluid-bind;
                      write-string++("=[", xp, 0, 2);
                      let args = pop!(args);
                      block (return)
                        local method go-l ()
                                if (empty?(args)) return(#f); end if;
                                fluid-bind (*print-escape* = #f)
                                  write+(pop!(args), xp);
                                end fluid-bind;
                                go-l();
                              end method go-l;
                        go-l();
                      end block;
                      write-char++(']', xp);
                    end;
                    if (args) copy-sequence(args); end if;
                  end method,
                  s, args);
          end method)(stream, vertex.vertex-name, neighbor.vertex-name,
                      labels);
       end method,
       list92543, matrix-transpose(vertex.vertex-labelings));
    list92543;
  end;
  values();
end method show-vertex;

define method show-diagram (diagram, #key title = "~2&Diagram:", stream = #t)
  // Print a diagram in a long form.  Include a title.
  title(stream);
  let list92543 = diagram.diagram-vertexes;
  begin do(show-vertex, list92543); list92543; end;
  let n = reduce1(\*, map(number-of-labelings, diagram.diagram-vertexes));
  if (n > 1)
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               let init = args;
               begin
                 pprint-newline+(fresh: xp);
                 write-string++("For ", xp, 0, 4);
                 using-format(xp, "~:d", pop!(args));
                 write-string++(" interpretation", xp, 0, 15);
                 if (~ (head(backup-in-list(1, init, args)) == 1))
                   write-char++('s', xp);
                 end if;
                 write-char++('.', xp);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(stream, n);
  end if;
  values();
end method show-diagram;

define method matrix-transpose (matrix)
  // Turn a matrix on its side.
  if (matrix)
    apply(// LTD: Can't convert complex function MAPCAR.
          mapcar, list, matrix);
  end if;
end method matrix-transpose;

begin
  let diagrams = make(<table>);
  define method diagram (name)
    // Get a fresh copy of the diagram with this name.
    make-copy-diagram(diagrams[name]);
  end method diagram;
  define method put-diagram (name, diagram)
    // Store a diagram under a name.
    diagrams[name] := diagram;
    name;
  end method put-diagram;
end;

define method construct-diagram (vertex-descriptors)
  // Build a new diagram from a set of vertex descriptor.
  let diagram = make-diagram();
  //  Put in the vertexes
  diagram.diagram-vertexes := map(construct-vertex, vertex-descriptors);
  //  Put in the neighbors for each vertex
  for (v-d in vertex-descriptors)
    vertex-neighbors(find-vertex(first(v-d), diagram))
     := map(method (neighbor) find-vertex(neighbor, diagram); end method,
            v-d-neighbors(v-d));
  end for;
  diagram;
end method construct-diagram;

define method construct-vertex (vertex-descriptor)
  // Build the vertex corresponding to the descriptor.
  //  Descriptors are like: (x L y z)
  make-vertex(name: first(vertex-descriptor), type: second(vertex-descriptor),
              labelings: possible-labelings(second(vertex-descriptor)));
end method construct-vertex;

define method v-d-neighbors (vertex-descriptor)
  // The neighboring vertex names in a vertex descriptor.
  tail(tail(vertex-descriptor));
end method v-d-neighbors;

define method make-copy-diagram (diagram)
  // Make a copy of a diagram, preserving connectivity.
  let new
      = make-diagram(vertexes: map(copy-vertex, diagram.diagram-vertexes));
  //  Put in the neighbors for each vertex
  for (v in new.diagram-vertexes)
    v.vertex-neighbors
     := map(method (neighbor)
              find-vertex(neighbor.vertex-name, new);
            end method,
            v.vertex-neighbors);
  end for;
  new;
end method make-copy-diagram;

define method ground (diagram, vertex-a, vertex-b)
  // Attach the line between the two vertexes to the ground.
  //   That is, label the line with a -
  let a = find-vertex(vertex-a, diagram);
  let b = find-vertex(vertex-b, diagram);
  let i = find-key(a.vertex-neighbors, curry(\==, b));
  assert(~ empty?(i));
  a.vertex-labelings
   := find-all-if(method (l) l[i] == #"-"; end method, a.vertex-labelings);
  diagram;
end method ground;

define method find-labelings (diagram)
  // Return a list of all consistent labelings of the diagram.
  every?(propagate-constraints, diagram.diagram-vertexes);
  search-solutions(diagram);
end method find-labelings;

// LTD: No macros.
#"defdiagram";

define method check-diagram (vertex-descriptors)
  // Check if the diagram description appears consistent.
  let errors = 0;
  for (v-d in vertex-descriptors)
    let a = first(v-d);
    let v-type = second(v-d);
    //  Check that the number of neighbors is right for
    //  the vertex type (and that the vertex type is legal)
    if (size(v-d-neighbors(v-d))
         ~= select (v-type)
              (#"w", #"y", #"t")
                 => 3;
              (#"l")
                 => 2;
              otherwise
                 => -1;
            end select)
      format-out("Illegal type/neighbor combo: ~a", v-d);
      inc!(errors);
    end if;
    //  Check that each neighbor B is connected to
    //  this vertex, A, exactly once
    for (b in v-d-neighbors(v-d))
      if (1
           ~= cl-count-if(method (v-d2)
                            (first(v-d2) == b
                              & member?(a, v-d-neighbors(v-d2)));
                          end method,
                          vertex-descriptors))
        format-out("Inconsistent vertex: ~a-~a", a, b);
        inc!(errors);
      end if;
    end for;
  end for;
  if (errors > 0)
    error(method (s, #rest args)
            apply(maybe-initiate-xp-printing,
                  method (xp, #rest args)
                    let init = args;
                    begin
                      write-string++("Inconsistent diagram.  ", xp, 0, 23);
                      using-format(xp, "~d", pop!(args));
                      write-string++(" total error", xp, 0, 12);
                      if (~ (head(backup-in-list(1, init, args)) == 1))
                        write-char++('s', xp);
                      end if;
                      write-char++('.', xp);
                    end;
                    if (args) copy-sequence(args); end if;
                  end method,
                  s, args);
          end method,
          errors);
  end if;
  vertex-descriptors;
end method check-diagram;

