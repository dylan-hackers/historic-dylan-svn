module: path
 

// Constants.
define constant <point-list> = <list>;
define constant <path-cost> = <integer>;


// Prioritized location type for the priority queue.
define sealed class <prioritized-location> (<object>)
  // Priority is the cost-to-here + estimate-to-target.
  slot p :: <path-cost>,
    required-init-keyword: p:;

  // The cost-to-here is conventionally denoted as g.
  slot g :: <path-cost>,
    required-init-keyword: g:;

  // The parent node is used to restore the path from the result.
  slot parent :: false-or(<prioritized-location>),
    required-init-keyword: parent:;

  // The actual point on the board is stored here.
  slot pt :: <point>,
    required-init-keyword: pt:;
end;


define sealed domain make(singleton(<prioritized-location>));
define sealed domain initialize(<prioritized-location>);


// Priority queue of proprity location elements.
// Should use more efficient implementation by Andreas shortly. :-)
define class <priority-queue> (<sequence>)
  slot elements :: <list>, init-value: #();
end;


define method add!(q :: <priority-queue>, pLoc :: <prioritized-location>)
 => (res :: <priority-queue>)
  q.elements := sort!(add!(q.elements, pLoc),
                      test: method(a :: <prioritized-location>,
                                   b :: <prioritized-location>)
                                a.p < b.p;
                            end);
  q;
end;


define method get(q :: <priority-queue>)
 => (res :: false-or(<prioritized-location>))
  if (q.elements.empty?)
    #f;
  else
    let result = q.elements.head;
    q.elements := q.elements.tail;
    result;
  end if;
end;


define method empty?(q :: <priority-queue>) 
 => (res :: <boolean>)
  q.elements.empty?;
end;


define method get-given-point(ptmbr :: <point>, q :: <priority-queue>)
 => (res :: false-or(<prioritized-location>))
  block(in-there)
    for (elt in q.elements)
      if (elt.pt = ptmbr)
        in-there(elt);
      end if;
    end for;
    #f;
  end;
end;


// Useful distance-cost function for the board. Manhattan cost to avoid square root.
define inline function distance-cost(source :: <point>,
                                     target :: <point>)
 => (res :: <path-cost>)
  abs(source.x - target.x) + abs(source.y - target.y);
end function distance-cost;


// Get the points you can move to from where you are at. Possibly
// already visited.
define function get-successors(p :: <point>,
                               board :: <board>)
 => (res :: <point-list>)
  let nodes :: <list> = #();

  let north = point(x: p.x, y: p.y + 1);
  if (passable?(board, north))
    nodes := add!(nodes, north);
  end if;
  
  let east = point(x: p.x + 1, y: p.y);
  if (passable?(board, east))
    nodes := add!(nodes, east);
  end if;
  
  let south = point(x: p.x, y: p.y - 1);
  if (passable?(board, south))
    nodes := add!(nodes, south);
  end if;
  
  let west = point(x: p.x - 1, y: p.y);
  if (passable?(board, west))
    nodes := add!(nodes, west);
  end if;
  
  nodes;
end function get-successors;


// Simple version of A*.
define function find-path(source :: <point>,
                          target :: <point>,
                          board :: <board>,
                          #key cutoff :: false-or(<path-cost>))
 => (res :: false-or(<point-list>))

  if (cutoff & distance-cost(source, target) >= cutoff)
    debug("### cutting off: source: %=, target: %=, cutoff: %=\n", source, target, cutoff);
  end;

unless (cutoff & distance-cost(source, target) >= cutoff)

  let open :: <priority-queue> = make(<priority-queue>);
  let closed :: <list> = #();
  open := add!(open, make(<prioritized-location>,
                          p: distance-cost(source, target),
                          g: 0,
                          parent: #f,
                          pt: source));
  block(path-found)
    while(~open.empty?)
      let current :: <prioritized-location> = open.get;
      if (current.pt = target)
        // Construct path.
        let path :: <point-list> = #();
        while (current.parent ~= #f)
          path := add!(path, current.pt);
          current := current.parent;
        end while;
        path-found(path);
      else
        let nodes :: <point-list> = get-successors(current.pt, board);
        for (node in nodes)
          let newg = current.g + distance-cost(current.pt, node);
          let nodeLocation = get-given-point(node, open);
          let nlg :: <path-cost> = current.g + 1;
          if (nodeLocation ~= #f)
            nlg := nodeLocation.g;
          end if;
          if ((nlg > newg) | ((nodeLocation = #f) & ~member?(node, closed, test: \=)))
            if (member?(node, closed, test: \=))
              closed := remove!(closed, node);
            end if;
            if (nodeLocation = #f)
              open := add!(open, make(<prioritized-location>,
                                      p: distance-cost(node, target) + newg,
                                      g: newg,
                                      parent: current,
                                      pt: node));
            end if;
          end if;
        end for;
        closed := add!(closed, current.pt);
      end if;
    end while;
    #f;
  end;
end unless;
end function find-path;

//
// Adding memoized path-length to path.dylan

define constant $not-memoized = #"Not memoized";

/* 

define function path-length (p1 :: <point>, p2 :: <point>, b :: <board>)
 => (len :: false-or(<integer>))
  let path = find-path(p1, p2, b);
  if (path)
    path.size
  else
    #f
  end if;
end function path-length;

*/

define function path-length (p1 :: <point>, p2 :: <point>, b :: <board>)
 => (len :: false-or(<integer>))
  let a = cons(p1, p2);
  let dist = element(b.path-length-cache, a, default: $not-memoized);
  when (dist = $not-memoized)
    let path = find-path(p1, p2, b);
    if (path)
      b.path-length-cache[a] := path.size;
    else
      b.path-length-cache[a] := #f;
    end if;
  end when;
  //
  b.path-length-cache[a];
end function path-length;

