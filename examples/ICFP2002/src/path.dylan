module: path


// Constants.
define constant <path-list> = <list>;
define constant <path-cost> = <integer>;


// Prioritised location type.
define sealed class <prioritized-location> (<object>)
  slot p :: <path-cost>, required-init-keyword: p:;
  slot pt :: <point>, required-init-keyword: point:;
end;


define sealed domain make(singleton(<prioritized-location>));
define sealed domain initialize(<prioritized-location>);


define sealed method \= (a :: <prioritized-location>, b :: <prioritized-location>)
 => (res :: <boolean>)
  (a.p = b.p) & (a.pt = b.pt)
end;

 
// Priority queue of proprity location elements.
define class <priority-queue> (<sequence>)
  slot elements :: <list>, init-value: #();
end;


define method add!(q :: <priority-queue>, pLoc :: <prioritized-location>)
 => (q :: <priority-queue>)
  q.elements := sort!(add!(q.elements,pLoc),
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


// Useful distance-cost function for the board. Manhattan cost to avoid square root.
define function distance-cost(source :: <point>,
                              dest :: <point>)
 => (res :: <path-cost>)
  abs(source.x - dest.x) + abs(source.y - dest.y);
end function distance-cost;


// Useful to query around you with nodes not in visited.
define function get-neighbors(p :: <point>,
                              board :: <board>, visited :: <list>)
 => (res :: <list>, visited :: <list>)
  let nodes :: <list> = #();

  let north = point(x: p.x,     y: p.y + 1);
  if (p.y < board.height & ~member?(north, visited, test: \=) &
	passable?(board, north))
    nodes := add!(nodes, north);
  end if;

  let east = point( x: p.x + 1, y: p.y);
  if (p.x < board.width & ~member?(east, visited, test: \=) & passable?(board, east))
    nodes := add!(nodes, east);
  end if;

  let south = point(x: p.x,     y: p.y - 1);
  if (p.y > 0 & ~member?(south, visited, test: \=) & passable?(board, south))
    nodes := add!(nodes, south);
  end if;

  let west = point( x: p.x - 1, y: p.y);
  if (p.x > 0 & ~member?(west, visited, test: \=) & passable?(board, west))
    nodes := add!(nodes, west);
  end if;


  visited := concatenate!(visited, nodes);

  values(nodes, visited);
end function get-neighbors;


// A* path finder.
define function find-path(source :: <point>, dest :: <point>,
                          board :: <board>) => (res :: false-or(<list>))
  let pQ :: <priority-queue> = make(<priority-queue>);
  add(pQ, make(<prioritized-location>, p: 0, point: source));

  let path :: <list> = #();
  let pathCost :: <path-cost> = 0;
  let visited :: <list> = #();
  
  block (path-found)
    while (~pQ.empty?)
      let cur :: <prioritized-location> = pQ.get;
      path = add!(path, cur.pt );
      pathCost := pathCost + 1;

      if (cur.pt = dest)
        path-found(path);
      else
        let (nodes, new-visited) = get-neighbors(cur.pt, board, visited);
        visited := new-visited;
        
        if (nodes.empty?)
          path := path.tail;
          pathCost := pathCost - 1;

          if (path.empty?)
            path-found(#f);
          end if;

          // The following should put us on the top of the queue, given that we got here.
          add(pQ, make(<prioritized-location>,
                       p: distance-cost(path.head, dest) + pathCost,
                       point: path.head));
        else
          for (node in nodes)
            add(pQ, make(<prioritized-location>,
                         p: distance-cost(node, dest) + pathCost,
                         point: node));
          end for;
        end if;
      end if;
    end;
    #f;
  end;
end function find-path;
