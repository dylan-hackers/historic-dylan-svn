module: path


// Constants.
define constant <path-list> = <list>;
define constant <path-cost> = <integer>;


// Prioritised location type.
define sealed class <prioritized-location> (<object>)
  slot p :: <path-cost>, required-init-keyword: p:;
  slot x :: <coordinate>, required-init-keyword: x:;
  slot y :: <coordinate>, required-init-keyword: y:;
end;


define sealed domain make(singleton(<prioritized-location>));
define sealed domain initialize(<prioritized-location>);


define sealed method \= (a :: <prioritized-location>, b :: <prioritized-location>)
 => (res :: <boolean>)
  (a.p = b.p) & (a.x = b.x) & (a.y = b.y)
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
    q.elements = q.elements.tail;
    result;
  end if;
end;


// Useful distance-cost function for the board. Manhattan cost to avoid square root.
define function distance-cost(sX :: <coordinate>, sY :: <coordinate>,
                              tX :: <coordinate>, tY :: <coordinate>)
 => (res :: <path-cost>)
  abs(sX - tX) + abs(sY - tY);
end function distance-cost;


// Useful to query around you with nodes not in visited.
define function get-neighbors(x :: <coordinate>, y :: <coordinate>,
                              board :: <board>, visited :: <list>)
 => (res :: <list>, visited :: <list>)
  let nodes :: <list> = #();

  if (x > 0 & ~member?(pair(x - 1, y), visited, test: \=) & passable?(board, x - 1, y))
    nodes := add!(nodes, pair(x - 1, y));
  end if;

  if (x < board.width & ~member?(pair(x + 1, y), visited, test: \=) & passable?(board, x + 1, y))
    nodes := add!(nodes, pair(x + 1, y));
  end if;

  if (y > 0 & ~member?(pair(x, y - 1), visited, test: \=) & passable?(board, x, y + 1))
    nodes := add!(nodes, pair(x, y - 1));
  end if;

  if (y < board.height & ~member?(pair(x, y + 1), visited, test: \=) & passable?(board, x, y + 1))
    nodes := add!(nodes, pair(x, y + 1));
  end if;

  visited := concatenate!(visited, nodes);

  values(nodes, visited);
end function get-neighbors;


// A* path finder.
define function find-path(sX :: <coordinate>, sY :: <coordinate>,
                          tX :: <coordinate>, tY :: <coordinate>,
                          board :: <board>) => (res :: false-or(<list>))
  let pQ :: <priority-queue> = make(<priority-queue>);
  add(pQ, make(<prioritized-location>, p: 0, x: sX, y: sY));

  let path :: <list> = #();
  let pathCost :: <path-cost> = 0;
  let visited :: <list> = #();
  
  block (path-found)
    while (~pQ.empty?)
      let cur :: <prioritized-location> = pQ.get;
      path = add!(path, pair(cur.x, cur.y) );
      pathCost := pathCost + 1;

      if (cur.x = tX & cur.y = tY)
        path-found(path);
      else
        let (nodes, new-visited) = get-neighbors(cur.x, cur.y, board, visited);
        visited := new-visited;
        
        if (nodes.empty?)
          path := path.tail;
          pathCost := pathCost - 1;

          if (path.empty?)
            path-found(#f);
          end if;

          // The following should put us on the top of the queue, given that we got here.
          add(pQ, make(<prioritized-location>,
                       p: distance-cost(path.head.x, path.head.y, tX, tY) + pathCost,
                       x: path.head.x,
                       y: path.head.y));
        else
          for (node in nodes)
            add(pQ, make(<prioritized-location>,
                         p: distance-cost(node.x, node.y, tX, tY) + pathCost,
                         x: node.x,
                         y: node.y));
          end for;
        end if;
      end if;
    end;
    #f;
  end;
end function find-path;
