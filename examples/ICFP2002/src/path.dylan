module: path


// Constants.
define constant <path-list> = <list>;
define constant <path-cost> = <integer>


// Prioritised location type.
define sealed class <prioritized-location> (<object>)
  slot p :: <integer>, required-init-keyword: p:;
  slot x :: <integer>, required-init-keyword: x:;
  slot y :: <integer>, required-init-keyword: y:;
end;


define sealed domain make(singleton(<prioritized-location>));
define sealed domain initialize(<prioritized-location>);


define sealed method \= (a :: <prioritized-location>, b :: <prioritized-location>) => (<boolean>)
  (a.p = b.p) & (a.x = b.x) & (a.y = b.y)
end;

 
// Priority queue of proprity location elements.
define class priority-queue (<object>)
  slot elements :: <list>, init-value: #();
end;


define method add(q :: <priority-queue>, pLoc :: <prioritizedLocation) => ()
  q.elements := sort!(q.elements.add!(pLoc),
                      test: method(a :: <prioritized-location>,
                                   b :: <prioritized-location>)
                                a.p < b.p;
                            end);
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
define function distance-cost(sX :: <integer>, sY :: <integer>,
                              tX :: <integer>, tY :: <integer>) => (<pathcost>)
  abs(sX - tX) + abs(sY - tY);
end function distance-cost;


// Useful to query around you with nodes not in visited.
define function get-neighbors(x :: <integer>, y :: <integer>,
                                           board :: <board>, visited :: <list>) => (<list>)
  let nodes :: <list> = $();

  if (x > 0 & ~member(pair(x - 1, y), visited, test: \=) & passable?(board, x - 1, y))
    nodes.add(pair(x - 1, y));
  end if;

  if (x < board.width & ~member(pair(x + 1, y), visited, test: \=) & passable?(board, x + 1, y))
    nodes.add(pair(x + 1, y));
  end if;

  if (y > 0 & ~member(pair(x, y - 1), visited, test: \=) & passable?(board, x, y + 1))
    nodes.add(pair(x, y - 1));
  end if;

  if (y < board.height & ~member(pair(x, y + 1), visited, test: \=) & passable?(board, x, y + 1))
    nodes.add(pair(x, y + 1));
  end if;

  visited := concatenate!(visited, nodes);

  nodes;
end function get-unvisited-nodes-around;


// A* path finder.
define function find-path(sX :: <integer>, sY :: <integer>,
                          tX :: <integer>, tY :: <integer>,
                          board :: <board>) => (res :: false-or(<list>))
  let pQ :: <priority-queue> = make(<priority-queue>);
  add(pQ, make(<prioritized-location>, p: 0, x: sX, y: sY));

  let path :: <list> = #();
  let pathCost :: <path-cost> = 0;
  let visited :: <list> = #();
  
  block (pair-found)
    while (~queue.empty?)
      let cur :: <prioritized-location> = pQ.get;
      path = path.add!(pair(cur.x, cur.y) );
      pathCost++;

      if (cur.x = tX & cur.y = tY)
        pair-found(path);
      else
        let nodes :: <list> = get-neighbors(cur.x, cur.y, board, visited);
        for (node in nodes)
          let pLoc = make(<prioritized-location>,
                          p: distance-cost(cur.x, cur.y, tX, tY) + pathCost,
                          x: node.x,
                          y: node.y);
          add(pQ, pLoc);
        end for;
      end if;
    end;
    path;
  end;
end function find-path;
