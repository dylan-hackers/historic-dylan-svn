module: path


define constant <pathlist> = <list>;


define sealed class <prioritisedLocation> (<object>)
  slot p :: <integer>, required-init-keyword: p:;
  slot x :: <integer>, required-init-keyword: x:;
  slot y :: <integer>, required-init-keyword: y:;
end;


define sealed domain make(singleton(<prioritisedLocation>));
define sealed domain initialize(<prioritisedLocation>);


define sealed method \= (a :: <prioritisedLocation>, b :: <prioritisedLocation>) => (<boolean>)
  (a.p = b.p) & (a.x = b.x) & (a.y = b.y)
end;


define class priorityQueue (<object>)
  slot elements :: <list>, init-value: #();
end;


define method add(q :: <priorityQueue>, pLoc :: <prioritiesLocation) => ()
  q.elements := sort!(q.elements.add!(pLoc),
                      test: method(a :: <prioritisedLocation>,
                                   b :: <prioritisedLocation>)
                                a.p < b.p;
                            end);
end;


define method get(q :: <priorityQueue>)
 => (res :: false-or(<prioritisedLocation>))
  if (q.elements.empty?)
    #f;
  else
    let result = q.elements.head;
    q.elements = q.elements.tail;
    result;
  end if;
end;


define function findpath(sX :: <integer>, sY :: <integer>,
                         tX :: <integer>, tY :: <integer>) => (<list>)
  let pQ :: <priorityQueue> = make(<priorityQueue>);
  add(pQ, make(<prioritisedLocation>, p: 1, x: sX, y: sY));

  let path = #();
  let final = pair(tX, tY);
  
  block (pair-found)
    while (~queue.empty?)
      let cur :: <priorityElement> = pQ.get;
      path = path.add!(pair(cur.x, cur.y) );
      if (cur = final)
        pair-found(path);
      else
        // get new nodes
        // eval nodes  use cost-so-far(path) + distance(cur, Y)
        // add nodes (priority queue)
      end if;
    end;
    path;
  end;
end function findpath;
