module: client
 

define class <dumbot> (<robot-agent>)
end class <dumbot>;


define class <dumber-bot> (<robot-agent>)
end class <dumber-bot>;


// Bot by Alex and Bruce. We use Keith's visualiser (on CVS) to track our robot's
// progress on the server with OpenGL visualiser. :-P
define method generate-next-move(me :: <dumber-bot>, s :: <state>)
 => <command>;

  // make(<move>, bid: 1, direction: $north);

  // Find the closest base.
  let myPosition = me.robot.location;
  let nextBase :: false-or(<point>) = #f;
  let baseDistance = 0;
  for (base in s.bases)
    let path = find-path(myPosition, base, s.board);

    if (path ~= #f)
      if (nextBase = #f)
        nextBase := base;
        baseDistance := distance-cost(myPosition, base);
      elseif (distance-cost(myPosition, base) < baseDistance)
        nextBase := base;
        baseDistance := distance-cost(myPosition, base);
      end if;
    end if;
  end for;

  let direction = $north;

  if (nextBase = #f)
    debug("Sorry, can't find any accessible bases!\n");
  else
    let path = find-path(myPosition, nextBase, s.board);
    let new-loc = path.head;

    if (new-loc = point(x: myPosition.x, y: myPosition.y + 1))
      direction := $north;
    elseif (new-loc = point(x: myPosition.x + 1, y: myPosition.y))
      direction := $east;
    elseif (new-loc = point(x: myPosition.x, y: myPosition.y - 1))
      direction := $south;
    elseif (new-loc = point(x: myPosition.x - 1, y: myPosition.y))
      direction := $west;
    end if;
  end if;

  make(<move>, bid: 1, direction: direction);
end method generate-next-move;
    

define method generate-next-move(me :: <dumbot>, s :: <state>)
 => (c :: <command>)
  block(return)
    // Deliver what we can:
    let drop-these = choose(at-destination?, me.robot.inventory);
    if (~drop-these.empty?)
      return(make(<drop>, bid: 1, package-ids: map(id, drop-these)));
    end if;
    
    // Pick ups:
    let packages-here = packages-at(s, me.robot.location);
    if (~packages-here.empty?)
      let take-these = make(<vector>);
      let left = me.robot.capacity;
      // Greedy algorithm to get as much as we can:
      for (pkg in sort(packages-here))
	if (pkg.weight <= me.robot.capacity
	      & find-path(me.robot.location, pkg.location, s.board))
	  left := left - pkg.weight;
	  take-these = add!(take-these, x);
	end if;
      end for;
      return(make(<pick>, bid: 1, package-ids: map(id, take-these)));
    end if;
    
    // Go to the next interesting place:
    let targets = concatenate(map(dest, me.robot.inventory),
			      map(location, s.free-packages)/*,
			      map(location, choose(unvisted?,
                                                   s.home-bases) )*/);

    let paths = map(curry(rcurry(find-path, s.board), me.robot,location),
		    targets);

    paths := sort!(paths, stable: #t, 
		  test: method (a :: <list>, b :: <list>) 
			  a.size < b.size;
                        end method);
    
    let new-loc = paths.first.first;
    let direction = $North;

    if (new-loc = point(x: new-loc.x, y: new-loc.y + 1))
      direction = #"north";
    elseif (new-loc = point(x: new-loc.x + 1, y: new-loc.y))
      direction = #"east";
    elseif (new-loc = point(x: new-loc.x, y: new-loc.y - 1))
      direction = #"south";
    elseif (new-loc = point(x: new-loc.x - 1, y: new-loc.y))
      direction = #"west";
    else
      error("Can't happen");
    end if;

    return(make(<move>, bid: 1, direction: direction));
  end block;
end method generate-next-move;
