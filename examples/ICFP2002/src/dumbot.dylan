module: client
 

define class <dumber-bot> (<robot-agent>)
end class <dumber-bot>;

// Bot by Alex and Bruce. We use Keith's visualiser (on CVS) to track our robot's
// progress on the server with OpenGL visualiser. :-P
define method generate-next-move(me :: <dumber-bot>, s :: <state>)
 => <command>;

  // make(<move>, bid: 1, direction: $north);

  // Find the closest base.
  let robot = find-robot(s, me.agent-id);
  let myPosition = robot.location;
  
  local find-near-base(best-base :: false-or(<point>), distance :: <integer>)
         => (better-base :: false-or(<point>), distance :: <integer>);
          block (found)
            for (base in s.bases)
              let path = find-path(myPosition, base, s.board, cutoff: best-base & distance);
              if (path)
                if (~best-base
                    | distance-cost(myPosition, base) < distance)
                  let (better-base, nearer-distance)
                    = find-near-base(base, distance-cost(myPosition, base));
                  found(better-base, nearer-distance)
                end if;
              end if;
            end for;
            values(best-base, distance)
          end block;
        end method;
    
  let (nextBase, baseDistance)
    = find-near-base(#f, 0);

  debug("nextBase = %=, baseDistance %=\n", nextBase, baseDistance);

  let direction
    = if (nextBase = #f)
        debug("Sorry, can't find any accessible bases!\n");
        $north
      else
        let path = find-path(myPosition, nextBase, s.board);
        let new-loc = path.head;

        case
          new-loc = point(x: myPosition.x, y: myPosition.y + 1)
            => $north;
          new-loc = point(x: myPosition.x + 1, y: myPosition.y)
            => $east;
          new-loc = point(x: myPosition.x, y: myPosition.y - 1)
            => $south;
          new-loc = point(x: myPosition.x - 1, y: myPosition.y)
            => $west;
        end case;
      end if;

  make(<move>, bid: 1, direction: direction);
end method generate-next-move;

define class <dumbot> (<robot-agent>)
  slot visited-bases :: <list>,
    init-value: #();
end class <dumbot>;

define method unvisited-bases(me :: <dumbot>, s :: <state>)
  choose(method (b :: <point>)
	   ~any?(curry(\=, b), me.visited-bases);
	 end method, s.bases);
end method unvisited-bases;

define method maybe-mark-base(me :: <dumbot>, s :: <state>, p :: <point>)
  if (any?(curry(\=, p), s.bases) & ~any?(curry(\=, p), me.visited-bases))
    me.visited-bases := add!(me.visited-bases, p);
  end if;
end method maybe-mark-base;

define method generate-next-move(me :: <dumbot>, s :: <state>)
 => (c :: <command>)
  let robot = find-robot(s, me.agent-id);
  block(return)
    format-out("DB: Considering next move (loc: %=)\n", robot.location);
    force-output(*standard-output*);

    // Deliver what we can:
    let drop-these = choose(at-destination?, robot.inventory);
    format-out("DB: drop-these = %=\n", drop-these);
    force-output(*standard-output*);
    
    if (~drop-these.empty?)
      return(make(<drop>, bid: 1, package-ids: map(id, drop-these)));
    else 
      format-out("DB: Nothing to deliver here.\n");
      force-output(*standard-output*);
    end if;
    
    // Pick ups:
    let packages-here = packages-at(s, robot.location, 
				    available-only: #t);
    format-out("DB: Packages here: %=\n", packages-here);
    force-output(*standard-output*);
    if (packages-here ~= #f & ~packages-here.empty?)
      let take-these = make(<vector>);
      let left = robot.capacity;
      // Greedy algorithm to get as much as we can:
      for (pkg in sort(packages-here, 
		       test: method (a :: <package>, b :: <package>)
			       a.weight > b.weight;
			     end method))
	if (pkg.weight <= robot.capacity
	      & find-path(robot.location, pkg.location, s.board))
	  left := left - pkg.weight;
	  take-these := add!(take-these, pkg);
	end if;
      end for;
      if (~take-these.empty?)
	return(make(<pick>, 
		    bid: 1, 
		    package-ids: map(id, take-these)));
      else 
	format-out("DB: Can't pick up or deliver anything from here.\n");
	force-output(*standard-output*);
      end if;
    else 
      format-out("DB: No packages here (or first move)\n");
      force-output(*standard-output*);
    end if;

    maybe-mark-base(me, s, robot.location);

    format-out("DB: package destinations: %=\n", map(dest, robot.inventory));
    force-output(*standard-output*);

    // Go to the next interesting place:
    let targets = concatenate(map(dest, robot.inventory),
			      map(location, s.free-packages),
			      unvisited-bases(me, s));
    
    format-out("DB: Targets: %=\n", targets);
    force-output(*standard-output*);

    let paths = map(curry(rcurry(find-path, s.board), robot.location),
		    targets);

    format-out("DB: Paths: %=\n", paths);
    force-output(*standard-output*);

    paths := choose(curry(\~=, #f), paths);

    paths := sort!(paths, stable: #t, 
		  test: method (a :: <list>, b :: <list>) 
			  a.size < b.size;
                        end method);
    let direction
      = if (empty?(paths))
	  error("Sorry, can't find anywhere to go!\n");
	else
	  let path = paths.first;
	  let new-loc = path.first;
	  
	  case
	    new-loc = point(x: robot.location.x, y: robot.location.y + 1)
	      => $north;
	    new-loc = point(x: robot.location.x + 1, y: robot.location.y)
	      => $east;
	    new-loc = point(x: robot.location.x, y: robot.location.y - 1)
	      => $south;
	    new-loc = point(x: robot.location.x - 1, y: robot.location.y)
	      => $west;
	    new-loc = point(x: robot.location.x, y: robot.location.y)
	      => error("Can't happen");
	  end case;
	end if;
    return(make(<move>, bid: 1, direction: direction));
  end block;
end method generate-next-move;
