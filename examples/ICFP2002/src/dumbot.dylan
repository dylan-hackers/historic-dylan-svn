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
  
  local find-near-base(best-base :: false-or(<point>), distance :: <integer>)
         => (better-base :: false-or(<point>), distance :: <integer>);
          block (found)
            for (base in s.bases)
              let path = find-path(myPosition, base, s.board);
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


define method generate-next-move(me :: <dumbot>, s :: <state>)
 => (c :: <command>)
  me.robot := find-robot(s, me.robot.id);
  block(return)
    format-out("DB: Considering next move\n");
    force-output(*standard-output*);

    // Deliver what we can:
    let drop-these = choose(at-destination?, me.robot.inventory);
    format-out("DB: drop-these = %=\n", me.robot.inventory);
    force-output(*standard-output*);
    
    if (~drop-these.empty?)
      return(make(<drop>, bid: 1, package-ids: map(id, drop-these)));
    else 
      format-out("DB: Nothing to deliver here.\n");
      force-output(*standard-output*);
    end if;
    
    // Pick ups:
    format-out("DB: All packages: %=\n", s.packages);
    force-output(*standard-output*);
    let packages-here = packages-at(s, me.robot.location);
    format-out("DB: Packages here: %=\n", packages-here);
    force-output(*standard-output*);
    if (packages-here ~= #f & ~packages-here.empty?)
      let take-these = make(<vector>);
      let left = me.robot.capacity;
      // Greedy algorithm to get as much as we can:
      for (pkg in sort(packages-here))
	if (pkg.weight <= me.robot.capacity
	      & find-path(me.robot.location, pkg.location, s.board))
	  left := left - pkg.weight;
	  take-these := add!(take-these, x);
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
    
    // Go to the next interesting place:
    let targets = concatenate(map(dest, me.robot.inventory),
			      map(location, s.free-packages),
			      s.bases);
    
    format-out("DB: Targets: %=\n", targets);
    force-output(*standard-output*);

    let paths = map(curry(rcurry(find-path, s.board), me.robot.location),
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
	  debug("Sorry, can't find anywhere to go!\n");
	  $north
	else
	  let path = paths.first;
	  let new-loc = path.first;
	  
	  case
	    new-loc = point(x: me.robot.location.x, y: me.robot.location.y + 1)
	      => $north;
	    new-loc = point(x: me.robot.location.x + 1, y: me.robot.location.y)
	      => $east;
	    new-loc = point(x: me.robot.location.x, y: me.robot.location.y - 1)
	      => $south;
	    new-loc = point(x: me.robot.location.x - 1, y: me.robot.location.y)
	      => $west;
	    new-loc = point(x: me.robot.location.x, y: me.robot.location.y)
	      => error("Can't happen");
	  end case;
	end if;
    return(make(<move>, bid: 1, direction: direction));
  end block;
end method generate-next-move;
