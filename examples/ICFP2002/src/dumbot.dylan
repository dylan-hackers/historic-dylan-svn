module: client

define class <dumbot> (<robot-agent>)
  slot visited-bases :: <list>,
    init-value: #();
end class <dumbot>;

define method unvisited-bases(me :: <dumbot>, s :: <state>)
 => (c :: <collection>);
  choose(method (b :: <point>) 
	   ~member?(b, me.visited-bases, test: \=);
	 end method, s.bases);
end method unvisited-bases;

define method maybe-mark-base-visited(me :: <dumbot>, s :: <state>, p :: <point>)
  if (member?(p, s.bases, test: \=))
    me.visited-bases := add-new!(me.visited-bases, p, test: \=);
  end if;
end method maybe-mark-base-visited;

define method deliverable?(me :: <dumbot>, s :: <state>, p :: <package>, 
			   #key robot = find-robot(s, me.agent-id), 
			        capacity = robot.capacity-left)
  p.weight <= capacity & find-path(robot.location, p.location, s.board);
end method deliverable?;

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
	if (deliverable?(me, s, pkg))
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

    maybe-mark-base-visited(me, s, robot.location);

    format-out("DB: package destinations: %=\n", map(dest, robot.inventory));
    force-output(*standard-output*);

    // Go to the next interesting place:
    let targets = concatenate(map(dest, robot.inventory),
			      choose(curry(curry(deliverable?, me), s),
				     map(location, s.free-packages)),
			      unvisited-bases(me, s));
    
    format-out("DB: Targets: %=\n", targets);
    force-output(*standard-output*);

    let paths = map(curry(rcurry(find-path, s.board), robot.location),
		    targets);

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
