module: client

define class <dumbot> (<robot-agent>)
end class <dumbot>;


define method generate-next-move(me :: <dumbot>, s :: <state>)
 => (c :: <command>)
  block(return)
    let drop-these = choose(at-destination?, me.robot.inventory);
    if (~ empty?(drop-these))
      return(make(<drop>, package-ids: map(id, drop-these)));
    end if;
    
    let packages-here = packages-at(s, me.robot.location);
    if (~ empty?(packages-here))
      let take-these = make(<vector>);
      let left = me.robot.capacity;
      for (pkg in sort(packages-here))
	if (pkg.weight <= me.robot.capacity
	      & find-path(me.robot.location, pkg.location, s.board))
	  left := left - pkg.weight;
	  take-these = add!(take-these, x);
	end if;
      end for;
      return(make(<pick>, package-ids: map(id, take-these)));
    end if;
/*
   targets := carried package destinations, free packages, unvisited homebases;
   target = closest(targets);

   return(make(<move>, find-path(target)[0]));
 */

  end block;
end method generate-next-move;
