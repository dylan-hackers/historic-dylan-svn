module: client

define abstract class <robot-agent>(<object>)
  slot agent-id :: <integer>, required-init-keyword: agent-id:;

  // Common utility functions:
  slot visited-bases :: <list>,
    init-value: #();
end class <robot-agent>;

// abstract function:
define method generate-next-move(me :: <robot-agent>, state :: <state>) => (c :: <command>)
end method generate-next-move;

// Unvisited-base stuff: 
define method unvisited-bases(me :: <robot-agent>, s :: <state>)
 => (c :: <collection>);
  choose(method (b :: <point>) 
	   ~member?(b, me.visited-bases, test: \=);
	 end method, s.bases);
end method unvisited-bases;

define method maybe-mark-base-visited(me :: <robot-agent>, s :: <state>, p :: <point>)
  if (member?(p, s.bases, test: \=))
    me.visited-bases := add-new!(me.visited-bases, p, test: \=);
  end if;
end method maybe-mark-base-visited;

// Find direction of adjacent point
define method points-to-direction(src :: <point>, dest :: <point>)
 => (dir :: one-of(#"north", #"south", #"west", #"east", #f))
  let xdiff = src.x - dest.x;
  let ydiff = src.y - dest.y;

  if(xdiff = -1 & ydiff = 0)
    #"east";
  elseif(xdiff = 1 & ydiff = 0)
    #"west";
  elseif(xdiff = 0 & ydiff = -1)
    #"north";
  elseif(xdiff = 0 & ydiff = 1)
    #"south";
  else
    #f;
  end;
end method points-to-direction;

// Finds the closest from provided list of targets.
define method closest-point(s :: <state>, 
			    from :: <point>, 
			    targets :: <collection>, 
			    #key find-path-func = find-path,
			      cutoff = $maximum-integer)
 => (target :: false-or(<point>), path ::  false-or(<point-list>));

  let min-path = #f;

  for (t in targets)
    let path = find-path-func(from, t, s.board, cutoff: cutoff);
    if (path & path.size < cutoff)
      min-path := path;
      cutoff := path.size;
    end if;
  end for;

  if (min-path)
    values(min-path.last, min-path);
  else
    values(#f, #f);
  end if;
end method closest-point;
