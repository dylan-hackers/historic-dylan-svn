module: client


define class <dumber-bot> (<robot-agent>)
end class <dumber-bot>;

// Bot by Alex and Bruce. We use Keith's visualiser (on CVS) to track our robot's
// progress on the server with OpenGL visualiser. :-P

define method generate-next-move(me :: <dumber-bot>, s :: <state>)
 => command :: <command>;

  // Find the closest base.
  let robot = agent-robot(me, s);
  let myPosition = robot.location;
  
  local find-near-base(best-base :: false-or(<point>), distance :: <path-cost>)
         => (better-base :: false-or(<point>), distance :: <path-cost>);
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

  let direction  // GGR: use points-to-direction TODO
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

  make(<move>, bid: 1, direction: direction, id: agent-robot(me, s));
end method generate-next-move;
