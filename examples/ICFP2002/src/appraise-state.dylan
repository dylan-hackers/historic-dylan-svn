module: client

// Appraises state from the eyes of a particular robot.
define method appraise-state(s :: <state>, robot-id :: <integer>) 
 => (value :: <float>);

  if (member?(robot-id, map(id, s.robots)))
    let robot = find-robot(s, robot-id);
    // Score points count for 1
    let value = robot.score;
    
    // Carried package weight counts for .5/unit
    value := value + reduce(\+, 0, map(weight, robot.inventory)) / 2.0;
    
    // Packages on ground are good for 0.125/unit, or 1.0 per package,
    // if unit is unknown.
    value := value + reduce(method (a, b)
			      if (b)
				a + b / 8.0;
			      else
				a + 1;
			      end if;
			    end method, 0, map(weight, free-packages(s)));
    
    // Each living enemy robot is -100 for us (i.e., 100 pt bonus for
    // wasting bots)
    value := value + (s.robots.size - 1) * -100;  
  else
    // Us being dead is -10000
    -10000;
  end if;
end method appraise-state;
