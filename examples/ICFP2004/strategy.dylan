module: strategy

/*
Forming
   Gathering
   Attacking Dividing Cornering
   Exploring
   Following
*/

// a formation is needed to
// - keep the ants marching coherently
// - react on obstacles/food etc.




// three ants in a row
define formation troika
  left :: [ sense #"RightAhead" => middle ]
  middle :: [ sense #"RightAhead" => right ]
  right :: []
end formation;


// a tactic is some minimal coherent
// behaviour that has an end trigger(s)
// and minimal rock avoidance

define tactic forward

end tactic;


// a strategy is a complicated behaviour
// of a formation which is result-oriented


define strategy hunt

end strategy;

