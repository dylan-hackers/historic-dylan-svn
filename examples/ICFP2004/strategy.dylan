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


// just one ant
define formation solo
  me :: []
end formation;


// a file of ants
define formation file
 /// me :: []
end formation;


// three ants in a row
define formation troika
  left :: [ sense #"RightAhead" => middle ]
  middle :: [ sense #"RightAhead" => right ]
  right :: []
end formation;

/*
left:
   sense RightAhead (next) (lost-middle) Friend ; found middle
   ... other senses
   {tactic}Move (left) (bummer)
   lost-middle:
    ...try to find it 
    turn right
    move (next) (bummer)
    Turn left
    
   bummer:
*/


// a tactic is some minimal coherent
// behaviour that has an end trigger(s)
// and minimal rock avoidance

define tactic forward
  Move;
  exception (rock) => turnleft, forward;
  exception => stop;
end tactic;


// a strategy is a complicated behaviour
// of a formation which is result-oriented


define strategy hunt

end strategy;




// a pattern is some repetition of marks
// that viewed in correct direction gives some
// orientation hint

define pattern way-home

end pattern;