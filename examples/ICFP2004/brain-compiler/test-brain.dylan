module: assembler


define brain test-brain1
  start: Move => problem;
         Sense LeftAhead Home, (turn-left, choose)

  choose:
         Flip 9, (turn-left, turn-right);



  turn-right:
         Turn Right, (start);
  turn-left:
         Turn Left;
         Move start  => problem;

  problem:
         Flip 1, (start, start);
 end;
 
 