module: assembler


define brain test-brain1
  [start:]
//         Verbatim { Drop(lookup(start:, 0)) };
         Move => problem;
         Sense LeftAhead (Marker 1) => choose; // no-branching
         Sense (Marker 1) => choose;
         Sense LeftAhead Home => choose;
         Sense Home => choose;


         Sense LeftAhead (Marker 1), (choose); // yes-branching
         Sense (Marker 1), (choose);
         Sense LeftAhead Home, (choose);
         Sense Home, (choose);

         Sense LeftAhead (Marker 1), (turn-left, choose);

  [choose:]
         Flip 3 => turn-right; // no-branching
         Flip 3, (turn-right); // yes-branching
         Flip 9, (turn-left, turn-right);
         Drop, (choose);


  [turn-right:]
         Mark 1;
         Turn Right, (start);
  [turn-left:]
         Turn Left;
         Move start  => problem;

  [problem:]
         Drop;
         Flip 1, (start, start);
end;
 
 
test-brain1().dump-brain;
