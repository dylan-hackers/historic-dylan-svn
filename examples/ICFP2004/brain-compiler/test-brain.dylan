module: assembler

define sub brain test-sub(back)
         Move back => sub-problem;

  [sub-problem:]
         Drop, (back);
end brain;

define returning ant-subbrain test-ant-subbrain
Move 1 1;
Flip 6 2 1
end;

define ant-subbrain test-ant-subbrain2
Flip 6 0 0
end;

/*
define sub brain test-ant-subbrain(back)
  Flip 6, (back, back);
end;
*/

define brain test-brain1
         Sub test-ant-subbrain;
         Sub test-ant-subbrain2;
         Move;
         // Drop, (start);
         Drop, (test-brain1);
/*         
  [start:]
         Set V1 = #t, (fonz => fonzT);
         Drop, (fonzT);
///         Set V1 = #t;
//         IfSet (V1)
//          { Drop, (fonzT); }
//          { Drop, (sub-problem); };

////         Sub test-sub;
         Move fonz  => start;
  [fonz:]
////         Sub test-sub;
///         Pickup, (start);
         Move sub-problem  => start;
  [sub-problem:]
         Move fonz  => start;
 */
 /*        Move => problem;
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
*/
end;
 
 
define constant $test-brain = test-brain1();

$test-brain.dump-brain;

define function dump-cross-reference(brain)
  for (i in brain, n from 0)
    format-out("%d -----> %=\n", n, block () $cross-reference[i] exception (<error>) "Ooops" end);
  end;
end;


$test-brain.dump-cross-reference;
