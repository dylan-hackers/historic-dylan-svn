module: assembler


define brain chris

  [start:]
	Turn Left;
	Turn Right, (random_turn);

  [move:]
	Move => start;
	Sense Here (Marker 0), (spin);
	Sense Ahead (Marker 0), (marker_0_ahead);
	Sense LeftAhead (Marker 0), (marker_0_left_ahead);
	Sense RightAhead (Marker 0), (marker_0_right_ahead);
//	Flip 300, (stake_place, move);
	Sense Here Home => move;
	Flip 50, (stake_place, move);
	
  [marker_0_ahead:]
	Move spin => start;

  [marker_0_left_ahead:]
	Turn Left, (move);

  [marker_0_right_ahead:]
	Turn Right, (move);
	
  [stake_place:]
	Mark 0;
	Turn Right;
	Turn Right;
	Move stake_place_2 => sp1_try1;

  [stake_place_2:]
	Mark 0;
	Turn Right;
	Move stake_place_3 => sp2_try1;

  [stake_place_3:]
	Mark 0;
	Turn Right;
	Move stake_place_4 => sp3_try1;

  [stake_place_4:]
	Mark 0;
	Turn Right;
	Move stake_place_5 => sp4_try1;

  [stake_place_5:]
	Mark 0, (spin);

  [sp1_try1:]
	Sense Ahead Rock, (start);	
	Move stake_place_2 => start;

  [sp2_try1:]
	Sense Ahead Rock, (start);	
	Move stake_place_3 => start;

  [sp3_try1:]
	Sense Ahead Rock, (start);	
	Move stake_place_4 => start;

  [sp4_try1:]
	Sense Ahead Rock, (start);	
	Move stake_place_5 => start;

  [spin:]
	Turn Right, (spin);

  [random_turn:]
	Flip 5, (turn_to_0);
	Flip 5, (turn_to_1);
	Flip 5, (turn_to_2);
	Flip 5, (turn_to_3, turn_to_4);

  [turn_to_0:]
	Turn Right, (move);
	
  [turn_to_1:]
	Turn Right;
	Turn Right, (move);

  [turn_to_2:]
	Turn Right;
	Turn Right;
	Turn Right, (move);

  [turn_to_3:]
	Turn Right;
	Turn Right;
	Turn Right;
	Turn Right, (move);

  [turn_to_4:]
	Turn Right;
	Turn Right;
	Turn Right;
	Turn Right;
	Turn Right, (move);
end;


chris().dump-brain;
