module: assembler


define brain alex-gatherer

  [start:]
    Mark 0;
    Mark 3;
    Flip 5, (kill-other-hill, start-2);

  [start-2:]
    Flip 15, (mark-around, get-out-of-home);

  [drop-and-get-out-of-home:]
    Turn Right;
    Turn Right;
    Turn Right;
    Drop, (get-out-of-home);

  // The following set of macros makes the ant get out of home.
  // After that it moves on to search-food.
  [get-out-of-home:]
    Sense Home, (move-forward-in-home, search-food);

  [move-forward-in-home:]
    Move get-out-of-home => turn-right-in-home;

//  [turn-left-or-right-in-home:]
//    Flip 1, (turn-left-in-home, turn-right-in-home);

//  [turn-left-in-home:]
//    Turn Left, (move-forward-in-home);

  [turn-right-in-home:]
    Turn Right, (move-forward-in-home);


  // The following set of macros makes the ant search for food,
  // while avoiding home cells.
  [search-food:]
    Sense Home, (get-out-of-home, search-food-out-of-home);

  [search-food-out-of-home:]
    Sense Food, (pick-up-food, search-food-in-empty);

  [pick-up-food:]
    Turn Left;
    Turn Left;
    Turn Left;
    PickUp deliver-food => search-food-in-empty;

  [pick-up-food-no-turn:]
    PickUp deliver-food => search-food-in-empty;

  [search-food-in-empty:]
    Sense Ahead Food, (move-forward-in-empty, try-left-or-right-or-f);

  [try-left-or-right-or-f:]
    Flip 1, (try-forward, try-left-or-right);

  [try-left-or-right:]
    Flip 1, (try-left, try-right);

  [try-left:]
    Sense LeftAhead Food, (move-left-in-empty, try-forward);

  [try-right:]
    Sense RightAhead Food, (move-right-in-empty, try-forward);

  [try-forward:]
    Mark 0;
    Move search-food => try-else;

  [try-else:]
    Flip 1, (move-left-or-right-twice-in-empty, move-left-or-right-or-forward-in-empty);

  [move-left-or-right-twice-in-empty:]
    Flip 1, (move-left-twice-in-empty, move-right-twice-in-empty);

  [move-left-twice-in-empty:]
    Turn Left;
    Turn Left;
    Mark 0;
    Move search-food => move-at-random-in-empty;

  [move-right-twice-in-empty:]
    Turn Right;
    Turn Right;
    Mark 0;
    Move search-food => move-at-random-in-empty;

  [move-left-or-right-or-forward-in-empty:]
    Flip 2, (move-forward-in-empty, move-left-or-right-in-empty);

  [move-forward-in-empty:]
    Mark 0;
    Move search-food => move-at-random-in-empty;

  [move-left-or-right-in-empty:]
    Flip 1, (move-left-in-empty, move-right-in-empty);

  [move-left-in-empty:]
    Turn Left;
    Mark 0;
    Move search-food => move-at-random-in-empty;

  [move-right-in-empty:]
    Turn Right;
    Mark 0;
    Move search-food => move-at-random-in-empty;

  [move-at-random-in-empty:]
    Flip 15, (move-back, move-other-five);

  [move-back:]
    Turn Left;
    Turn Left;
    Turn Left;
    Mark 0;
    Move search-food => move-at-random-in-empty;

  [move-other-five:]
    Flip 2, (try-forward, try-else);


  // The following set of macros makes it possible to deliver food,
  // using the trail of marks left using marker 0.
  [deliver-food:]
    Sense Home, (drop-and-get-out-of-home, deliver-forward);

  [deliver-forward:]
    Sense Ahead (Marker 0), (deliver-move-forward, deliver-try-left-1);

  [deliver-move-forward:]
    // Unmark 0;
    Move deliver-food => deliver-try-other-four;

  [deliver-try-left-1:]
    Sense Ahead (Marker 0), (deliver-move-forward, deliver-try-left-2);

  [deliver-try-left-2:]
    Sense Ahead (Marker 0), (deliver-move-forward, deliver-try-left-3);

  [deliver-try-left-3:]
    Sense Ahead (Marker 0), (deliver-move-forward, deliver-try-left-4);

  [deliver-try-left-4:]
    Sense Ahead (Marker 0), (deliver-move-forward, deliver-try-left-5);

  [deliver-try-left-5:]
    Sense Ahead (Marker 0), (deliver-move-forward, deliver-at-random);

  // We lost our trail. Try randomly or use mark-up of ant hill kind. :-)
  [deliver-at-random:]
    Flip 5, (deliver-move-forward, deliver-try-other-five);

  [deliver-try-other-five:]
    Flip 4, (deliver-move-backward, deliver-try-other-four);

  [deliver-move-backward:]
    Turn Left;
    Turn Left;
    Turn Left, (deliver-move-forward);

  [deliver-try-other-four:]
    Flip 1, (deliver-move-left-or-right, deliver-move-left-or-right-twice);

  [deliver-move-left-or-right:]
    Flip 1, (deliver-move-left, deliver-move-right);

  [deliver-move-left:]
    Turn Left, (deliver-move-forward);

  [deliver-move-right:]
    Turn Right, (deliver-move-forward);   

  [deliver-move-left-or-right-twice:]
    Flip 1, (deliver-move-left-twice, deliver-move-right-twice);

  [deliver-move-left-twice:]
    Turn Left;
    Turn Left, (deliver-move-forward);

  [deliver-move-right-twice:]
    Turn Right;
    Turn Right, (deliver-move-forward);


  // Perform marking algorithm.
  [mark-around:]
    Sense Home, (mark-out-of-home, kill-other-hill);

  [mark-out-of-home:]
    Sense Home, (mark-move-forward-in-home, do-mark-l-1-home);

  [mark-move-forward-in-home:]
    Move mark-out-of-home => mark-turn-right-in-home;

  [mark-turn-right-in-home:]
    Turn Right, (mark-move-forward-in-home);

  [do-mark-l-1-home:]
    Mark 4;
    Move do-mark-l-2-home => kill-other-hill;

  [do-mark-l-2-home:]
    Mark 5;
    Move do-mark-l-3-home => kill-other-hill;

  [do-mark-l-3-home:]
    Mark 3;
    Move do-mark-l-1-home => kill-other-hill;
  

  // Perform agression of another hill.
  [kill-other-hill:]
    Sense FoeHome, (kill-other-hill-check-food, kill-try-forward);

  [kill-other-hill-check-food:]
    Turn Right;
    Sense Ahead Food, (kill-try-to-eat, kill-other-hill-check-food);

  [kill-try-to-eat:]
    Move pick-up-food-no-turn => kill-other-hill-check-food;

  [kill-try-forward:]
    Sense Ahead FoeHome, (kill-move-forward, kill-try-unmarked);

  [kill-move-forward:]
    Mark 1;
    Move kill-other-hill => kill-try-unmarked;

  [kill-try-unmarked:]
    Sense Ahead (Marker 1), (kill-try-other-four, kill-move-forward);

  [kill-try-other-four:]
    Flip 1, (kill-l-or-r, kill-2l-or-2r);

  [kill-l-or-r:]
    Flip 1, (kill-try-left, kill-try-right);

  [kill-try-left:]
    Sense LeftAhead FoeHome, (kill-move-left, kill-try-left-M);

  [kill-try-left-M:]
    Sense LeftAhead (Marker 1), (kill-try-random, kill-move-left);

  [kill-move-left:]
    Turn Left, (kill-move-forward);

  [kill-try-right:]
    Sense RightAhead FoeHome, (kill-move-right, kill-try-right-M);

  [kill-try-right-M:]
    Sense RightAhead (Marker 1), (kill-try-random, kill-move-right);

  [kill-move-right:]
    Turn Right, (kill-move-forward);

  [kill-2l-or-2r:]
    Flip 1, (kill-try-2-left, kill-try-2-right);

  [kill-try-2-left:]
    Turn Left;
    Turn Left, (kill-move-forward);

  [kill-try-2-right:]
    Turn Right;
    Turn Right, (kill-move-forward);

  [kill-try-random:]
    Turn Left;
    Flip 4, (kill-move-forward, kill-try-random);

end;


alex-gatherer().dump-brain;
