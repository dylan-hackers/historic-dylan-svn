module: assembler


define brain alex-gatherer

  [start:]
    Drop, (get-out-of-home);


  // The following set of macros makes the ant get out of home.
  // After that it moves on to search-food.
  [get-out-of-home:]
    Sense Home, (move-forward-in-home, search-food);

  [move-forward-in-home:]
    Move get-out-of-home => turn-left-or-right-in-home;

  [turn-left-or-right-in-home:]
    Flip 2, (turn-left-in-home, turn-right-in-home);

  [turn-left-in-home:]
    Turn Left, (move-forward-in-home);

  [turn-right-in-home:]
    Turn Right, (move-forward-in-home);


  // The following set of macros makes the ant search for food,
  // while avoiding home cells.
  [search-food:]
    Sense Home, (get-out-of-home, search-food-out-of-home);

  [search-food-out-of-home:]
    Sense Food, (pick-up-food, search-food-in-empty);

  [pick-up-food:]
    PickUp deliver-food => search-food-in-empty;

  [search-food-in-empty:]
    Sense Ahead Food, (move-forward-in-empty, try-left-or-right);

  [try-left-or-right:]
    Flip 2, (try-left, try-right);

  [try-left:]
    Sense LeftAhead Food, (move-left-in-empty, try-forward);

  [try-right:]
    Sense RightAhead Food, (move-right-in-empty, try-forward);

  [try-forward:]
    Move search-food => try-else;

  [try-else:]
    Flip 2, (move-left-or-right-twice-in-empty, move-left-or-right-or-forward-in-empty);

  [move-left-or-right-twice-in-empty:]
    Flip 2, (move-left-twice-in-empty, move-right-twice-in-empty);

  [move-left-twice-in-empty:]
    Turn Left;
    Turn Left;
    Move search-food => move-at-random-in-empty;

  [move-right-twice-in-empty:]
    Turn Right;
    Turn Right;
    Move search-food => move-at-random-in-empty;

  [move-left-or-right-or-forward-in-empty:]
    Flip 3, (move-forward-in-empty, move-left-or-right-in-empty);

  [move-forward-in-empty:]
    Move search-food => move-at-random-in-empty;

  [move-left-or-right-in-empty:]
    Flip 2, (move-left-in-empty, move-right-in-empty);

  [move-left-in-empty:]
    Turn Left;
    Move search-food => move-at-random-in-empty;

  [move-right-in-empty:]
    Turn Right;
    Move search-food => move-at-random-in-empty;

  [move-at-random-in-empty:]
    Flip 6, (move-back, move-other-five);

  [move-back:]
    Turn Left;
    Turn Left;
    Turn Left;
    Move search-food => move-at-random-in-empty;

  [move-other-five:]
    Flip 5, (try-forward, try-else);


  // The following set of macros makes it possible to deliver food,
  // using the trail of marks left using marker 0.
  [deliver-food:]
    Turn Right, (deliver-food);

end;


alex-gatherer().dump-brain;
