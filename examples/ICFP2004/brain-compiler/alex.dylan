module: assembler


define brain alex

  [start:]
    Flip 10, (gatherer, attacker);


  // Gatherers gather food. Based on the best gatherer we had (by Keith Bauer!).
  [gatherer:]
    Flip 2, (rightant-search, leftant-search);


  [rightant-search:]
    Move => rightant-search-blocked;
    Sense Home, (rightant-search);
    PickUp, (rightant-turn-and-return);
    Sense FoeHome, (rightant-patrol, rightant-search);

  [rightant-search-blocked:]
    Turn Right, (rightant-search);

  [rightant-turn-and-return:]
    Turn Right;
    Turn Right;
    Turn Right, (rightant-return);

  [rightant-return:]
    Move => rightant-return-blocked;
    Sense Home => rightant-return;
    Drop, (rightant-turn-and-search);

  [rightant-return-blocked:]
    Turn Left, (rightant-return);

  [rightant-turn-and-search:]
    Turn Right;
    Turn Right;
    Turn Right, (rightant-search);

  [rightant-patrol:]
    PickUp, (rightant-return);
    Sense Ahead FoeHome => rightant-turn-and-patrol;
    Move rightant-patrol => rightant-turn-and-patrol;

  [rightant-turn-and-patrol:]
    Turn Right, (rightant-patrol);


  [leftant-search:]
    Move => leftant-search-blocked;
    Sense Home, (leftant-search);
    PickUp, (leftant-turn-and-return);
    Sense FoeHome, (leftant-patrol, leftant-search);

  [leftant-search-blocked:]
    Turn Left, (leftant-search);

  [leftant-turn-and-return:]
    Turn Left;
    Turn Left;
    Turn Left, (leftant-return);

  [leftant-return:]
    Move => leftant-return-blocked;
    Sense Home => leftant-return;
    Drop, (leftant-turn-and-search);

  [leftant-return-blocked:]
    Turn Right, (leftant-return);

  [leftant-turn-and-search:]
    Turn Left;
    Turn Left;
    Turn Left, (leftant-search);

  [leftant-patrol:]
    PickUp, (leftant-return);
    Sense Ahead FoeHome => leftant-turn-and-patrol;
    Move leftant-patrol => leftant-turn-and-patrol;

  [leftant-turn-and-patrol:]
    Turn Left, (leftant-patrol);


  // Attackers attack other ant hill.
  [attacker:]
    Sense FoeHome, (attacker-success, a-search-foe-home);

  [attacker-success:]
    PickUp attacker-success => attacker-success;

  [a-search-foe-home:]
    Sense Ahead FoeHome, (a-near-hub-ahead);
    Sense LeftAhead FoeHome, (a-near-hub-left);
    Sense RightAhead FoeHome, (a-near-hub-right);
    Flip 1, (a-move-forward, a-move-lr);


  // We are near their hill.
  [a-near-hub-ahead:]
    Move attacker => a-near-hub-ahead-blocked;

  [a-near-hub-left:]
    Turn Left;
    Move attacker => a-search-foe-home;

  [a-near-hub-right:]
    Turn Right;
    Move attacker => a-search-foe-home;

  [a-near-hub-ahead-blocked:]
    Turn Right;
    Move a-near-hub-ahead-blocked-go-around-right => a-near-hub-ahead-blocked-try-left-instead;

  [a-near-hub-ahead-blocked-go-around-right:]
    Turn Left, (a-search-foe-home);

  [a-near-hub-ahead-blocked-try-left-instead:]
    Turn Left;
    Turn Left;
    Move a-near-hub-ahead-blocked-go-around-left => a-near-hub-ahead-blocked-try-right-instead;

  [a-near-hub-ahead-blocked-try-right-instead:]
    Turn Right;
    Turn Right;
    Move a-near-hub-ahead-blocked-go-around-right => a-near-hub-ahead-blocked-try-left-instead;

  [a-near-hub-ahead-blocked-go-around-left:]
    Turn Right, (a-search-foe-home);
    

  // We are still searching.
  [a-move-forward:]
    Move attacker => a-move-lr;

  [a-move-lr:]
    Flip 1, (a-move-left, a-move-right);

  [a-move-left:]
    Turn Left;
    Move attacker => a-move-left;

  [a-move-right:]
    Turn Right;
    Move attacker => a-move-right;

end;


alex().dump-brain;
