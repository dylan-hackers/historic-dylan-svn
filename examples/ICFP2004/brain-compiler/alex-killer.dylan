module: assembler


define brain alex-attacker
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
    Move alex-attacker => a-near-hub-ahead-blocked;

  [a-near-hub-left:]
    Turn Left;
    Move alex-attacker => a-search-foe-home;

  [a-near-hub-right:]
    Turn Right;
    Move alex-attacker => a-search-foe-home;

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
    Move alex-attacker => a-move-lr;

  [a-move-lr:]
    Flip 1, (a-move-left, a-move-right);

  [a-move-left:]
    Turn Left;
    Move alex-attacker => a-move-left;

  [a-move-right:]
    Turn Right;
    Move alex-attacker => a-move-right;

end;


alex().dump-brain;
