module: assembler


// Attackers attack other ant hills.
define sub brain alex-attacker-sb (return)

  Flip 2, (alex-attacker, alex-attacker);

  [alex-attacker:]
    Sense FoeHome, (attacker-success, a-search-foe-home);


  // Steal food.
  [attacker-success:]
    Sense Food, (a-steal-from-under);
    Sense Ahead FoeHome, (a-steal-from-in-front);
    Turn Right, (attacker-success);

  [attacker-success-right:]
    Turn Right, (attacker-success);

  [a-steal-from-in-front:]
//    Sense LeftAhead FoeHome, (a-steal-from-in-front-right);
    Move attacker-success => attacker-success-right;

  [a-steal-from-in-front-right:]
//    Sense RightAhead FoeHome, (attacker-success-right);
    Move attacker-success => attacker-success-right;


  // From under is easy.
  [a-steal-from-under:]
    PickUp a-steal-do-from-under => attacker-success;

  [a-steal-do-from-under:]
    Turn Left;
    Sense Ahead FoeHome, (a-steal-do-from-under, a-steal-food-out);

  [a-steal-food-out:]
    Move => a-steal-food-out;
    Drop;
    Turn Left;
    Turn Left;
    Turn Left, (a-near-hub-ahead);


  [a-search-foe-home:]
    Sense FoeHome, (attacker-success);
    Sense Ahead FoeHome, (a-near-hub-ahead);
    Sense LeftAhead FoeHome, (a-near-hub-left);
    Sense RightAhead FoeHome, (a-near-hub-right);
    Flip 2, (a-move-forward, a-move-lr);


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
    Flip 4, (a-near-hub-ahead-blocked-right, a-near-hub-ahead-blocked-left);

  [a-near-hub-ahead-blocked-right:]
    Turn Right;
    Sense Friend, (a-near-hub-left);
    Move a-near-hub-ahead-blocked-go-around-right => a-near-hub-ahead-blocked-try-left-instead;

  [a-near-hub-ahead-blocked-left:]
    Turn Left;
    Move a-near-hub-ahead-blocked-go-around-left => a-near-hub-ahead-blocked-try-right-instead;

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
    Flip 2, (a-move-left, a-move-right);

  [a-move-left:]
    Turn Left;
    Move alex-attacker => a-move-left;

  [a-move-right:]
    Turn Right;
    Move alex-attacker => a-move-right;

end brain; // alex-attacker



// Gatherers gather food. Based on the best gatherer we had (by Keith Bauer!).
define sub brain keith-gatherer-sb (return)

  Flip 2, (keith-gatherer, keith-gatherer);

  [keith-gatherer:]
    Flip 2, (rightant-search, leftant-search);


  [rightant-search:]
    Move => rightant-search-blocked;
    Sense Home, (rightant-search);
    PickUp rightant-turn-and-return => rightant-search;
//    Sense FoeHome, (rightant-patrol, rightant-search);

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

//  [rightant-patrol:]
//    PickUp, (rightant-return);
//    Sense Ahead FoeHome => rightant-turn-and-patrol;
//    Move rightant-patrol => rightant-turn-and-patrol;

//  [rightant-turn-and-patrol:]
//    Turn Right, (rightant-patrol);


  [leftant-search:]
    Move => leftant-search-blocked;
    Sense Home, (leftant-search);
    PickUp leftant-turn-and-return => leftant-search;
//    Sense FoeHome, (leftant-patrol, leftant-search);

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

//  [leftant-patrol:]
//    PickUp, (leftant-return);
//    Sense Ahead FoeHome => leftant-turn-and-patrol;
//    Move leftant-patrol => leftant-turn-and-patrol;

//  [leftant-turn-and-patrol:]
//    Turn Left, (leftant-patrol);

end brain; // keith-gatherer


// Main brain.
define brain alex-keith

  Flip 5, (attacker, gatherer);


  [attacker:]
    Sub alex-attacker-sb;


  [gatherer:]
    Sub keith-gatherer-sb;

end brain;


alex-keith().dump-brain;
