module: assembler


// Attackers attack other ant hills.
define sub brain alex-attacker (return)

    Sense FoeHome, (attacker-success, a-search-foe-home);


  // Steal food.
  [attacker-success:]
    Sense Food, (a-steal-from-under-entry);
    Flip 3, (attacker-success-right);
    Sense Ahead FoeHome, (a-steal-from-in-front);
    Turn Right, (attacker-success);

  [attacker-success-right:]
    Flip 9, (attacker-success-left);
    Turn Right, (attacker-success);

  [attacker-success-left:]
    Turn Left, (attacker-success);


  [a-steal-from-in-front:]
    Move attacker-success;
    Sense Ahead Friend, (a-steal-from-in-front-left-avoid);
    Flip 3, (a-steal-from-in-front, attacker-success);

  [a-steal-from-in-front-left-avoid:]
    Turn Left, (attacker-success);


/*
  [a-steal-from-in-front:]
    Sense LeftAhead FoeHome, (a-steal-from-in-front-right);
    Move attacker-success => attacker-success-right;

  [a-steal-from-in-front-right:]
    Sense RightAhead FoeHome, (attacker-success-right);
    Move attacker-success => attacker-success-right;
*/


  // From under is easy on the edge.
  [a-steal-from-under-entry:]
    PickUp a-steal-from-under => attacker-success;

  [a-steal-from-under:]
    Sense Ahead FoeHome => a-steal-do-from-under;
    Move a-steal-from-under => a-steal-from-under-turn;

  [a-steal-from-under-turn:]
    Turn Left, (a-steal-from-under);

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
    Flip 4, (a-move-lr, a-move-forward);


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
define sub brain keith-gatherer (return)

    Flip 2, (rightant-search, leftant-search);


  [rightant-search:]
    Move => rightant-search-blocked;
    Sense Home, (rightant-search);
    PickUp rightant-turn-and-return => rightant-search;
    Sense Ahead FoeHome, (rightant-patrol);
    Sense LeftAhead FoeHome, (rightant-patrol);
    Sense RightAhead FoeHome, (rightant-patrol, rightant-search);

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
    Sense Food, (rightant-patrol-pickup);
    Sense Ahead FoeHome, (rightant-turn-and-patrol);
    Sense LeftAhead FoeHome, (rightant-patrol-forward-left);
    Sense RightAhead FoeHome, (rightant-patrol-forward-right, rightant-patrol-turn-and-patrol);

  [rightant-patrol-forward-left:]
    Move => rightant-patrol-forward-try-left-left;
    Turn Left, (rightant-patrol);

  [rightant-patrol-forward-right:]
    Move => rightant-patrol-forward-try-right-right;
    Turn Right, (rightant-patrol);

  [rightant-patrol-forward-try-left-left:]
    Turn Left;
    Turn Left, (rightant-patrol-forward-right);

  [rightant-patrol-forward-try-right-right:]
    Turn Right;
    Turn Right, (rightant-patrol-forward-left);


  [rightant-patrol-pickup:]
    PickUp, (rightant-return);

  [rightant-turn-and-patrol:]
    Turn Right, (rightant-patrol);


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
    Sub alex-attacker;


  [gatherer:]
    Sub keith-gatherer;

end brain;


alex-keith().dump-brain;
