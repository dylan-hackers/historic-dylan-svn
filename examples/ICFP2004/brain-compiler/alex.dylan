module: assembler


// Attackers attack other ant hills.
define sub brain alex-attacker (leave)

  Sense FoeHome, (attacker-success, a-search-foe-home);


  // Steal food.
  [attacker-success:]
    Sense Food, (a-steal-from-under-entry);
    Flip 4, (attacker-success-right);
    Sense Ahead FoeHome, (a-steal-from-in-front);
    Turn Right, (attacker-success);

  [attacker-success-right:]
    Flip 10, (attacker-success-left);
    Turn Right, (attacker-success);

  [attacker-success-left:]
    Turn Left, (attacker-success);


  [a-steal-from-in-front:]
    Move attacker-success;
    Sense Ahead Friend, (a-steal-from-in-front-left-avoid);
    Flip 3, (attacker-success-right, attacker-success-left);

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
//    Sense LeftAhead Rock, (a-move-right);
//    Sense RightAhead Rock, (a-move-left);
//    Sense Ahead Rock, (a-move-lr);
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



// Gatherers gather food. Based on the gatherer 3 we had (by Keith Bauer!).
define sub brain keith-gatherer (leave)

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

end brain; // keith-gatherer


// This one has patrol support.
define sub brain keith-patroller (leave)

  Flip 2, (rightant-search, leftant-search);


  [rightant-search:]
    Move => rightant-search-blocked;
    Sense Home, (rightant-search);
    PickUp, (rightant-turn-and-return);
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
    Sense RightAhead FoeHome, (rightant-patrol-forward-right, rightant-turn-and-patrol);

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
    PickUp rightant-return => rightant-patrol;

  [rightant-turn-and-patrol:]
    Turn Right, (rightant-patrol);


  [leftant-search:]
    Move => leftant-search-blocked;
    Sense Home, (leftant-search);
    PickUp, (leftant-turn-and-return);
    Sense Ahead FoeHome, (leftant-patrol);
    Sense RightAhead FoeHome, (leftant-patrol);
    Sense LeftAhead FoeHome, (leftant-patrol, leftant-search);

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
    Sense Food, (leftant-patrol-pickup);
    Sense Ahead FoeHome, (leftant-turn-and-patrol);
    Sense LeftAhead FoeHome, (leftant-patrol-forward-left);
    Sense RightAhead FoeHome, (leftant-patrol-forward-right, leftant-turn-and-patrol);

  [leftant-patrol-forward-left:]
    Move => leftant-patrol-forward-try-left-left;
    Turn Left, (leftant-patrol);

  [leftant-patrol-forward-right:]
    Move => leftant-patrol-forward-try-right-right;
    Turn Right, (leftant-patrol);

  [leftant-patrol-forward-try-left-left:]
    Turn Left;
    Turn Left, (leftant-patrol-forward-right);

  [leftant-patrol-forward-try-right-right:]
    Turn Right;
    Turn Right, (leftant-patrol-forward-left);


  [leftant-patrol-pickup:]
    PickUp leftant-return => leftant-patrol;

  [leftant-turn-and-patrol:]
    Turn Left, (leftant-patrol);


/*
  [leftant-patrol:]
    PickUp, (leftant-return);
    Sense Ahead FoeHome => leftant-turn-and-patrol;
    Move leftant-patrol => leftant-turn-and-patrol;

  [leftant-turn-and-patrol:]
    Turn Left, (leftant-patrol);
*/

end brain; // keith-patroller


define sub brain keith-gatherer-special-patrol (leave)

  Move => search_blocked;
  Sense Home, (keith-gatherer-special-patrol);
  PickUp, (turn_and_return);
  Sense Ahead FoeHome, (patrol, keith-gatherer-special-patrol);

  [search_blocked:]
    Turn Right, (keith-gatherer-special-patrol);
  
  [turn_and_return:]
    Turn Right;
    Turn Right;
    Turn Right, (return);
  
  [return:]
    Move => return_blocked;
    Sense Home => return;
    Move;
    Move;
    Move;
    Drop, (turn_and_search);
  
  [return_blocked:]
    Turn Left, (return);
  
  [turn_and_search:]
    Turn Right;
    Turn Right;
    Turn Right, (keith-gatherer-special-patrol);
  
  [patrol:]
    PickUp, (turn_and_return);
    Sense RightAhead FoeHome, (patrol_turn_right1, patrol_move_right);
  
  [patrol_turn_right1:]
    Turn Right, (patrol_move_right);
  
  [patrol_move_right:]
    Turn Right, (patrol_move1);
  
  [patrol_move1:]
    Move patrol_move_right_part_2 => patrol_move1;
  
  [patrol_move_right_part_2:]
    Sense LeftAhead FoeHome => patrol_corner;
    Turn Left, (patrol);
  
  [patrol_corner:]
    Turn Left;
    Turn Left, (patrol);
  
  [kill:]
    Flip 10, (kill_turn);
    Move => kill_turn;
    Sense FoeHome, (kill_loop, kill);
  
  [kill_turn:]
    Flip 2, (kill_turn_left, kill_turn_right);
  
  [kill_turn_left:]
    Turn Left, (kill);
  
  [kill_turn_right:]
    Turn Right, (kill);
  
  [kill_loop:]
    PickUp, (kill_dump);
    Sense Ahead FoeHome, (kill_turn_and_loop);
    Move kill_loop => kill_turn_and_loop;
  
  [kill_turn_and_loop:]
    Turn Right, (kill_loop);
  
  [kill_dump:]
    Turn Right;
    Turn Right;
    Turn Right, (kill_exit);
  
  [kill_exit:]
    Sense FoeHome, (kill_drop);
    Move kill_exit => kill_exit;
  
  [kill_drop:]
    Drop;
    Turn Right;
    Turn Right;
    Turn Right, (kill_move1);
  
  [kill_move1:]
    Move kill_loop => kill_move1;

end brain; // keith-gatherer-special-patrol

  
// Main brain.
define brain alex-keith

  Flip 5, (attacker, gatherer);
//  Flip 90, (attacker, gatherer);


  [attacker:]
    Sub alex-attacker;


  [gatherer:]
    Flip 5, (patroller);
    Sub keith-gatherer;

  [patroller:]
    Sub keith-gatherer-special-patrol;
//    Sub keith-patroller;

end brain;


alex-keith().dump-brain;
