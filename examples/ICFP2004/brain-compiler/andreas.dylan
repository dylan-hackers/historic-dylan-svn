module: assembler

define brain andreas

  Flip 1, (get-out, get-out);

  [get-out:]
    Move => turn-and-try-again;
    Sense Home, (get-out, walk-space);

  [turn-and-try-again:]
    Turn Left, (get-out);

  [walk-space:]
    Mark 0;
    Sense Food, (pickup);
    Sense Ahead Food, (move-forward-and-pickup);
    Sense LeftAhead Food, (turn-left-and-pickup);
    Sense RightAhead Food, (turn-right-and-pickup);
    Sense Ahead (Marker 1), (follow-marker-1);

    Flip 1, (sense-left-first, sense-right-first);

  [sense-left-first:]
    Sense LeftAhead (Marker 1), (follow-marker-1-left);
    Sense RightAhead (Marker 1), (follow-marker-1-right, search-on);

  [sense-right-first:]
    Sense RightAhead (Marker 1), (follow-marker-1-right);
    Sense LeftAhead (Marker 1), (follow-marker-1-left, search-on);

  [search-on:]
    Sense Ahead Rock, (turn-left-and-search);
    Move walk-space => get-out-of-the-way;

  [get-out-of-the-way:]
    Turn Right;
    Flip 3, (get-out-of-the-way);
    Move => get-out-of-the-way;
    Mark 0;
    Turn Right;
    Turn Right;
    Turn Right, (walk-space);

  [move-forward-and-pickup:]
    Move pickup => get-out-of-the-way;

  [turn-left-and-pickup:]
    Turn Left, (move-forward-and-pickup);

  [turn-right-and-pickup:]
    Turn Right, (move-forward-and-pickup);

  [turn-left-and-search:]
    Turn Left;
    Move walk-space => turn-left-and-search;
    
  [follow-marker-1-left:]
    Turn Left, (follow-marker-1);
    
  [follow-marker-1-right:]
    Turn Right, (follow-marker-1);

  [follow-marker-1:]
    Move => get-out-of-the-way-marker-1;
    Sense Food, (pickup);
    Sense Ahead Food, (move-forward-and-pickup);
    Sense LeftAhead Food, (turn-left-and-pickup);
    Sense RightAhead Food, (turn-right-and-pickup);
    Sense Ahead (Marker 1), (follow-marker-1);

    Flip 1, (sense-left-first-follow, sense-right-first-follow);

  [sense-left-first-follow:]
    Sense LeftAhead (Marker 1), (follow-marker-1-left);
    Sense RightAhead (Marker 1), (follow-marker-1-right, follow-on);

  [sense-right-first-follow:]
    Sense RightAhead (Marker 1), (follow-marker-1-right);
    Sense LeftAhead (Marker 1), (follow-marker-1-left, follow-on);

  [follow-on:]
    Sense LeftAhead (Marker 1), (follow-marker-1-left);
    Sense RightAhead (Marker 1), (follow-marker-1-right);
    Turn Left, (follow-marker-1);

  [get-out-of-the-way-marker-1:]
    Turn Right;
    Flip 3, (get-out-of-the-way-marker-1);
    Move => get-out-of-the-way-marker-1;
    Mark 1;
    Turn Right;
    Turn Right;
    Turn Right, (follow-marker-1);

  [pickup:]
    Sense Home, (get-out);
    Pickup => walk-space;
    Turn Left;
    Turn Left;
    Turn Left;
    Sense Ahead (Marker 1), (deliver-marker-1);
    Sense LeftAhead (Marker 1), (deliver-marker-1-left);
    Sense RightAhead (Marker 1), (deliver-marker-1-right, deliver-marker-0);

  [deliver-marker-1:]
    Move => get-out-of-the-way-deliver-marker-1;
    Sense Home, (drop);
    Sense Ahead Home, (move-forward-and-drop);
    Sense LeftAhead Home, (turn-left-and-drop);
    Sense RightAhead Home, (turn-right-and-drop);
    Sense Ahead (Marker 1), (deliver-marker-1);

    Flip 1, (deliver-left-first-follow, deliver-right-first-follow);

  [deliver-left-first-follow:]
    Sense LeftAhead (Marker 1), (deliver-marker-1-left);
    Sense RightAhead (Marker 1), (deliver-marker-1-right, deliver-on);

  [deliver-right-first-follow:]
    Sense RightAhead (Marker 1), (deliver-marker-1-right);
    Sense LeftAhead (Marker 1), (deliver-marker-1-left, deliver-on);

  [deliver-on:]
    Turn Left, (deliver-marker-1);

  [get-out-of-the-way-deliver-marker-1:]
    Turn Right;
    Flip 3, (get-out-of-the-way-deliver-marker-1);
    Move => get-out-of-the-way-deliver-marker-1;
    Mark 1;
    Turn Right;
    Turn Right;
    Turn Right, (deliver-marker-1);

  [move-forward-and-drop:]
    Move drop => get-out-of-the-way-deliver-marker-1;

  [turn-left-and-drop:]
    Turn Left, (move-forward-and-drop);
    
  [turn-right-and-drop:]
    Turn Right, (move-forward-and-drop);
    
  [deliver-marker-1-left:]
    Turn Left, (deliver-marker-1);
    
  [deliver-marker-1-right:]
    Turn Right, (deliver-marker-1);
    
  [deliver-marker-0:]
    Move => get-out-of-the-way-deliver-marker-0;
    Mark 1;
    Sense Home, (drop);
    Sense Ahead Home, (move-forward-and-drop);
    Sense LeftAhead Home, (turn-left-and-drop);
    Sense RightAhead Food, (turn-right-and-drop);
    Sense Ahead (Marker 0), (deliver-marker-0);

    Flip 1, (deliver-0-left-first-follow, deliver-0-right-first-follow);

  [deliver-0-left-first-follow:]
    Sense LeftAhead (Marker 0), (deliver-marker-0-left);
    Sense RightAhead (Marker 0), (deliver-marker-0-right, deliver-0-on);

  [deliver-0-right-first-follow:]
    Sense RightAhead (Marker 0), (deliver-marker-0-right);
    Sense LeftAhead (Marker 0), (deliver-marker-0-left, deliver-0-on);

  [deliver-0-on:]
    Turn Right, (deliver-marker-0);

  [get-out-of-the-way-deliver-marker-0:]
    Turn Right;
    Flip 3, (get-out-of-the-way-deliver-marker-0);
    Move => get-out-of-the-way-deliver-marker-0;
    Mark 0;
    Turn Right;
    Turn Right;
    Turn Right, (deliver-marker-0);

  [deliver-marker-0-left:]
    Turn Left, (deliver-marker-0);
    
  [deliver-marker-0-right:]
    Turn Right, (deliver-marker-0);
    

  [drop:]
    Drop;
    Turn Left;  
    Turn Left;  
    Turn Left, (get-out);  
end;

andreas().dump-brain;


    