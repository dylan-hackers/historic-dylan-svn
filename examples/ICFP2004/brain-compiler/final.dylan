module: assembler

define ant-subbrain guards-init(defense-find-guard-post, search)
Sense Ahead 1 3 Home
;Move 0 2
;Turn Right 0
;Turn Right 4
;Sense Ahead 6 5 Home
;Turn Right 6
;Sense Ahead 7 9 Home
;Move 6 8
;Turn Right 0
;Turn Right 10
;Turn Right 11
;Move 12 0
;Move 13 0
;Move 14 0
;Move 15 0
;Move 16 0
;Sense Here defense-find-guard-post 17 Marker 4
;Mark 4 18
;Turn Right 19
;Move 20 19
;Mark 3 21
;Turn Right 22
;Turn Right 23
;Turn Right 24
;Move 25 24
;Turn Left 26
;Turn Left 27
;Move 28 27
;Mark 3 29
;Turn Right 30
;Turn Right 31
;Turn Right 32
;Move 33 32
;Turn Left 34
;Turn Left 35
;Move 36 35
;Mark 3 37
;Turn Right 38
;Turn Right 39
;Turn Right 40
;Move 41 40
;Turn Left 42
;Turn Left 43
;Move 44 43
;Mark 3 45
;Turn Right 46
;Turn Right 47
;Turn Right 48
;Move 49 48
;Turn Left 50
;Turn Left 51
;Move 52 51
;Mark 3 53
;Turn Right 54
;Turn Right 55
;Turn Right 56
;Move search 56
end;

define returning ant-subbrain defense-find-guard-post
Sense Ahead 11 1 Marker 3
;Turn Right 2
;Sense Ahead 13 3 Marker 3
;Turn Right 4
;Sense Ahead 15 5 Marker 3
;Turn Right 6
;Sense Ahead 17 7 Marker 3
;Turn Right 8
;Sense Ahead 19 9 Marker 3
;Turn Right 10
;Sense Ahead 21 11 Marker 3
;Sense Ahead 12 22 Friend
;Turn Right 2
;Sense Ahead 14 22 Friend
;Turn Right 4
;Sense Ahead 16 22 Friend
;Turn Right 6
;Sense Ahead 18 22 Friend
;Turn Right 8
;Sense Ahead 20 22 Friend
;Turn Right 10
;Sense Ahead 25 22 Friend
;Move 23 25
;Sense Here 24 25 Marker 3
;Drop 24
end;

define ant-subbrain deliver-food(defense-find-guard-post, search)
Sense Ahead 1 5 Home
;Move 0 2
;Flip 20 4 3
;Turn Right 0
;Drop search
;Turn Right 6
;Sense Ahead 8 7 Home
;Turn Right 8
;Sense Ahead 9 11 Home
;Move 8 10
;Turn Right 0
;Turn Right 12
;Turn Right 13
;Move 14 0
;Move 15 0
;Move 16 0
;Move 17 0
;Move 18 0
;Drop defense-find-guard-post
;Turn Right 20
;Turn Right 21
;Turn Right search
end;

define returning ant-subbrain patrol
Flip 4 10 1
;PickUp 27 2
;Sense RightAhead 3 4 FoeHome
;Turn Right 4
;Turn Right 5
;Move 6 5
;Sense LeftAhead 7 8 FoeHome
;Turn Left 1
;Turn Left 9
;Turn Left 1
;PickUp 15 11
;Sense Ahead 12 14 FoeHome
;Flip 10 14 13
;Move 10 14
;Turn Right 10
;Flip 6 27 19
;Turn Right 17
;Turn Right 18
;Turn Right 19
;Sense Here 20 22 FoeHome
;Move 19 21
;Turn Right 19
;Drop 23
;Turn Right 24
;Turn Right 25
;Turn Right 26
;Move 10 26
end;

define sub brain simple-search(back)
    Flip 1, (guards-init, guards-init);

  [guards-init:]
    Sub guards-init;

  [search:]
    Move => search-blocked;
    Sense Here (Marker 3), (search);
    Pickup, (turn-and-return);
    Sense Ahead FoeHome, (patrol, search);

  [search-blocked:]
    Turn Right, (search);

  [turn-and-return:]
    Turn Right;
    Turn Right;
    Turn Right, (return);

  [return:]
    Move => return-blocked;
    Sense Here Home, (deliver-food, return);

  [patrol:]
    Sub Patrol;
    Flip 1, (deliver-food, deliver-food);

  [deliver-food:]
    Sub deliver-food;
    Flip 1, (search, search);

  [defense-find-guard-post:]
    Sub defense-find-guard-post;
    Flip 1, (turn-and-return, turn-and-return);    
end;

// Marks 0 and 1 and 2.
define ant-subbrain bruce-spiral
Sense Here 6 1 Marker 0
;Sense Here 4 2 Marker 1
;Sense Here 3 19 Home
;Sense Ahead 19 150 Home
;Sense Ahead 186 5 Rock
;Sense Ahead 16 12 Marker 1
;Sense Ahead 186 7 Rock
;Sense Here 11 8 Marker 1
;Sense Ahead 9 10 Marker 0
;Sense Ahead 12 16 Marker 1
;Sense Ahead 16 12 Marker 1
;Sense Ahead 16 12 Marker 0
;Move 0 13
;Sense Ahead 12 14 Foe
;Sense Ahead 16 15 Friend
;Sense Ahead 16 12 Rock
;Flip 2 17 18
;Turn Right 0
;Turn Left 0
;Sense LeftAhead 51 20 Marker 0
;Sense LeftAhead 36 21 Marker 1
;Sense Ahead 29 22 Marker 0
;Sense Ahead 26 23 Marker 1
;Sense RightAhead 25 24 Marker 0
;Sense RightAhead 83 82 Marker 1
;Sense RightAhead 85 84 Marker 1
;Sense RightAhead 28 27 Marker 0
;Sense RightAhead 87 86 Marker 1
;Sense RightAhead 89 88 Marker 1
;Sense Ahead 33 30 Marker 1
;Sense RightAhead 32 31 Marker 0
;Sense RightAhead 91 90 Marker 1
;Sense RightAhead 93 92 Marker 1
;Sense RightAhead 35 34 Marker 0
;Sense RightAhead 95 94 Marker 1
;Sense RightAhead 97 96 Marker 1
;Sense Ahead 44 37 Marker 0
;Sense Ahead 41 38 Marker 1
;Sense RightAhead 40 39 Marker 0
;Sense RightAhead 99 98 Marker 1
;Sense RightAhead 101 100 Marker 1
;Sense RightAhead 43 42 Marker 0
;Sense RightAhead 103 102 Marker 1
;Sense RightAhead 105 104 Marker 1
;Sense Ahead 48 45 Marker 1
;Sense RightAhead 47 46 Marker 0
;Sense RightAhead 107 106 Marker 1
;Sense RightAhead 109 108 Marker 1
;Sense RightAhead 50 49 Marker 0
;Sense RightAhead 111 110 Marker 1
;Sense RightAhead 113 112 Marker 1
;Sense LeftAhead 67 52 Marker 1
;Sense Ahead 60 53 Marker 0
;Sense Ahead 57 54 Marker 1
;Sense RightAhead 56 55 Marker 0
;Sense RightAhead 115 114 Marker 1
;Sense RightAhead 117 116 Marker 1
;Sense RightAhead 59 58 Marker 0
;Sense RightAhead 119 118 Marker 1
;Sense RightAhead 121 120 Marker 1
;Sense Ahead 64 61 Marker 1
;Sense RightAhead 63 62 Marker 0
;Sense RightAhead 123 122 Marker 1
;Sense RightAhead 125 124 Marker 1
;Sense RightAhead 66 65 Marker 0
;Sense RightAhead 127 126 Marker 1
;Sense RightAhead 129 128 Marker 1
;Sense Ahead 75 68 Marker 0
;Sense Ahead 72 69 Marker 1
;Sense RightAhead 71 70 Marker 0
;Sense RightAhead 131 130 Marker 1
;Sense RightAhead 133 132 Marker 1
;Sense RightAhead 74 73 Marker 0
;Sense RightAhead 135 134 Marker 1
;Sense RightAhead 137 136 Marker 1
;Sense Ahead 79 76 Marker 1
;Sense RightAhead 78 77 Marker 0
;Sense RightAhead 139 138 Marker 1
;Sense RightAhead 141 140 Marker 1
;Sense RightAhead 81 80 Marker 0
;Sense RightAhead 143 142 Marker 1
;Sense RightAhead 145 144 Marker 1
;Sense Here 16 16 Home
;Sense Here 16 16 Home
;Sense Here 16 16 Home
;Sense Here 16 16 Home
;Sense Here 16 16 Home
;Sense Here 16 148 Home
;Sense Here 16 148 Home
;Sense Here 16 146 Home
;Sense Here 16 16 Home
;Sense Here 16 148 Home
;Sense Here 16 150 Home
;Sense Here 16 150 Home
;Sense Here 16 16 Home
;Sense Here 16 146 Home
;Sense Here 16 150 Home
;Sense Here 16 146 Home
;Sense Here 16 182 Home
;Sense Here 16 12 Home
;Sense Here 16 12 Home
;Sense Here 16 12 Home
;Sense Here 16 148 Home
;Sense Here 150 148 Home
;Sense Here 148 148 Home
;Sense Here 150 148 Home
;Sense Here 16 148 Home
;Sense Here 148 148 Home
;Sense Here 146 150 Home
;Sense Here 146 16 Home
;Sense Here 16 146 Home
;Sense Here 150 146 Home
;Sense Here 146 16 Home
;Sense Here 146 146 Home
;Sense Here 16 182 Home
;Sense Here 16 12 Home
;Sense Here 16 12 Home
;Sense Here 16 12 Home
;Sense Here 16 148 Home
;Sense Here 148 148 Home
;Sense Here 146 148 Home
;Sense Here 146 16 Home
;Sense Here 16 150 Home
;Sense Here 146 150 Home
;Sense Here 146 150 Home
;Sense Here 150 150 Home
;Sense Here 16 150 Home
;Sense Here 146 16 Home
;Sense Here 150 150 Home
;Sense Here 148 146 Home
;Sense Here 16 182 Home
;Sense Here 16 12 Home
;Sense Here 16 12 Home
;Sense Here 16 12 Home
;Sense Here 16 146 Home
;Sense Here 150 148 Home
;Sense Here 146 16 Home
;Sense Here 146 146 Home
;Sense Here 16 150 Home
;Sense Here 146 16 Home
;Sense Here 150 150 Home
;Sense Here 148 150 Home
;Sense Here 16 146 Home
;Sense Here 146 146 Home
;Sense Here 148 146 Home
;Sense Here 148 146 Home
;Turn Left 147
;Mark 1 153
;Turn Left 149
;Mark 0 153
;Turn Left 151
;Mark 0 152
;Mark 1 153
;Sense Here 154 155 Food
;PickUp 196 153
;Sense LeftAhead 186 156 Rock
;Sense LeftAhead 158 157 Marker 0
;Sense LeftAhead 158 0 Marker 1
;Sense Ahead 186 159 Rock
;Sense Ahead 161 160 Marker 0
;Sense Ahead 161 0 Marker 1
;Sense RightAhead 186 162 Rock
;Sense RightAhead 164 163 Marker 0
;Sense RightAhead 164 0 Marker 1
;Turn Left 165
;Turn Left 166
;Turn Left 167
;Sense LeftAhead 186 168 Rock
;Sense LeftAhead 170 169 Marker 0
;Sense LeftAhead 170 0 Marker 1
;Sense Ahead 186 171 Rock
;Sense Ahead 173 172 Marker 0
;Sense Ahead 173 0 Marker 1
;Sense RightAhead 186 174 Rock
;Sense RightAhead 186 175 Marker 0
;Sense RightAhead 186 0 Marker 1
;Sense LeftAhead 146 16 Rock
;Sense LeftAhead 148 16 Rock
;Sense LeftAhead 150 16 Rock
;Sense RightAhead 146 16 Rock
;Sense RightAhead 148 16 Rock
;Sense RightAhead 150 16 Rock
;Move 183 16
;Turn Left 0
;Move 185 16
;Turn Right 0
;Sense Here 0 187 Marker 2
;Sense Here 189 188 Marker 0
;Sense Here 189 0 Marker 1
;Move 190 195
;Sense Here 186 191 Home
;PickUp 192 186
;Turn Left 193
;Turn Left 194
;Turn Left 196
;Turn Right 186
;Sense Here 222 197 Home
;Sense Here 203 198 Marker 0
;Sense Here 201 199 Marker 1
;Move 196 200
;Drop 0
;Sense Ahead 202 219 Marker 0
;Sense Ahead 208 219 Marker 1
;Sense Here 206 204 Marker 1
;Sense Ahead 219 205 Marker 0
;Sense Ahead 208 219 Marker 1
;Sense Ahead 207 219 Marker 0
;Sense Ahead 219 208 Marker 1
;Move 196 209
;Sense Ahead 216 210 FriendWithFood
;Sense Ahead 211 196 Friend
;Drop 212
;Turn Left 213
;Turn Left 214
;Turn Left 215
;Move 226 226
;Flip 2 217 218
;Turn Left 208
;Turn Right 208
;Flip 2 220 221
;Turn Left 196
;Turn Right 196
;Drop 223
;Turn Left 224
;Turn Left 225
;Turn Left 186
;Sense Here 231 227 Home
;PickUp 196 228
;Sense LeftAhead 235 229 Food
;Sense Ahead 237 230 Food
;Sense RightAhead 236 231 Food
;Sense Ahead 237 232 Marker 2
;Sense LeftAhead 235 233 Marker 2
;Sense RightAhead 236 234 Marker 2
;Unmark 2 186
;Turn Left 237
;Turn Right 237
;Move 226 237
;Flip 2 239 240
;Turn Left 226
;Turn Right 226
end; // bruce-spiral


define ant-subbrain alex-killer2
Flip 2 1 55
;Flip 2 2 2
;Sense Here 3 21 FoeHome
;Sense Here 4 45 Food
;PickUp 5 3
;Sense Ahead 6 8 FoeHome
;Move 5 7
;Turn Left 5
;Turn Left 9
;Sense Ahead 8 10 FoeHome
;Move 11 10
;Drop 12
;Turn Left 13
;Turn Left 14
;Turn Left 15
;Move 2 16
;Flip 4 17 43
;Turn Right 18
;Sense Here 19 34 Friend
;Turn Left 20
;Move 2 21
;Sense Here 3 22 FoeHome
;Sense Ahead 15 23 FoeHome
;Sense LeftAhead 19 24 FoeHome
;Sense RightAhead 25 27 FoeHome
;Turn Right 26
;Move 2 21
;Flip 4 28 33
;Flip 2 29 31
;Turn Left 30
;Move 2 29
;Turn Right 32
;Move 2 31
;Move 2 28
;Move 35 36
;Turn Left 21
;Turn Left 37
;Turn Left 38
;Move 39 40
;Turn Right 21
;Turn Right 41
;Turn Right 42
;Move 35 36
;Turn Left 44
;Move 39 40
;Flip 3 46 49
;Flip 9 47 48
;Turn Left 3
;Turn Right 3
;Sense Ahead 50 54 FoeHome
;Move 3 51
;Sense Ahead 52 53 Friend
;Turn Left 3
;Flip 3 50 3
;Turn Right 3
;Flip 2 56 56
;Flip 2 57 71
;Move 58 70
;Sense Here 57 59 Home
;PickUp 60 57
;Turn Right 61
;Turn Right 62
;Turn Right 63
;Move 64 69
;Sense Here 65 63 Home
;Drop 66
;Turn Right 67
;Turn Right 68
;Turn Right 57
;Turn Left 63
;Turn Right 57
;Move 72 84
;Sense Here 71 73 Home
;PickUp 74 71
;Turn Left 75
;Turn Left 76
;Turn Left 77
;Move 78 83
;Sense Here 79 77 Home
;Drop 80
;Turn Left 81
;Turn Left 82
;Turn Left 71
;Turn Right 77
;Turn Left 71
end; // alex-killer2


define sub brain alex-killer-original (leave)
    Sense FoeHome, (attacker-success, a-search-foe-home);

  [attacker-success:]
    PickUp attacker-success => attacker-success;

  [a-search-foe-home:]
    Sense Ahead FoeHome, (a-near-hub-ahead);
    Sense LeftAhead FoeHome, (a-near-hub-left);
    Sense RightAhead FoeHome, (a-near-hub-right);
    Flip 2, (a-move-forward, a-move-lr);


  // We are near their hill.
  [a-near-hub-ahead:]
    Move alex-killer-original;
    Sense RightAhead FoeHome, (try-r);
    Sense LeftAhead FoeHome, (try-l, a-near-hub-ahead-blocked);

  [a-near-hub-left:]
    Turn Left;
    Move alex-killer-original => a-search-foe-home;

  [a-near-hub-right:]
    Turn Right;
    Move alex-killer-original => a-search-foe-home;

  [a-near-hub-ahead-blocked:]
    Flip 3, (try-r, try-l);

  [try-l:]
    Turn Left;
    Move a-near-hub-ahead-blocked-go-around-left => a-near-hub-ahead-blocked-try-right-instead;

  [restart:]
    Turn Right;
    Turn Right;
    Move => restart;
    Move => a-search-foe-home;
    Move a-search-foe-home => a-search-foe-home;

  [try-r:]
    Turn Right;
    Move a-near-hub-ahead-blocked-go-around-right => a-near-hub-ahead-blocked-try-left-instead;

  [a-near-hub-ahead-blocked-go-around-right:]
    Turn Left, (a-search-foe-home);

  [a-near-hub-ahead-blocked-try-left-instead:]
    Turn Left;
    Turn Left;
    Move a-near-hub-ahead-blocked-go-around-left => a-near-hub-ahead-blocked-try-right-instead;

  [a-near-hub-ahead-blocked-try-right-instead:]
    Flip 2, (restart);
    Turn Right;
    Turn Right;
    Move a-near-hub-ahead-blocked-go-around-right => a-near-hub-ahead-blocked-try-left-instead;

  [a-near-hub-ahead-blocked-go-around-left:]
    Turn Right, (a-search-foe-home);
    

  // We are still searching.
  [a-move-forward:]
    Move alex-killer-original => a-move-lr;

  [a-move-lr:]
    Flip 2, (a-move-left, a-move-right);

  [a-move-left:]
    Turn Left;
    Move alex-killer-original => a-move-left;

  [a-move-right:]
    Turn Right;
    Move alex-killer-original => a-move-right;

end; // alex-killer-original


define sub brain alex-killer-original-stealing (leave)
  Sense FoeHome, (attacker-success, a-search-foe-home);

//  [attacker-success:]
//    PickUp attacker-success => attacker-success;

  // Steal food.
  [attacker-success:]
    Sense Food, (a-steal-from-under-entry);
    Flip 4, (attacker-success-right);
//    Sense Ahead FoeHome, (a-steal-from-in-front);
    Turn Right, (attacker-success);

  [attacker-success-right:]
    Flip 10, (attacker-success-left);
    Turn Right, (attacker-success);

  [attacker-success-left:]
    Turn Left, (attacker-success);

//  [a-steal-from-in-front:]
//    Sub detect-two-enemies-and-plug;
//    Move attacker-success => attacker-success-right;

  [a-steal-from-in-front:]
    Sense LeftAhead FoeHome, (a-steal-from-in-front-right);
    Move attacker-success => attacker-success-right;

  [a-steal-from-in-front-right:]
    Sense RightAhead FoeHome, (attacker-success-right);
    Move attacker-success => attacker-success-right;


  // From under is easy on the edge.
  [a-steal-from-under-entry:]
    PickUp a-steal-from-under => attacker-success;

  [a-steal-from-under:]
    Sense Ahead FoeHome => a-steal-do-from-under;
//    Sub detect-two-enemies-and-plug;
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
    Sense Ahead FoeHome, (a-near-hub-ahead);
    Sense LeftAhead FoeHome, (a-near-hub-left);
    Sense RightAhead FoeHome, (a-near-hub-right);
    Flip 2, (a-move-forward, a-move-lr);


  // We are near their hill.
  [a-near-hub-ahead:]
    Move alex-killer-original-stealing;
    Sense RightAhead FoeHome, (try-r);
    Sense LeftAhead FoeHome, (try-l, a-near-hub-ahead-blocked);

  [a-near-hub-left:]
    Turn Left;
    Move alex-killer-original-stealing => a-search-foe-home;

  [a-near-hub-right:]
    Turn Right;
    Move alex-killer-original-stealing => a-search-foe-home;

  [a-near-hub-ahead-blocked:]
    Flip 3, (try-r, try-l);

  [try-l:]
    Turn Left;
    Move a-near-hub-ahead-blocked-go-around-left => a-near-hub-ahead-blocked-try-right-instead;

  [restart:]
    Turn Right;
    Turn Right;
    Move => restart;
    Move => a-search-foe-home;
    Move a-search-foe-home => a-search-foe-home;

  [try-r:]
    Turn Right;
    Move a-near-hub-ahead-blocked-go-around-right => a-near-hub-ahead-blocked-try-left-instead;

  [a-near-hub-ahead-blocked-go-around-right:]
    Turn Left, (a-search-foe-home);

  [a-near-hub-ahead-blocked-try-left-instead:]
    Turn Left;
    Turn Left;
    Move a-near-hub-ahead-blocked-go-around-left => a-near-hub-ahead-blocked-try-right-instead;

  [a-near-hub-ahead-blocked-try-right-instead:]
    Flip 2, (restart);
    Turn Right;
    Turn Right;
    Move a-near-hub-ahead-blocked-go-around-right => a-near-hub-ahead-blocked-try-left-instead;

  [a-near-hub-ahead-blocked-go-around-left:]
    Turn Right, (a-search-foe-home);
    

  // We are still searching.
  [a-move-forward:]
    Move alex-killer-original-stealing => a-move-lr;

  [a-move-lr:]
    Flip 2, (a-move-left, a-move-right);

  [a-move-left:]
    Turn Left;
    Move alex-killer-original-stealing => a-move-left;

  [a-move-right:]
    Turn Right;
    Move alex-killer-original-stealing => a-move-right;

end; // alex-killer-original-stealing


// Main brain.
define brain combo
//  Flip 1, (a-original, a-original);

  Flip 7, (protector, gatherer);

  [gatherer:]
    Flip 5, (attacker);
    Sub bruce-spiral;

  [protector:]
    Sub simple-search;
    Flip 1, (protector, protector);

  [attacker:]
    Flip 3, (a-original, a-killer-2);

  [a-original:]
    Sub alex-killer-original-stealing;

  [a-killer-2:]
    Sub alex-killer2;

end; // combo


combo().dump-brain;
