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
    Flip 25, (guards-init, search);

  [guards-init:]
    Sub guards-init;

  [search:]
    Sub bruce-spiral;
    Flip 1, (search, search);

  [patrol:]
    Flip 3, (a-original, a-killer-2);

  [a-original:]
    Sub alex-killer-original-stealing;

  [a-killer-2:]
    Flip 3 => a-patrol;
    Sub alex-killer2;

  [a-patrol:]
    Sub Patrol;
    Drop, (search);

  [deliver-food:]
    Sub deliver-food;
    Flip 1, (search, search);

  [defense-find-guard-post:]
    Sub defense-find-guard-post;
    Flip 1, (search, search);    
end;

// Marks 0 and 1 and 2.
define ant-subbrain bruce-spiral(patrol, deliver-food)
Sense Here 7 1 Marker 0
;Sense Here 4 2 Marker 1
;Sense Here 3 76 Home
;Sense Ahead 76 207 Home
;Sense Here 32 5 Marker 2
;Sense Ahead 243 6 Rock
;Sense Ahead 73 16 Marker 1
;Sense Here 13 8 Marker 1
;Sense Here 43 9 Marker 2
;Sense Ahead 243 10 Rock
;Sense Ahead 11 12 Marker 0
;Sense Ahead 16 73 Marker 1
;Sense Ahead 73 16 Marker 1
;Sense Here 21 14 Marker 2
;Sense Ahead 243 15 Rock
;Sense Ahead 73 16 Marker 0
;Sense Ahead patrol 17 FoeHome
;Move 0 18
;Sense Ahead 16 19 Foe
;Sense Ahead 73 20 Friend
;Sense Ahead 73 16 Rock
;Sense Ahead 22 24 Marker 2
;Sense Ahead 24 23 Marker 0
;Sense Ahead 54 24 Marker 1
;Sense LeftAhead 25 28 Marker 2
;Sense LeftAhead 28 26 Marker 0
;Sense LeftAhead 27 28 Marker 1
;Turn Left 54
;Sense RightAhead 29 58 Marker 2
;Sense RightAhead 58 30 Marker 0
;Sense RightAhead 31 58 Marker 1
;Turn Right 54
;Sense Ahead 33 35 Marker 2
;Sense Ahead 34 35 Marker 0
;Sense Ahead 35 54 Marker 1
;Sense LeftAhead 36 39 Marker 2
;Sense LeftAhead 37 39 Marker 0
;Sense LeftAhead 39 38 Marker 1
;Turn Left 54
;Sense RightAhead 40 58 Marker 2
;Sense RightAhead 41 58 Marker 0
;Sense RightAhead 58 42 Marker 1
;Turn Right 54
;Sense Ahead 44 46 Marker 2
;Sense Ahead 45 46 Marker 0
;Sense Ahead 54 46 Marker 1
;Sense LeftAhead 47 50 Marker 2
;Sense LeftAhead 48 50 Marker 0
;Sense LeftAhead 49 50 Marker 1
;Turn Left 54
;Sense RightAhead 51 58 Marker 2
;Sense RightAhead 52 58 Marker 0
;Sense RightAhead 53 58 Marker 1
;Turn Right 54
;Move 243 55
;Sense Ahead 54 56 FriendWithFood
;Sense Ahead 57 54 Friend
;Turn Left 54
;Sense Ahead 71 59 Food
;Sense LeftAhead 69 60 Food
;Sense RightAhead 70 61 Food
;Unmark 2 62
;Turn Left 63
;Turn Left 64
;Turn Left 65
;Move 243 243
;Sense Ahead 54 67 FriendWithFood
;Sense Ahead 68 54 Friend
;Turn Left 54
;Turn Left 71
;Turn Right 71
;Move 72 243
;PickUp 255 243
;Flip 2 74 75
;Turn Right 0
;Turn Left 0
;Sense LeftAhead 108 77 Marker 0
;Sense LeftAhead 93 78 Marker 1
;Sense Ahead 86 79 Marker 0
;Sense Ahead 83 80 Marker 1
;Sense RightAhead 82 81 Marker 0
;Sense RightAhead 140 139 Marker 1
;Sense RightAhead 142 141 Marker 1
;Sense RightAhead 85 84 Marker 0
;Sense RightAhead 144 143 Marker 1
;Sense RightAhead 146 145 Marker 1
;Sense Ahead 90 87 Marker 1
;Sense RightAhead 89 88 Marker 0
;Sense RightAhead 148 147 Marker 1
;Sense RightAhead 150 149 Marker 1
;Sense RightAhead 92 91 Marker 0
;Sense RightAhead 152 151 Marker 1
;Sense RightAhead 154 153 Marker 1
;Sense Ahead 101 94 Marker 0
;Sense Ahead 98 95 Marker 1
;Sense RightAhead 97 96 Marker 0
;Sense RightAhead 156 155 Marker 1
;Sense RightAhead 158 157 Marker 1
;Sense RightAhead 100 99 Marker 0
;Sense RightAhead 160 159 Marker 1
;Sense RightAhead 162 161 Marker 1
;Sense Ahead 105 102 Marker 1
;Sense RightAhead 104 103 Marker 0
;Sense RightAhead 164 163 Marker 1
;Sense RightAhead 166 165 Marker 1
;Sense RightAhead 107 106 Marker 0
;Sense RightAhead 168 167 Marker 1
;Sense RightAhead 170 169 Marker 1
;Sense LeftAhead 124 109 Marker 1
;Sense Ahead 117 110 Marker 0
;Sense Ahead 114 111 Marker 1
;Sense RightAhead 113 112 Marker 0
;Sense RightAhead 172 171 Marker 1
;Sense RightAhead 174 173 Marker 1
;Sense RightAhead 116 115 Marker 0
;Sense RightAhead 176 175 Marker 1
;Sense RightAhead 178 177 Marker 1
;Sense Ahead 121 118 Marker 1
;Sense RightAhead 120 119 Marker 0
;Sense RightAhead 180 179 Marker 1
;Sense RightAhead 182 181 Marker 1
;Sense RightAhead 123 122 Marker 0
;Sense RightAhead 184 183 Marker 1
;Sense RightAhead 186 185 Marker 1
;Sense Ahead 132 125 Marker 0
;Sense Ahead 129 126 Marker 1
;Sense RightAhead 128 127 Marker 0
;Sense RightAhead 188 187 Marker 1
;Sense RightAhead 190 189 Marker 1
;Sense RightAhead 131 130 Marker 0
;Sense RightAhead 192 191 Marker 1
;Sense RightAhead 194 193 Marker 1
;Sense Ahead 136 133 Marker 1
;Sense RightAhead 135 134 Marker 0
;Sense RightAhead 196 195 Marker 1
;Sense RightAhead 198 197 Marker 1
;Sense RightAhead 138 137 Marker 0
;Sense RightAhead 200 199 Marker 1
;Sense RightAhead 202 201 Marker 1
;Sense Here 73 73 Home
;Sense Here 73 73 Home
;Sense Here 73 73 Home
;Sense Here 73 73 Home
;Sense Here 73 73 Home
;Sense Here 73 205 Home
;Sense Here 73 205 Home
;Sense Here 73 203 Home
;Sense Here 73 73 Home
;Sense Here 73 205 Home
;Sense Here 73 207 Home
;Sense Here 73 207 Home
;Sense Here 73 73 Home
;Sense Here 73 203 Home
;Sense Here 73 207 Home
;Sense Here 73 203 Home
;Sense Here 73 239 Home
;Sense Here 73 16 Home
;Sense Here 73 16 Home
;Sense Here 73 16 Home
;Sense Here 73 205 Home
;Sense Here 207 205 Home
;Sense Here 205 205 Home
;Sense Here 207 205 Home
;Sense Here 73 205 Home
;Sense Here 205 205 Home
;Sense Here 203 207 Home
;Sense Here 203 73 Home
;Sense Here 73 203 Home
;Sense Here 207 203 Home
;Sense Here 203 73 Home
;Sense Here 203 203 Home
;Sense Here 73 239 Home
;Sense Here 73 16 Home
;Sense Here 73 16 Home
;Sense Here 73 16 Home
;Sense Here 73 205 Home
;Sense Here 205 205 Home
;Sense Here 203 205 Home
;Sense Here 203 73 Home
;Sense Here 73 207 Home
;Sense Here 203 207 Home
;Sense Here 203 207 Home
;Sense Here 207 207 Home
;Sense Here 73 207 Home
;Sense Here 203 73 Home
;Sense Here 207 207 Home
;Sense Here 205 203 Home
;Sense Here 73 239 Home
;Sense Here 73 16 Home
;Sense Here 73 16 Home
;Sense Here 73 16 Home
;Sense Here 73 203 Home
;Sense Here 207 205 Home
;Sense Here 203 73 Home
;Sense Here 203 203 Home
;Sense Here 73 207 Home
;Sense Here 203 73 Home
;Sense Here 207 207 Home
;Sense Here 205 207 Home
;Sense Here 73 203 Home
;Sense Here 203 203 Home
;Sense Here 205 203 Home
;Sense Here 205 203 Home
;Turn Left 204
;Mark 1 210
;Turn Left 206
;Mark 0 210
;Turn Left 208
;Mark 0 209
;Mark 1 210
;Sense Here 211 212 Food
;PickUp 255 210
;Sense LeftAhead 243 213 Rock
;Sense LeftAhead 215 214 Marker 0
;Sense LeftAhead 215 0 Marker 1
;Sense Ahead 243 216 Rock
;Sense Ahead 218 217 Marker 0
;Sense Ahead 218 0 Marker 1
;Sense RightAhead 243 219 Rock
;Sense RightAhead 221 220 Marker 0
;Sense RightAhead 221 0 Marker 1
;Turn Left 222
;Turn Left 223
;Turn Left 224
;Sense LeftAhead 243 225 Rock
;Sense LeftAhead 227 226 Marker 0
;Sense LeftAhead 227 0 Marker 1
;Sense Ahead 243 228 Rock
;Sense Ahead 230 229 Marker 0
;Sense Ahead 230 0 Marker 1
;Sense RightAhead 243 231 Rock
;Sense RightAhead 243 232 Marker 0
;Sense RightAhead 243 0 Marker 1
;Sense LeftAhead 203 73 Rock
;Sense LeftAhead 205 73 Rock
;Sense LeftAhead 207 73 Rock
;Sense RightAhead 203 73 Rock
;Sense RightAhead 205 73 Rock
;Sense RightAhead 207 73 Rock
;Move 240 73
;Turn Left 0
;Move 242 73
;Turn Right 0
;Sense Ahead patrol 244 FoeHome
;Sense Here 0 245 Marker 2
;Sense Here 247 246 Marker 0
;Sense Here 247 0 Marker 1
;Move 248 254
;Sense Ahead patrol 249 FoeHome
;Sense Here 243 250 Home
;PickUp 251 243
;Turn Left 252
;Turn Left 253
;Turn Left 255
;Turn Right 243
;Sense Here 281 256 Home
;Sense Here 261 257 Marker 0
;Sense Here 259 258 Marker 1
;Drop 0
;Sense Ahead 260 278 Marker 0
;Sense Ahead 266 278 Marker 1
;Sense Here 264 262 Marker 1
;Sense Ahead 278 263 Marker 0
;Sense Ahead 266 278 Marker 1
;Sense Ahead 265 278 Marker 0
;Sense Ahead 278 266 Marker 1
;Mark 2 267
;Move 255 268
;Sense Ahead 275 269 FriendWithFood
;Sense Ahead 270 255 Friend
;Drop 271
;Turn Left 272
;Turn Left 273
;Turn Left 274
;Move 282 282
;Flip 2 276 277
;Turn Left 266
;Turn Right 266
;Flip 2 279 280
;Turn Left 255
;Turn Right 255
;Mark 2 deliver-food
;Sense Here 287 283 Home
;PickUp 255 284
;Sense LeftAhead 291 285 Food
;Sense Ahead 293 286 Food
;Sense RightAhead 292 287 Food
;Sense Ahead 293 288 Marker 2
;Sense LeftAhead 291 289 Marker 2
;Sense RightAhead 292 290 Marker 2
;Unmark 2 243
;Turn Left 293
;Turn Right 293
;Sense Ahead patrol 294 FoeHome
;Move 282 293
;Flip 2 296 297
;Turn Left 282
;Turn Right 282
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

    Sub simple-search;
    Flip 1, (protector, protector);

/*
  Flip 7, (protector, gatherer);

  [gatherer:]
    Flip 5, (attacker, protector);

  [protector:]
    Sub simple-search;
    Flip 1, (protector, protector);

  [attacker:]
    Flip 3, (a-original, a-killer-2);

  [a-original:]
    Sub alex-killer-original-stealing;

  [a-killer-2:]
    Sub alex-killer2;
*/
end; // combo


combo().dump-brain;
