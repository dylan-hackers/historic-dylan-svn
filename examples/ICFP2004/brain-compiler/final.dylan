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
    Sub bruce-spiral;
    Flip 1, (search, search);

  [patrol:]
    Sub Patrol;
    Drop, (search, search);

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
;Sense Here 3 75 Home
;Sense Ahead 75 206 Home
;Sense Here 31 5 Marker 2
;Sense Ahead 243 6 Rock
;Sense Ahead 72 16 Marker 1
;Sense Here 13 8 Marker 1
;Sense Here 42 9 Marker 2
;Sense Ahead 243 10 Rock
;Sense Ahead 11 12 Marker 0
;Sense Ahead 16 72 Marker 1
;Sense Ahead 72 16 Marker 1
;Sense Here 20 14 Marker 2
;Sense Ahead 243 15 Rock
;Sense Ahead 72 16 Marker 0
;Move 0 17
;Sense Ahead 16 18 Foe
;Sense Ahead 72 19 Friend
;Sense Ahead 72 16 Rock
;Sense Ahead 21 23 Marker 2
;Sense Ahead 23 22 Marker 0
;Sense Ahead 53 23 Marker 1
;Sense LeftAhead 24 27 Marker 2
;Sense LeftAhead 27 25 Marker 0
;Sense LeftAhead 26 27 Marker 1
;Turn Left 53
;Sense RightAhead 28 57 Marker 2
;Sense RightAhead 57 29 Marker 0
;Sense RightAhead 30 57 Marker 1
;Turn Right 53
;Sense Ahead 32 34 Marker 2
;Sense Ahead 33 34 Marker 0
;Sense Ahead 34 53 Marker 1
;Sense LeftAhead 35 38 Marker 2
;Sense LeftAhead 36 38 Marker 0
;Sense LeftAhead 38 37 Marker 1
;Turn Left 53
;Sense RightAhead 39 57 Marker 2
;Sense RightAhead 40 57 Marker 0
;Sense RightAhead 57 41 Marker 1
;Turn Right 53
;Sense Ahead 43 45 Marker 2
;Sense Ahead 44 45 Marker 0
;Sense Ahead 53 45 Marker 1
;Sense LeftAhead 46 49 Marker 2
;Sense LeftAhead 47 49 Marker 0
;Sense LeftAhead 48 49 Marker 1
;Turn Left 53
;Sense RightAhead 50 57 Marker 2
;Sense RightAhead 51 57 Marker 0
;Sense RightAhead 52 57 Marker 1
;Turn Right 53
;Move 243 54
;Sense Ahead 53 55 FriendWithFood
;Sense Ahead 56 53 Friend
;Turn Left 53
;Sense Ahead 70 58 Food
;Sense LeftAhead 68 59 Food
;Sense RightAhead 69 60 Food
;Unmark 2 61
;Turn Left 62
;Turn Left 63
;Turn Left 64
;Move 243 243
;Sense Ahead 53 66 FriendWithFood
;Sense Ahead 67 53 Friend
;Turn Left 53
;Turn Left 70
;Turn Right 70
;Move 71 243
;PickUp 253 243
;Flip 2 73 74
;Turn Right 0
;Turn Left 0
;Sense LeftAhead 107 76 Marker 0
;Sense LeftAhead 92 77 Marker 1
;Sense Ahead 85 78 Marker 0
;Sense Ahead 82 79 Marker 1
;Sense RightAhead 81 80 Marker 0
;Sense RightAhead 139 138 Marker 1
;Sense RightAhead 141 140 Marker 1
;Sense RightAhead 84 83 Marker 0
;Sense RightAhead 143 142 Marker 1
;Sense RightAhead 145 144 Marker 1
;Sense Ahead 89 86 Marker 1
;Sense RightAhead 88 87 Marker 0
;Sense RightAhead 147 146 Marker 1
;Sense RightAhead 149 148 Marker 1
;Sense RightAhead 91 90 Marker 0
;Sense RightAhead 151 150 Marker 1
;Sense RightAhead 153 152 Marker 1
;Sense Ahead 100 93 Marker 0
;Sense Ahead 97 94 Marker 1
;Sense RightAhead 96 95 Marker 0
;Sense RightAhead 155 154 Marker 1
;Sense RightAhead 157 156 Marker 1
;Sense RightAhead 99 98 Marker 0
;Sense RightAhead 159 158 Marker 1
;Sense RightAhead 161 160 Marker 1
;Sense Ahead 104 101 Marker 1
;Sense RightAhead 103 102 Marker 0
;Sense RightAhead 163 162 Marker 1
;Sense RightAhead 165 164 Marker 1
;Sense RightAhead 106 105 Marker 0
;Sense RightAhead 167 166 Marker 1
;Sense RightAhead 169 168 Marker 1
;Sense LeftAhead 123 108 Marker 1
;Sense Ahead 116 109 Marker 0
;Sense Ahead 113 110 Marker 1
;Sense RightAhead 112 111 Marker 0
;Sense RightAhead 171 170 Marker 1
;Sense RightAhead 173 172 Marker 1
;Sense RightAhead 115 114 Marker 0
;Sense RightAhead 175 174 Marker 1
;Sense RightAhead 177 176 Marker 1
;Sense Ahead 120 117 Marker 1
;Sense RightAhead 119 118 Marker 0
;Sense RightAhead 179 178 Marker 1
;Sense RightAhead 181 180 Marker 1
;Sense RightAhead 122 121 Marker 0
;Sense RightAhead 183 182 Marker 1
;Sense RightAhead 185 184 Marker 1
;Sense Ahead 131 124 Marker 0
;Sense Ahead 128 125 Marker 1
;Sense RightAhead 127 126 Marker 0
;Sense RightAhead 187 186 Marker 1
;Sense RightAhead 189 188 Marker 1
;Sense RightAhead 130 129 Marker 0
;Sense RightAhead 191 190 Marker 1
;Sense RightAhead 193 192 Marker 1
;Sense Ahead 135 132 Marker 1
;Sense RightAhead 134 133 Marker 0
;Sense RightAhead 195 194 Marker 1
;Sense RightAhead 197 196 Marker 1
;Sense RightAhead 137 136 Marker 0
;Sense RightAhead 199 198 Marker 1
;Sense RightAhead 201 200 Marker 1
;Sense Here 72 72 Home
;Sense Here 72 72 Home
;Sense Here 72 72 Home
;Sense Here 72 72 Home
;Sense Here 72 72 Home
;Sense Here 72 204 Home
;Sense Here 72 204 Home
;Sense Here 72 202 Home
;Sense Here 72 72 Home
;Sense Here 72 204 Home
;Sense Here 72 206 Home
;Sense Here 72 206 Home
;Sense Here 72 72 Home
;Sense Here 72 202 Home
;Sense Here 72 206 Home
;Sense Here 72 202 Home
;Sense Here 72 239 Home
;Sense Here 72 16 Home
;Sense Here 72 16 Home
;Sense Here 72 16 Home
;Sense Here 72 204 Home
;Sense Here 206 204 Home
;Sense Here 204 204 Home
;Sense Here 206 204 Home
;Sense Here 72 204 Home
;Sense Here 204 204 Home
;Sense Here 202 206 Home
;Sense Here 202 72 Home
;Sense Here 72 202 Home
;Sense Here 206 202 Home
;Sense Here 202 72 Home
;Sense Here 202 202 Home
;Sense Here 72 239 Home
;Sense Here 72 16 Home
;Sense Here 72 16 Home
;Sense Here 72 16 Home
;Sense Here 72 204 Home
;Sense Here 204 204 Home
;Sense Here 202 204 Home
;Sense Here 202 72 Home
;Sense Here 72 206 Home
;Sense Here 202 206 Home
;Sense Here 202 206 Home
;Sense Here 206 206 Home
;Sense Here 72 206 Home
;Sense Here 202 72 Home
;Sense Here 206 206 Home
;Sense Here 204 202 Home
;Sense Here 72 239 Home
;Sense Here 72 16 Home
;Sense Here 72 16 Home
;Sense Here 72 16 Home
;Sense Here 72 202 Home
;Sense Here 206 204 Home
;Sense Here 202 72 Home
;Sense Here 202 202 Home
;Sense Here 72 206 Home
;Sense Here 202 72 Home
;Sense Here 206 206 Home
;Sense Here 204 206 Home
;Sense Here 72 202 Home
;Sense Here 202 202 Home
;Sense Here 204 202 Home
;Sense Here 204 202 Home
;Turn Left 203
;Mark 1 209
;Turn Left 205
;Mark 0 209
;Turn Left 207
;Mark 0 208
;Mark 1 209
;Sense Ahead 210 patrol FoeHome
;Sense Here 211 212 Food
;PickUp 253 209
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
;Sense LeftAhead 202 72 Rock
;Sense LeftAhead 204 72 Rock
;Sense LeftAhead 206 72 Rock
;Sense RightAhead 202 72 Rock
;Sense RightAhead 204 72 Rock
;Sense RightAhead 206 72 Rock
;Move 240 72
;Turn Left 0
;Move 242 72
;Turn Right 0
;Sense Here 0 244 Marker 2
;Sense Here 246 245 Marker 0
;Sense Here 246 0 Marker 1
;Move 247 252
;Sense Here 243 248 Home
;PickUp 249 243
;Turn Left 250
;Turn Left 251
;Turn Left 253
;Turn Right 243
;Sense Here 279 254 Home
;Sense Here 259 255 Marker 0
;Sense Here 257 256 Marker 1
;Drop 0
;Sense Ahead 258 276 Marker 0
;Sense Ahead 264 276 Marker 1
;Sense Here 262 260 Marker 1
;Sense Ahead 276 261 Marker 0
;Sense Ahead 264 276 Marker 1
;Sense Ahead 263 276 Marker 0
;Sense Ahead 276 264 Marker 1
;Mark 2 265
;Move 253 266
;Sense Ahead 273 267 FriendWithFood
;Sense Ahead 268 253 Friend
;Drop 269
;Turn Left 270
;Turn Left 271
;Turn Left 272
;Move 280 280
;Flip 2 274 275
;Turn Left 264
;Turn Right 264
;Flip 2 277 278
;Turn Left 253
;Turn Right 253
;Mark 2 deliver-food
;Sense Here 285 281 Home
;PickUp 253 282
;Sense LeftAhead 289 283 Food
;Sense Ahead 291 284 Food
;Sense RightAhead 290 285 Food
;Sense Ahead 291 286 Marker 2
;Sense LeftAhead 289 287 Marker 2
;Sense RightAhead 290 288 Marker 2
;Unmark 2 243
;Turn Left 291
;Turn Right 291
;Move 280 291
;Flip 2 293 294
;Turn Left 280
;Turn Right 280
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
    Flip 5, (attacker, protector);
//    Sub bruce-spiral;

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
