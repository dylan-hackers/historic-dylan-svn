; smell codings
;
; Bits 0..1:  distance to the nest of 0 if not visited
;
; 00 or 01 -> 10 -> 11 -> 01
;
; ??
; Bit 2: has been explored
; Bit 3: track to food
; Bit 4: track to enemy nest

;exploring the world
;
; Strategy: move outward semi-randomly through marked cells.  Mark new cells.  Return home
; if find food or enemy camp

forage:							;; TODO check for FOOD!
Sense Here forage_colour1x: . Marker 0
Sense Here forage_colour01: . Marker 1			; no existing colour

Sense Here . forage_onblank_: Home			; if home but we can see off home then mark as 3
Sense Ahead forage_onblank_: forage_onblank_setstate3: Home




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; move further out, depending on what colour we're on now

forage_colour01: 						; move onto 10 or blank
Sense Here follow_trail10: . Marker 2
Sense Ahead OSA: . Rock
Sense Ahead turn_and_forage_blank: move_and_forage: Marker 1	; if not 00 or 10, turn

forage_colour1x:
Sense Here forage_colour11: forage_colour10: Marker 1

forage_colour10: 						; move onto 11 or blank
Sense Here follow_trail11: . Marker 2
Sense Ahead OSA: . Rock
Sense Ahead . forage_colour10a: Marker 0			; if not 1x, check for 00
Sense Ahead move_and_forage: turn_and_forage_blank: Marker 1	; if not 11 rotate and retry (which is safe)

forage_colour10a:						; it's 0x
Sense Ahead turn_and_forage_blank: move_and_forage: Marker 1	; 00 -> move, 01 to turn and retry


forage_colour11:						; move onto 01 or blank
Sense Here follow_trail01: . Marker 2
Sense Ahead OSA: . Rock
Sense Ahead turn_and_forage_blank: move_and_forage: Marker 0	; not 00 or 01, turn

move_and_forage:
;Flip 20 . move_and_forage2:				; small probability of turning first to get unstuck
;Turn Right .
;move_and_forage2:
Move forage: .
;; bugger -- someone or something there
Sense Ahead move_and_forage: . Foe			; wait for an enemy to move (blocking is good!)
Sense Ahead turn_and_forage: . Friend			; a friend already there ... it'll get marked without us
Sense Ahead turn_and_forage: move_and_forage: Rock	; huh?  if it's none of the above then it moved meantime!

; TODO!! navigate around rock to avoid getting stuck!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; follow trail to food
; it *must* be easy to follow .. otherwise nuke it

follow_trail01:
Sense Ahead . follow_trail01_try_left: Marker 2
Sense Ahead follow_trail01_try_left: . Marker 0
Sense Ahead follow_trail_move: follow_trail01_try_left: Marker 1

follow_trail01_try_left:
Sense LeftAhead . follow_trail01_try_right: Marker 2
Sense LeftAhead follow_trail01_try_right: . Marker 0
Sense LeftAhead . follow_trail01_try_right: Marker 1
Turn Left follow_trail_move:

follow_trail01_try_right:
Sense RightAhead . nuke_path: Marker 2
Sense RightAhead nuke_path: . Marker 0
Sense RightAhead . nuke_path: Marker 1
Turn Right follow_trail_move:

;;;;;;;;;;

follow_trail10:
Sense Ahead . follow_trail10_try_left: Marker 2
Sense Ahead . follow_trail10_try_left: Marker 0
Sense Ahead follow_trail10_try_left: follow_trail_move: Marker 1

follow_trail10_try_left:
Sense LeftAhead . follow_trail10_try_right: Marker 2
Sense LeftAhead . follow_trail10_try_right: Marker 0
Sense LeftAhead follow_trail10_try_right: . Marker 1
Turn Left follow_trail_move:

follow_trail10_try_right:
Sense RightAhead . nuke_path: Marker 2
Sense RightAhead . nuke_path: Marker 0
Sense RightAhead nuke_path: . Marker 1
Turn Right follow_trail_move:

;;;;;;;;;;

follow_trail11:
Sense Ahead . follow_trail11_try_left: Marker 2
Sense Ahead . follow_trail11_try_left: Marker 0
Sense Ahead follow_trail_move: follow_trail11_try_left: Marker 1

follow_trail11_try_left:
Sense LeftAhead . follow_trail11_try_right: Marker 2
Sense LeftAhead . follow_trail11_try_right: Marker 0
Sense LeftAhead . follow_trail11_try_right: Marker 1
Turn Left follow_trail_move:

follow_trail11_try_right:
Sense RightAhead . nuke_path: Marker 2
Sense RightAhead . nuke_path: Marker 0
Sense RightAhead . nuke_path: Marker 1
Turn Right follow_trail_move:


;;;;;;;;;;;;;;;;

follow_trail_move:	; move onto the cell we just proved is part of the trail
Move OSA: .
Sense Ahead follow_trail_move: . FriendWithFood
Sense Ahead . follow_trail_move: Friend
Turn Left follow_trail_move:

nuke_path:		; lost the path, go home, deleting
Sense Ahead grab_food_ahead: . Food
Sense LeftAhead grab_food_leftahead: . Food
Sense RightAhead grab_food_rightahead: . Food
Unmark 2 .
Turn Left .
Turn Left .
Turn Left .
Move OSA: OSA:
Sense Ahead follow_trail_move: . FriendWithFood
Sense Ahead . follow_trail_move: Friend
Turn Left follow_trail_move:


grab_food_leftahead:
Turn Left grab_food_ahead:

grab_food_rightahead:
Turn Right grab_food_ahead:

grab_food_ahead:
Move . OSA:
PickUp got_food: OSA:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
turn_and_forage:					 ; maybe the next one isn't blocked

turn_and_forage_blank:					 ; maybe the next one isn't blocked
Flip 2 . turn_and_forage2:
Turn Right forage:

turn_and_forage2:
Turn Left forage:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


forage_onblank_:	Sense LeftAhead forage_onblank_1: forage_onblank_0: Marker 0
forage_onblank_0:	Sense LeftAhead forage_onblank_01: forage_onblank_00: Marker 1
forage_onblank_00:	Sense Ahead forage_onblank_001: forage_onblank_000: Marker 0
forage_onblank_000:	Sense Ahead forage_onblank_0001: forage_onblank_0000: Marker 1
forage_onblank_0000:	Sense RightAhead forage_onblank_00001: forage_onblank_00000: Marker 0
forage_onblank_00000:	Sense RightAhead forage_onblank_000001: forage_onblank_000000: Marker 1
forage_onblank_00001:	Sense RightAhead forage_onblank_000011: forage_onblank_000010: Marker 1
forage_onblank_0001:	Sense RightAhead forage_onblank_00011: forage_onblank_00010: Marker 0
forage_onblank_00010:	Sense RightAhead forage_onblank_000101: forage_onblank_000100: Marker 1
forage_onblank_00011:	Sense RightAhead forage_onblank_000111: forage_onblank_000110: Marker 1
forage_onblank_001:	Sense Ahead forage_onblank_0011: forage_onblank_0010: Marker 1
forage_onblank_0010:	Sense RightAhead forage_onblank_00101: forage_onblank_00100: Marker 0
forage_onblank_00100:	Sense RightAhead forage_onblank_001001: forage_onblank_001000: Marker 1
forage_onblank_00101:	Sense RightAhead forage_onblank_001011: forage_onblank_001010: Marker 1
forage_onblank_0011:	Sense RightAhead forage_onblank_00111: forage_onblank_00110: Marker 0
forage_onblank_00110:	Sense RightAhead forage_onblank_001101: forage_onblank_001100: Marker 1
forage_onblank_00111:	Sense RightAhead forage_onblank_001111: forage_onblank_001110: Marker 1
forage_onblank_01:	Sense Ahead forage_onblank_011: forage_onblank_010: Marker 0
forage_onblank_010:	Sense Ahead forage_onblank_0101: forage_onblank_0100: Marker 1
forage_onblank_0100:	Sense RightAhead forage_onblank_01001: forage_onblank_01000: Marker 0
forage_onblank_01000:	Sense RightAhead forage_onblank_010001: forage_onblank_010000: Marker 1
forage_onblank_01001:	Sense RightAhead forage_onblank_010011: forage_onblank_010010: Marker 1
forage_onblank_0101:	Sense RightAhead forage_onblank_01011: forage_onblank_01010: Marker 0
forage_onblank_01010:	Sense RightAhead forage_onblank_010101: forage_onblank_010100: Marker 1
forage_onblank_01011:	Sense RightAhead forage_onblank_010111: forage_onblank_010110: Marker 1
forage_onblank_011:	Sense Ahead forage_onblank_0111: forage_onblank_0110: Marker 1
forage_onblank_0110:	Sense RightAhead forage_onblank_01101: forage_onblank_01100: Marker 0
forage_onblank_01100:	Sense RightAhead forage_onblank_011001: forage_onblank_011000: Marker 1
forage_onblank_01101:	Sense RightAhead forage_onblank_011011: forage_onblank_011010: Marker 1
forage_onblank_0111:	Sense RightAhead forage_onblank_01111: forage_onblank_01110: Marker 0
forage_onblank_01110:	Sense RightAhead forage_onblank_011101: forage_onblank_011100: Marker 1
forage_onblank_01111:	Sense RightAhead forage_onblank_011111: forage_onblank_011110: Marker 1
forage_onblank_1:	Sense LeftAhead forage_onblank_11: forage_onblank_10: Marker 1
forage_onblank_10:	Sense Ahead forage_onblank_101: forage_onblank_100: Marker 0
forage_onblank_100:	Sense Ahead forage_onblank_1001: forage_onblank_1000: Marker 1
forage_onblank_1000:	Sense RightAhead forage_onblank_10001: forage_onblank_10000: Marker 0
forage_onblank_10000:	Sense RightAhead forage_onblank_100001: forage_onblank_100000: Marker 1
forage_onblank_10001:	Sense RightAhead forage_onblank_100011: forage_onblank_100010: Marker 1
forage_onblank_1001:	Sense RightAhead forage_onblank_10011: forage_onblank_10010: Marker 0
forage_onblank_10010:	Sense RightAhead forage_onblank_100101: forage_onblank_100100: Marker 1
forage_onblank_10011:	Sense RightAhead forage_onblank_100111: forage_onblank_100110: Marker 1
forage_onblank_101:	Sense Ahead forage_onblank_1011: forage_onblank_1010: Marker 1
forage_onblank_1010:	Sense RightAhead forage_onblank_10101: forage_onblank_10100: Marker 0
forage_onblank_10100:	Sense RightAhead forage_onblank_101001: forage_onblank_101000: Marker 1
forage_onblank_10101:	Sense RightAhead forage_onblank_101011: forage_onblank_101010: Marker 1
forage_onblank_1011:	Sense RightAhead forage_onblank_10111: forage_onblank_10110: Marker 0
forage_onblank_10110:	Sense RightAhead forage_onblank_101101: forage_onblank_101100: Marker 1
forage_onblank_10111:	Sense RightAhead forage_onblank_101111: forage_onblank_101110: Marker 1
forage_onblank_11:	Sense Ahead forage_onblank_111: forage_onblank_110: Marker 0
forage_onblank_110:	Sense Ahead forage_onblank_1101: forage_onblank_1100: Marker 1
forage_onblank_1100:	Sense RightAhead forage_onblank_11001: forage_onblank_11000: Marker 0
forage_onblank_11000:	Sense RightAhead forage_onblank_110001: forage_onblank_110000: Marker 1
forage_onblank_11001:	Sense RightAhead forage_onblank_110011: forage_onblank_110010: Marker 1
forage_onblank_1101:	Sense RightAhead forage_onblank_11011: forage_onblank_11010: Marker 0
forage_onblank_11010:	Sense RightAhead forage_onblank_110101: forage_onblank_110100: Marker 1
forage_onblank_11011:	Sense RightAhead forage_onblank_110111: forage_onblank_110110: Marker 1
forage_onblank_111:	Sense Ahead forage_onblank_1111: forage_onblank_1110: Marker 1
forage_onblank_1110:	Sense RightAhead forage_onblank_11101: forage_onblank_11100: Marker 0
forage_onblank_11100:	Sense RightAhead forage_onblank_111001: forage_onblank_111000: Marker 1
forage_onblank_11101:	Sense RightAhead forage_onblank_111011: forage_onblank_111010: Marker 1
forage_onblank_1111:	Sense RightAhead forage_onblank_11111: forage_onblank_11110: Marker 0
forage_onblank_11110:	Sense RightAhead forage_onblank_111101: forage_onblank_111100: Marker 1
forage_onblank_11111:	Sense RightAhead forage_onblank_111111: forage_onblank_111110: Marker 1
forage_onblank_000000:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_000001:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_000010:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_000011:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_000100:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_000101:	Sense Here turn_and_forage: forage_onblank_setstate2: Home
forage_onblank_000110:	Sense Here turn_and_forage: forage_onblank_setstate2: Home
forage_onblank_000111:	Sense Here turn_and_forage: forage_onblank_setstate1: Home
forage_onblank_001000:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_001001:	Sense Here turn_and_forage: forage_onblank_setstate2: Home
forage_onblank_001010:	Sense Here turn_and_forage: forage_onblank_setstate3: Home
forage_onblank_001011:	Sense Here turn_and_forage: forage_onblank_setstate3: Home
forage_onblank_001100:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_001101:	Sense Here turn_and_forage: forage_onblank_setstate1: Home
forage_onblank_001110:	Sense Here turn_and_forage: forage_onblank_setstate3: Home
forage_onblank_001111:	Sense Here turn_and_forage: forage_onblank_setstate1: Home
forage_onblank_010000:	Sense Here turn_and_forage: move_turn_left_and_forage: Home
forage_onblank_010001:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_010010:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_010011:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_010100:	Sense Here turn_and_forage: forage_onblank_setstate2: Home
forage_onblank_010101:	Sense Here forage_onblank_setstate3: forage_onblank_setstate2: Home
forage_onblank_010110:	Sense Here forage_onblank_setstate2: forage_onblank_setstate2: Home
forage_onblank_010111:	Sense Here forage_onblank_setstate3: forage_onblank_setstate2: Home
forage_onblank_011000:	Sense Here turn_and_forage: forage_onblank_setstate2: Home
forage_onblank_011001:	Sense Here forage_onblank_setstate2: forage_onblank_setstate2: Home
forage_onblank_011010:	Sense Here forage_onblank_setstate1: forage_onblank_setstate3: Home
forage_onblank_011011:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_011100:	Sense Here turn_and_forage: forage_onblank_setstate1: Home
forage_onblank_011101:	Sense Here forage_onblank_setstate3: forage_onblank_setstate1: Home
forage_onblank_011110:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_011111:	Sense Here forage_onblank_setstate1: forage_onblank_setstate1: Home
forage_onblank_100000:	Sense Here turn_and_forage: move_turn_left_and_forage: Home
forage_onblank_100001:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_100010:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_100011:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_100100:	Sense Here turn_and_forage: forage_onblank_setstate2: Home
forage_onblank_100101:	Sense Here forage_onblank_setstate2: forage_onblank_setstate2: Home
forage_onblank_100110:	Sense Here forage_onblank_setstate1: forage_onblank_setstate2: Home
forage_onblank_100111:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_101000:	Sense Here turn_and_forage: forage_onblank_setstate3: Home
forage_onblank_101001:	Sense Here forage_onblank_setstate1: forage_onblank_setstate3: Home
forage_onblank_101010:	Sense Here forage_onblank_setstate1: forage_onblank_setstate3: Home
forage_onblank_101011:	Sense Here forage_onblank_setstate3: forage_onblank_setstate3: Home
forage_onblank_101100:	Sense Here turn_and_forage: forage_onblank_setstate3: Home
forage_onblank_101101:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_101110:	Sense Here forage_onblank_setstate3: forage_onblank_setstate3: Home
forage_onblank_101111:	Sense Here forage_onblank_setstate2: forage_onblank_setstate1: Home
forage_onblank_110000:	Sense Here turn_and_forage: move_turn_left_and_forage: Home
forage_onblank_110001:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_110010:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_110011:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_110100:	Sense Here turn_and_forage: forage_onblank_setstate1: Home
forage_onblank_110101:	Sense Here forage_onblank_setstate3: forage_onblank_setstate2: Home
forage_onblank_110110:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_110111:	Sense Here forage_onblank_setstate1: forage_onblank_setstate1: Home
forage_onblank_111000:	Sense Here turn_and_forage: forage_onblank_setstate3: Home
forage_onblank_111001:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_111010:	Sense Here forage_onblank_setstate3: forage_onblank_setstate3: Home
forage_onblank_111011:	Sense Here forage_onblank_setstate2: forage_onblank_setstate3: Home
forage_onblank_111100:	Sense Here turn_and_forage: forage_onblank_setstate1: Home
forage_onblank_111101:	Sense Here forage_onblank_setstate1: forage_onblank_setstate1: Home
forage_onblank_111110:	Sense Here forage_onblank_setstate2: forage_onblank_setstate1: Home
forage_onblank_111111:	Sense Here forage_onblank_setstate2: forage_onblank_setstate1: Home

forage_onblank_setstate1:
Turn Left .
;Turn Left .
;Unmark 0 .
Mark 1 just_marked_check_stuck:

forage_onblank_setstate2:
Turn Left .
;Turn Left .
;Mark 0 .
;Unmark 1 forage:
Mark 0 just_marked_check_stuck:

forage_onblank_setstate3:
Turn Left .
;Turn Left .
Mark 0 .
Mark 1 just_marked_check_stuck:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; check that we can see a blank cell somewhere
; NB -- walls aren't blank :-(

just_marked_check_stuck:	; check six directions for blank cell, change mode if none (stuck)

Sense Ahead . patrol: FoeHome
Sense Here . just_marked_check_stuck1: Food		; but first check for Food!
PickUp got_food: just_marked_check_stuck:
;Turn Left .
;Turn Left .
;Turn Left got_food:

just_marked_check_stuck1:
Sense LeftAhead OSA: . Rock
Sense LeftAhead just_marked_check_stuck_ahead: . Marker 0
Sense LeftAhead . forage: Marker 1

just_marked_check_stuck_ahead:
Sense Ahead OSA: . Rock
Sense Ahead just_marked_check_stuck_right: . Marker 0
Sense Ahead . forage: Marker 1

just_marked_check_stuck_right:
Sense RightAhead OSA: . Rock
Sense RightAhead just_marked_check_stuck_turnaround: . Marker 0
Sense RightAhead . forage: Marker 1

just_marked_check_stuck_turnaround:
Turn Left .
Turn Left .
Turn Left .

Sense LeftAhead OSA: . Rock
Sense LeftAhead just_marked_check_stuck_ahead2: . Marker 0
Sense LeftAhead . forage: Marker 1

just_marked_check_stuck_ahead2:
Sense Ahead OSA: . Rock
Sense Ahead just_marked_check_stuck_right2: . Marker 0
Sense Ahead . forage: Marker 1

just_marked_check_stuck_right2:
Sense RightAhead OSA: . Rock
Sense RightAhead OSA: . Marker 0
Sense RightAhead OSA: forage: Marker 1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
forage_onblank_check_rock_left1:
Sense LeftAhead forage_onblank_setstate1: turn_and_forage: Rock

forage_onblank_check_rock_left2:
Sense LeftAhead forage_onblank_setstate2: turn_and_forage: Rock

forage_onblank_check_rock_left3:
Sense LeftAhead forage_onblank_setstate3: turn_and_forage: Rock

forage_onblank_check_rock_right1:
Sense RightAhead forage_onblank_setstate1: turn_and_forage: Rock

forage_onblank_check_rock_right2:
Sense RightAhead forage_onblank_setstate2: turn_and_forage: Rock

forage_onblank_check_rock_right3:
Sense RightAhead forage_onblank_setstate3: turn_and_forage: Rock

move_turn_left_and_forage:
Move . turn_and_forage:
Turn Left forage:

;move_turn_left_and_forage:
move_turn_right_and_forage:
Move . turn_and_forage:
Turn Right forage:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; basically OSA

OSA:
Sense Here forage: . Marker 2
Sense Here OSA_move: . Marker 0
Sense Here OSA_move: forage: Marker 1

OSA_move:
Move . OSA_cant_move:
Sense Here OSA: . Home
PickUp . OSA:
Turn Left .
Turn Left .
Turn Left got_food:

OSA_cant_move:
Turn Right OSA:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; go home with food

got_food:
Sense Here got_food_home: . Home
Sense Here got_food_colour1x: . Marker 0
Sense Here got_food_colour01: . Marker 1			; no existing colour

; got food, but on blank square???  Iconceivable! Just move forward?
;Move got_food: .
Drop forage:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; move home, depending on what colour we're on now

got_food_colour01: 					; move onto 11
Sense Ahead . got_food_turn: Marker 0
Sense Ahead got_food_move: got_food_turn: Marker 1

got_food_colour1x:
Sense Here got_food_colour11: got_food_colour10: Marker 1

got_food_colour10: 					; move onto 01
Sense Ahead got_food_turn: . Marker 0
Sense Ahead got_food_move: got_food_turn: Marker 1


got_food_colour11:					; move onto 10
Sense Ahead . got_food_turn: Marker 0
Sense Ahead got_food_turn: got_food_move: Marker 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

got_food_move:
Mark 2 .
Move got_food: .
;Flip 2 . got_food_blocked_try_right:
;Turn Left got_food_move:

;got_food_blocked_try_right:
;Turn Right got_food_move:

Sense Ahead got_food_widen_trail: . FriendWithFood
Sense Ahead . got_food: Friend
Drop .
Turn Left .
Turn Left .
Turn Left .
Move follow_trail_to_food: follow_trail_to_food:

got_food_widen_trail:
Flip 2 . got_food_widen_trail_turnRight:
Turn Left got_food_move:

got_food_widen_trail_turnRight:
Turn Right got_food_move:

got_food_turn:
Flip 2 . got_food_turnRight:
Turn Left got_food:

got_food_turnRight:
Turn Right got_food:

got_food_home:
Mark 2 deliver_food:	
;Drop .			
;Turn Left .
;Turn Left .
;Turn Left .
;Move forage: OSA: ;follow_trail_to_food:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

follow_trail_to_food:
Sense Here follow_trail_to_food2: . Home
PickUp got_food: .				; check for food all ways in front
Sense LeftAhead follow_trail_to_food_leftahead: . Food
Sense Ahead follow_trail_to_food_move: . Food
Sense RightAhead follow_trail_to_food_rightahead: . Food

follow_trail_to_food2:
Sense Ahead follow_trail_to_food_move: . Marker 2
Sense LeftAhead follow_trail_to_food_leftahead: . Marker 2
Sense RightAhead follow_trail_to_food_rightahead: . Marker 2
Unmark 2 OSA:

; hmmm .. trail ran out .. random more for now...

;Sense Here follow_trail_to_food_colour1x: . Marker 0
;Sense Here follow_trail_to_food_colour01: OSA: Marker 1	; punt if no existing colour
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move away from home, depending on what colour we're on now
;
;follow_trail_to_food_colour01: 					; move onto 10
;Sense Ahead . follow_trail_to_food_turn: Marker 0
;Sense Ahead follow_trail_to_food_turn: follow_trail_to_food_move: Marker 1
;
;follow_trail_to_food_colour1x:
;Sense Here follow_trail_to_food_colour11: follow_trail_to_food_colour10: Marker 1
;
;follow_trail_to_food_colour10: 					; move onto 11
;Sense Ahead . follow_trail_to_food_turn: Marker 0
;Sense Ahead follow_trail_to_food_move: follow_trail_to_food_turn: Marker 1
;
;follow_trail_to_food_colour11:					; move onto 01
;Sense Ahead follow_trail_to_food_turn: . Marker 0
;Sense Ahead follow_trail_to_food_move: follow_trail_to_food_turn: Marker 1
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;

follow_trail_to_food_leftahead:
Turn Left follow_trail_to_food_move:

follow_trail_to_food_rightahead:
Turn Right follow_trail_to_food_move:

follow_trail_to_food_move:
Move follow_trail_to_food: follow_trail_to_food_move:
;Turn Right .
;Move forage: forage:

follow_trail_to_food_turn:
Flip 2 . follow_trail_to_food_turnRight:
Turn Left follow_trail_to_food:

follow_trail_to_food_turnRight:
Turn Right follow_trail_to_food:

patrol:
deliver_food: