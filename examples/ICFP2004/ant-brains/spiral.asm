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

forage_colour01: 					; move onto 10 or blank
Sense Ahead turn_and_forage: move_and_forage: Marker 1	; if not 00 or 10, turn

forage_colour1x:
Sense Here forage_colour11: forage_colour10: Marker 1

forage_colour10: 					; move onto 11 or blank
Sense Ahead . forage_colour10a: Marker 0		; if not 1x, check for 00
Sense Ahead move_and_forage: turn_and_forage: Marker 1	; if not 11 rotate and retry (which is safe)

forage_colour10a:					; it's 0x
Sense Ahead turn_and_forage: move_and_forage: Marker 1	; 00 -> move, 01 to turn and retry


forage_colour11:					; move onto 01 or blank
Sense Ahead turn_and_forage: move_and_forage: Marker 0	; not 00 or 01, turn

move_and_forage:
Move forage: .
;; bugger -- someone or something there
Sense Ahead move_and_forage: . Foe			; wait for an enemy to move (blocking is good!)
Sense Ahead turn_and_forage: . Friend			; a friend already there ... it'll get marked without us
Sense Ahead turn_and_forage: move_and_forage: Rock	; huh?  if it's none of the above then it moved meantime!

; TODO!! navigate around rock to avoid getting stuck!

turn_and_forage: Turn Left forage: ; maybe the next one isn't blocked

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

try_to_leave_home:



; got food, and on the way home

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
forage_onblank_000001:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_000010:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_000011:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_000100:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_000101:	Sense Here turn_and_forage: forage_onblank_setstate2: Home
forage_onblank_000110:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_000111:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_001000:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_001001:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_001010:	Sense Here turn_and_forage: forage_onblank_setstate3: Home
forage_onblank_001011:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_001100:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_001101:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_001110:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_001111:	Sense Here turn_and_forage: forage_onblank_setstate1: Home
forage_onblank_010000:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_010001:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_010010:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_010011:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_010100:	Sense Here turn_and_forage: forage_onblank_setstate2: Home
forage_onblank_010101:	Sense Here forage_onblank_setstate3: forage_onblank_setstate1: Home
forage_onblank_010110:	Sense Here forage_onblank_setstate2: forage_onblank_setstate2: Home
forage_onblank_010111:	Sense Here forage_onblank_setstate3: forage_onblank_setstate2: Home
forage_onblank_011000:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_011001:	Sense Here forage_onblank_setstate2: forage_onblank_setstate1: Home
forage_onblank_011010:	Sense Here forage_onblank_setstate1: forage_onblank_setstate3: Home
forage_onblank_011011:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_011100:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_011101:	Sense Here forage_onblank_setstate3: forage_onblank_setstate1: Home
forage_onblank_011110:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_011111:	Sense Here forage_onblank_setstate1: forage_onblank_setstate1: Home
forage_onblank_100000:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_100001:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_100010:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_100011:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_100100:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_100101:	Sense Here forage_onblank_setstate2: forage_onblank_setstate2: Home
forage_onblank_100110:	Sense Here forage_onblank_setstate1: forage_onblank_setstate2: Home
forage_onblank_100111:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_101000:	Sense Here turn_and_forage: forage_onblank_setstate3: Home
forage_onblank_101001:	Sense Here forage_onblank_setstate1: forage_onblank_setstate3: Home
forage_onblank_101010:	Sense Here forage_onblank_setstate1: forage_onblank_setstate2: Home
forage_onblank_101011:	Sense Here forage_onblank_setstate3: forage_onblank_setstate3: Home
forage_onblank_101100:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_101101:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_101110:	Sense Here forage_onblank_setstate3: forage_onblank_setstate2: Home
forage_onblank_101111:	Sense Here forage_onblank_setstate2: forage_onblank_setstate1: Home
forage_onblank_110000:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_110001:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_110010:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_110011:	Sense Here turn_and_forage: move_and_forage: Home
forage_onblank_110100:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_110101:	Sense Here forage_onblank_setstate3: forage_onblank_setstate2: Home
forage_onblank_110110:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_110111:	Sense Here forage_onblank_setstate1: forage_onblank_setstate3: Home
forage_onblank_111000:	Sense Here turn_and_forage: turn_and_forage: Home
forage_onblank_111001:	Sense Here forage_onblank_setstate1: turn_and_forage: Home
forage_onblank_111010:	Sense Here forage_onblank_setstate3: forage_onblank_setstate3: Home
forage_onblank_111011:	Sense Here forage_onblank_setstate2: forage_onblank_setstate3: Home
forage_onblank_111100:	Sense Here turn_and_forage: forage_onblank_setstate1: Home
forage_onblank_111101:	Sense Here forage_onblank_setstate1: forage_onblank_setstate1: Home
forage_onblank_111110:	Sense Here forage_onblank_setstate2: forage_onblank_setstate1: Home
forage_onblank_111111:	Sense Here forage_onblank_setstate2: forage_onblank_setstate3: Home

forage_onblank_setstate1:
;Unmark 0 .
Mark 1 forage:

forage_onblank_setstate2:
;Mark 0 .
;Unmark 1 forage:
Mark 0 forage:

forage_onblank_setstate3:
Mark 0 .
Mark 1 forage:
