Flip 6 kill: defense_get_out_of_home:

defense_get_out_of_home:
Sense Here . defense_out_of_home: Home
Move defense_get_out_of_home: .
Sense Ahead . defense_get_out_of_home_panic: Marker 0
Mark 1 defense_wait_for_opening:

defense_get_out_of_home_panic:
Turn Right defense_get_out_of_home:

defense_wait_for_opening:
Sense Ahead defense_wait_for_opening: . Friend
Unmark 1 defense_get_out_of_home:

defense_out_of_home:
Mark 0 .
Turn Right .
Turn Right .
Turn Right defense:

defense:
Sense Ahead leave_defense: . Marker 1
Sense RightAhead leave_defense: . Marker 1
Sense LeftAhead leave_defense: . Marker 1
Turn Right .
Turn Right .
Turn Right .
Sense Here defense_food_here: . Food
Sense Ahead defense_fetch_food: defense: Food

leave_defense:
Turn Right .
Turn Right .
Turn Right search:

defense_food_here:
PickUp . .
Turn Right . .
Turn Right . .
Turn Right . .
Move . .
Sense Here . return: Home
Drop defense_get_out_of_home:

defense_fetch_food:
Move . .
PickUp . .
Turn Right .
Turn Right .
Turn Right .
Move . .
Move . .
Sense Here . return: Home
Drop defense_get_out_of_home:

Flip 6 kill: search:

search:
Move . search_blocked:
Sense Here defense_get_out_of_home: . Home
Sense Here search_too_close: . Marker 0
PickUp turn_and_return: .
Sense Ahead patrol: search: FoeHome

search_blocked:
Turn Right search:

search_too_close:
Turn Right .
Turn Right .
Move search: search:

turn_and_return:
Turn Right .
Turn Right .
Turn Right return:

return:
Move . return_blocked:
Sense Ahead . return: Marker 0
Sense Ahead . return_direct: Friend
Drop turn_and_search:

return_blocked:
Turn Left return:

return_direct:
Move . .
Move . .
Drop defense_get_out_of_home:

turn_and_search:
Turn Right .
Turn Right .
Turn Right search:

patrol:
PickUp turn_and_return: . Food
Sense RightAhead patrol_turn_right1: patrol_move_right: FoeHome

patrol_turn_right1:
Turn Right patrol_move_right:

patrol_move_right:
Turn Right patrol_move1:

patrol_move1:
Move patrol_move_right_part_2: patrol_move1:

patrol_move_right_part_2:
Sense LeftAhead . patrol_corner: FoeHome
Turn Left patrol:

patrol_corner:
Turn Left .
Turn Left patrol:

kill:
Flip 10 kill_turn: .
Move . kill_turn:
Sense Here kill_loop: kill: FoeHome

kill_turn:
Flip 2 kill_turn_left: kill_turn_right:

kill_turn_left:
Turn Left kill:

kill_turn_right:
Turn Right kill:

kill_loop:
PickUp kill_dump: .
Sense Ahead . kill_turn_and_loop: FoeHome
Move kill_loop: kill_turn_and_loop:

kill_turn_and_loop:
Turn Right kill_loop:

kill_dump:
Turn Right .
Turn Right .
Turn Right kill_exit:

kill_exit:
Sense Here . kill_drop: FoeHome
Move kill_exit: kill_exit:

kill_drop:
Drop .
Turn Right .
Turn Right .
Turn Right kill_move1:

kill_move1:
Move kill_loop: kill_move1:
