Flip 5 defense_get_out_of_home: .
Flip 4 defense_get_out_of_home: .
Flip 4 kill: search:

defense_get_out_of_home:
Sense Here . defense_out_of_home: Home
Move defense_get_out_of_home: .
Turn Right defense_get_out_of_home:

defense_out_of_home:
Mark 0 .
Turn Right .
Turn Right .
Turn Right defense:

defense:
Sense RightAhead defense_turn_right1: defense_move_right: Home

defense_turn_right1:
Turn Right defense_move_right:

defense_move_right:
Turn Right defense_move1:

defense_move1:
Move . defense_move1:
Sense RightAhead defense_take_food: . Food
Sense LeftAhead defense_step_out: defense_move_right_part_2: Friend

defense_take_food:
Turn Right .
take_food_move1: Move . defense_move_right_part_2:
PickUp . .
Turn Right .
Turn Right .
Turn Right .
return_food_move1: Move . return_food_move1:
return_food_move2: Move . return_food_move2:
Drop defense_get_out_of_home:

defense_step_out:
Turn Right .
step_out_move1: Move . defense_move_right_part_2:
step_out_move2: Move . step_out_move2:
Turn Right .
Turn Right .
Turn Right .
step_in_move1: Move . step_in_move1:
step_in_move2: Move . step_in_move2:
Turn Right .
Turn Right defense_move_right_part_2:

defense_move_right_part_2:
Sense LeftAhead . defense_corner: Home
Turn Left defense:

defense_corner:
Turn Left .
Turn Left defense:

Flip 6 kill: search:

search:
Move . search_blocked:
Sense Here search: . Home
PickUp turn_and_return: .
Sense Ahead patrol: search: FoeHome

search_blocked:
Turn Right search:

turn_and_return:
Turn Right .
Turn Right .
Turn Right return:

return:
Move . return_blocked:
Sense Ahead . return: Marker 0
Drop turn_and_search:

return_blocked:
Turn Left return:

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
