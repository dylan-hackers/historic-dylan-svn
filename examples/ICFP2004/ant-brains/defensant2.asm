Flip 30 defense_find_center_of_home: .
Flip 6 kill: search:

defense_find_center_of_home:
Sense Ahead . defense_at_home_edge: Home
Move defense_find_center_of_home: .
Turn Right defense_find_center_of_home:

defense_at_home_edge:
Turn Right .
Sense Ahead defense_find_corner_of_home: . Home
Turn Right defense_find_corner_of_home:

defense_find_corner_of_home:
Sense Ahead . defense_at_corner_of_home: Home
Move defense_find_corner_of_home: .
Turn Right defense_find_center_of_home:

defense_at_corner_of_home:
Turn Right .
Turn Right .
Move . defense_find_center_of_home:
Move . defense_find_center_of_home:
Move . defense_find_center_of_home:
Move . defense_find_center_of_home:
Move . defense_find_center_of_home:
Sense Here defense_find_guard_post: . Marker 1
Mark 1 .
Turn Right .
point1out: Move . point1out:
Mark 0 .
Turn Right .
Turn Right .
Turn Right .
point1in: Move . point1in:
Turn Left .
Turn Left .
point2out: Move . point2out:
Mark 0 .
Turn Right .
Turn Right .
Turn Right .
point2in: Move . point2in:
Turn Left .
Turn Left .
point3out: Move . point3out:
Mark 0 .
Turn Right .
Turn Right .
Turn Right .
point3in: Move . point3in:
Turn Left .
Turn Left .
point4out: Move . point4out:
Mark 0 .
Turn Right .
Turn Right .
Turn Right .
point4in: Move . point4in:
Turn Left .
Turn Left .
point5out: Move . point5out:
Mark 0 .
Turn Right .
Turn Right .
Turn Right .
point5in: Move search: point5in:

defense_find_guard_post:
defense_find_guard_post1: Sense Ahead defense_stand_guard1: . Marker 0
Turn Right .
defense_find_guard_post2: Sense Ahead defense_stand_guard2: . Marker 0
Turn Right .
defense_find_guard_post3: Sense Ahead defense_stand_guard3: . Marker 0
Turn Right .
defense_find_guard_post4: Sense Ahead defense_stand_guard4: . Marker 0
Turn Right .
defense_find_guard_post5: Sense Ahead defense_stand_guard5: . Marker 0
Turn Right .
defense_find_guard_post6: Sense Ahead defense_stand_guard6: . Marker 0

defense_stand_guard1:
Sense Ahead . defense_stand_guard: Friend
Turn Right defense_find_guard_post2:

defense_stand_guard2:
Sense Ahead . defense_stand_guard: Friend
Turn Right defense_find_guard_post3:

defense_stand_guard3:
Sense Ahead . defense_stand_guard: Friend
Turn Right defense_find_guard_post4:

defense_stand_guard4:
Sense Ahead . defense_stand_guard: Friend
Turn Right defense_find_guard_post5:

defense_stand_guard5:
Sense Ahead . defense_stand_guard: Friend
Turn Right defense_find_guard_post6:

defense_stand_guard6:
Sense Ahead search: defense_stand_guard: Friend

defense_stand_guard:
Move . search:
Sense Here defense_guard_post: search: Marker 0

defense_guard_post:
Drop defense_guard_post:

search:
Move . search_blocked:
Sense Here search: . Marker 1
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
Sense Here return_find_center_of_home: return: Home

return_find_center_of_home:
Sense Ahead . return_at_home_edge: Home
Move return_find_center_of_home: .
Turn Right return_find_center_of_home:

return_at_home_edge:
Turn Right .
Sense Ahead return_find_corner_of_home: . Home
Turn Right return_find_corner_of_home:

return_find_corner_of_home:
Sense Ahead . return_at_corner_of_home: Home
Move return_find_corner_of_home: .
Turn Right return_find_center_of_home:

return_at_corner_of_home:
Turn Right .
Turn Right .
Move . return_find_center_of_home:
Move . return_find_center_of_home:
Move . return_find_center_of_home:
Move . return_find_center_of_home:
Move . return_find_center_of_home:
Drop defense_find_guard_post:

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
Move kill_exit: .
Turn Right kill_exit:

kill_drop:
Drop .
Turn Right .
Turn Right .
Turn Right kill_move1:

kill_move1:
Move kill_loop: kill_move1:
