Flip 2 rightant_search: leftant_search:

rightant_search:
Move . rightant_search_blocked:
Sense Here rightant_search: . Home
PickUp rightant_turn_and_return: .
Sense Here rightant_patrol: rightant_search: FoeHome

rightant_search_blocked:
Turn Right rightant_search:

rightant_turn_and_return:
Turn Right .
Turn Right .
Turn Right rightant_return:

rightant_return:
Move . rightant_return_blocked:
Sense Here . rightant_return: Home
Move . .
Move . .
Move . .
Drop rightant_turn_and_search:

rightant_return_blocked:
Turn Left rightant_return:

rightant_turn_and_search:
Turn Right .
Turn Right .
Turn Right rightant_search:

rightant_patrol:
PickUp rightant_return: .
Sense Ahead . rightant_turn_and_patrol: FoeHome
Move rightant_patrol: rightant_turn_and_patrol:

rightant_turn_and_patrol:
Turn Right rightant_patrol:



leftant_search:
Move . leftant_search_blocked:
Sense Here leftant_search: . Home
PickUp leftant_turn_and_return: .
Sense Here leftant_patrol: leftant_search: FoeHome

leftant_search_blocked:
Turn Left leftant_search:

leftant_turn_and_return:
Turn Left .
Turn Left .
Turn Left leftant_return:

leftant_return:
Move . leftant_return_blocked:
Sense Here . leftant_return: Home
Move . .
Move . .
Move . .
Drop leftant_turn_and_search:

leftant_return_blocked:
Turn Right leftant_return:

leftant_turn_and_search:
Turn Left .
Turn Left .
Turn Left leftant_search:

leftant_patrol:
PickUp leftant_return: .
Sense Ahead . leftant_turn_and_patrol: FoeHome
Move leftant_patrol: leftant_turn_and_patrol:

leftant_turn_and_patrol:
Turn Left leftant_patrol: