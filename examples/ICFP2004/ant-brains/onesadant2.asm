search:
Move . search_blocked:
Sense Here search: . Home
PickUp turn_and_return: .
Sense Here patrol: search: FoeHome

search_blocked:
Turn Right search:

turn_and_return:
Turn Right .
Turn Right .
Turn Right return:

return:
Move . return_blocked:
Sense Here . return: Home
Drop turn_and_search:

return_blocked:
Turn Left return:

turn_and_search:
Turn Right .
Turn Right .
Turn Right search:

patrol:
PickUp return: .
Sense Ahead . turn_and_patrol: FoeHome
Move patrol: turn_and_patrol:

turn_and_patrol:
Turn Right patrol: