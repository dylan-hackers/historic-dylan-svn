Move . cant_move:
;don't pickup food from home!
Sense Here 0 . Home
PickUp turn_around: 0

;disabled for now
Sense LeftAhead pickup_leftahead: . Food
Sense RightAhead pickup_rightahead: . Food


cant_move:
Sense Ahead turn_right: . Rock
Sense Here turn_right: 0 Home

turn_right:
Turn Right 0

pickup_leftahead:
Turn Left .
Move . 0
PickUp . 0
Turn Right .
Turn Right .
Turn Right .
Move . 0
Turn Right look_for_home:

pickup_rightahead:
Turn Right .
Move . 0
PickUp . 0
Turn Right .
Turn Right .
Turn Right .
Move . 0
Turn Left look_for_home:


turn_around:
Turn Right .
Turn Right .
Turn Right look_for_home:

look_for_home:
Move check_if_home: .
Sense Ahead look_for_home: . FriendWithFood
Sense Ahead drop_and_search_more: . Friend
Sense Ahead . look_for_home: Rock
Turn Left look_for_home:

check_if_home:
Sense Here drop_and_search_more: look_for_home: Home

drop_and_search_more:
Drop turn180_and_search:


turn180_and_search:
Turn Right .
Turn Right .
Turn Right 0
