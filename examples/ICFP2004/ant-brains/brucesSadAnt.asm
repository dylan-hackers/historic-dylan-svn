Move 1 cant_move:
Sense Here 0 . Home
PickUp turn_around: 0

cant_move:
Turn Right 0

turn_around:
Turn Right .
Turn Right .
Turn Right .

look_for_home:
Move . turn_and_retry:
Sense Here . look_for_home: Home
Drop turn180_and_search:

turn_and_retry:
Turn Left look_for_home:

turn180_and_search:
Turn Right .
Turn Right .
Turn Right 0
