Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define constant <general-sensory> = <byte>;
define constant $c2-general-sensory-descriptions = #(
    "I've been patted",
    "I've been slapped",
    "I've bumped into a wall",
    "I am near a wall",
    "I am in a vehicle",
    "User has spoken",
    "Creature has spoken",
    "Own kind has spoken",
    "Audible event",
    "Visible event",
    "It is approaching",
    "It is retreating",
    "It is near me",
    "It is active",
    "It is an object",
    "It is a creature",
    "It is my sibling",
    "It is my parent",
    "It is my child",
    "It is opposite sex",
    "It has pushed me",
    "It has hit me",
    "spare4",
    "spare5",
    "spare6",
    "spare7",
    "spare8",
    "spare9",
    "Approaching an edge",
    "Retreating from an edge",
    "Falling through the air",
    "Hitting the ground post fall"
);

define constant $c3-general-sensory-descriptions = #();

define method convert-to-general-sensory-description( version == #"creatures2", type :: <general-sensory> ) => (result :: <string>)
    element($c2-general-sensory-descriptions, 
      type, 
      default: format-to-string("Unknown General Sensory: %=", type));
end method convert-to-general-sensory-description;

define method convert-to-general-sensory-description( version == #"creatures3", type :: <general-sensory> ) => (result :: <string>)
    element($c3-general-sensory-descriptions, 
      type, 
      default: format-to-string("Unknown General Sensory: %=", type));
end method convert-to-general-sensory-description;

