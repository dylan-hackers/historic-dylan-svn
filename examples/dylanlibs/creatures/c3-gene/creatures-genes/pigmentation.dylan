Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

/* Define allowable constants for pigments. Also
   allow for values not within range. */
define constant <pigmentation> = type-union(<byte>, one-of(
  #"red",
  #"green", 
  #"blue"
));


define table $pigmentation-values = 
{
  #"red"   => 0,
  #"green" => 1,
  #"blue"  => 2
};

define table $pigmentation-descriptions =
{
  #"red"   => "Red",
  #"green" => "Green",
  #"blue"   => "Blue"
};

define method convert-to-pigmentation ( value :: <byte> ) => (result :: <pigmentation>)
    find-key( $pigmentation-values, curry (\= , value ), failure: value );
end method convert-to-pigmentation;

define method convert-to-value ( class == <pigmentation>, type :: <pigmentation> )
    element( $pigmentation-values,
      type,
      default: type );
end method convert-to-value;

define method convert-to-description( class == <pigmentation>, type :: <pigmentation> ) => (result :: <string>)
    element($pigmentation-descriptions, 
      type, 
      default: format-to-string("Unknown pigmentation: %=", type));
end method convert-to-description;

