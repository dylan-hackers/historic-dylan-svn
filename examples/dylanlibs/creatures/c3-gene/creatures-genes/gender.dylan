Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

/* Define allowable constants for gender. Also
   allow for values not within range. */
define constant <gender> = type-union(<byte>, one-of(
  #"male",
  #"female", 
  #"both"
));

define table $gender-values = 
{
  #"male"   => #b00001000,
  #"female" => #b00010000,
  #"both"   => #b00000000
};

define table $gender-descriptions =
{
  #"male"   => "Male",
  #"female" => "Female",
  #"both"   => "Both"
};

define method convert-to-gender ( value :: <byte> ) => (result :: <gender>)
    // Only check the bits that actually relate to gender (4 and 5)
    let gender-portion = logand(value, #b00011000 );
    find-key( $gender-values, curry (\= , gender-portion ), failure: gender-portion );
end method convert-to-gender;

define method convert-to-value ( class == <gender>, type :: <gender> )
    element( $gender-values,
      type,
      default: type );
end method convert-to-value;

define method convert-to-description( class == <gender>, type :: <gender> ) => (result :: <string>)
    element($gender-descriptions, 
      type, 
      default: format-to-string("Unknown gender: %=", type));
end method convert-to-description;

