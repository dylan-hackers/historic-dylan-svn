Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

/* Define allowable constants for switch on stage. Also
   allow for values not within range. */
define constant <switch-on-stage> = type-union(<byte>, one-of(
  #"embryo",
  #"child", 
  #"adolescent", 
  #"youth", 
  #"adult", 
  #"old",
  #"senile"
));

define table $switch-on-stage-values = 
{
  #"embryo"     => 0,
  #"child"      => 1,
  #"adolescent" => 2,
  #"youth"      => 3,
  #"adult"      => 4,
  #"old"        => 5,
  #"senile"     => 6
};

define table $switch-on-stage-descriptions =
{
  #"embryo"     => "Embryo",
  #"child"      => "Child",
  #"adolescent" => "Adolescent",
  #"youth"      => "Youth",
  #"adult"      => "Adult",
  #"old"        => "Old",
  #"senile"     => "Senile"
};

define method convert-to-switch-on-stage ( value :: <byte> ) => (result :: <switch-on-stage>)
    find-key( $switch-on-stage-values, curry (\= , value ), failure: value );
end method convert-to-switch-on-stage;

define method convert-to-value ( class == <switch-on-stage>, sos :: <switch-on-stage> )
    element( $switch-on-stage-values,
      sos,
      default: sos );
end method convert-to-value;

define method convert-to-description( class == <switch-on-stage>, sos :: <switch-on-stage> ) => (result :: <string>)
    element($switch-on-stage-descriptions, 
      sos, 
      default: format-to-string("Unknown stage: %=", sos));
end method convert-to-description;

