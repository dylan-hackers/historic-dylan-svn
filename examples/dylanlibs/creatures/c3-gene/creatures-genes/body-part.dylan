Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define constant <body-part> = type-union(<byte>, one-of(
  #"head",
  #"body", 
  #"leg",
  #"arm",
  #"tail"
));


define table $body-part-values = 
{
  #"head"   => 0,
  #"body"   => 1,
  #"leg"    => 2,
  #"arm"    => 3,
  #"tail"   => 4
};

define table $body-part-descriptions =
{
  #"head"   => "Head",
  #"body"   => "Body",
  #"leg"    => "Leg",
  #"arm"    => "Arm",
  #"tail"   => "Tail"
};

define method convert-to-body-part ( value :: <byte> ) => (result :: <body-part>)
    find-key( $body-part-values, curry (\= , value ), failure: value );
end method convert-to-body-part;

define method convert-to-value ( class == <body-part>, type :: <body-part> )
    element( $body-part-values,
      type,
      default: type );
end method convert-to-value;

define method convert-to-description( class == <body-part>, type :: <body-part> ) => (result :: <string>)
    element($body-part-descriptions, 
      type, 
      default: format-to-string("Unknown body part: %=", type));
end method convert-to-description;

