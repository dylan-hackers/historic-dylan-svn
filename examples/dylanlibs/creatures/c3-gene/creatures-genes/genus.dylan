Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define constant <genus> = type-union(<byte>, one-of(
  #"norn",
  #"grendel", 
  #"ettin",
  #"shee"
));


define table $genus-values = 
{
  #"norn"    => 0,
  #"grendel" => 1,
  #"ettin"   => 2,
  #"shee"    => 3
};

define table $genus-descriptions =
{
  #"norn"    => "Norn",
  #"grendel" => "Grendel",
  #"ettin"   => "Ettin",
  #"shee"    => "Shee"
};

define method convert-to-genus ( value :: <byte> ) => (result :: <genus>)
    find-key( $genus-values, curry (\= , value ), failure: value );
end method convert-to-genus;

define method convert-to-value ( class == <genus>, type :: <genus> )
    element( $genus-values,
      type,
      default: type );
end method convert-to-value;

define method convert-to-description( class == <genus>, type :: <genus> ) => (result :: <string>)
    element($genus-descriptions, 
      type, 
      default: format-to-string("Unknown genus: %=", type));
end method convert-to-description;

