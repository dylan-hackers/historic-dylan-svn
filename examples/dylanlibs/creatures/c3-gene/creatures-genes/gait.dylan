Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define constant <gait> = type-union(<byte>, one-of(
    #"normal",
    #"pain",
    #"limping",
    #"fearful",
    #"angry",
    #"sleepy",
    #"tired",
    #"stagger",
    #"uphill",
    #"downhill",
    #"backing",
    #"bored",
    #"panicked",
    #"frisky"
));


define table $gait-values = 
{
    #"normal" => 0,
    #"pain" => 1,
    #"limping" => 2,
    #"fearful" => 3,
    #"angry" => 4,
    #"sleepy" => 5,
    #"tired" => 6,
    #"stagger" => 7,
    #"uphill" => 8,
    #"downhill" => 9,
    #"backing" => 10,
    #"bored" => 11,
    #"panicked" => 12,
    #"frisky" => 13
};

define table $gait-descriptions =
{
    #"normal" => "Normal",
    #"pain" => "Pain",
    #"limping" => "Limping",
    #"fearful" => "Fearful",
    #"angry" => "Angry",
    #"sleepy" => "Sleepy",
    #"tired" => "Tired",
    #"stagger" => "Stagger",
    #"uphill" => "Uphill",
    #"downhill" => "Downhill",
    #"backing" => "Backing",
    #"bored" => "Bored",
    #"panicked" => "Panicked",
    #"frisky" => "Frisky"
};

define method convert-to-gait ( value :: <byte> ) => (result :: <gait>)
    find-key( $gait-values, curry (\= , value ), failure: value );
end method convert-to-gait;

define method convert-to-value ( class == <gait>, type :: <gait> )
    element( $gait-values,
      type,
      default: type );
end method convert-to-value;

define method convert-to-description( class == <gait>, type :: <gait> ) => (result :: <string>)
    element($gait-descriptions, 
      type, 
      default: format-to-string("Unknown gait: %=", type));
end method convert-to-description;

