Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define constant <svrule-opcode> = type-union(<byte>, one-of(
    #"<end>",
    #"0",
    #"1",
    #"64",
    #"255",
    #"chem0",
    #"chem1",
    #"chem2",
    #"chem3",
    #"state",
    #"output",
    #"thres",
    #"type0",
    #"type1",
    #"anded0",
    #"anded1",
    #"input",
    #"conduct",
    #"suscept",
    #"STW",
    #"LTW",
    #"strength",
    #"32",
    #"128",
    #"rnd const",
    #"chem4",
    #"chem5",
    #"leak in",
    #"leak out",
    #"curr src leak in",
    #"TRUE",
    #"PLUS",
    #"MINUS",
    #"TIMES",
    #"INCR",
    #"DECR",
    #"FALSE",
    #"multiply",
    #"average",
    #"move twrds",
    #"random",
    #"<ERROR>"
));


define table $svrule-opcode-values = 
{
    #"<end>" => 0,
    #"0" => 1,
    #"1" => 2,
    #"64" => 3,
    #"255" => 4,
    #"chem0" => 5,
    #"chem1" => 6,
    #"chem2" => 7,
    #"chem3" => 8,
    #"state" => 9,
    #"output" => 10,
    #"thres" => 11,
    #"type0" => 12,
    #"type1" => 13,
    #"anded0" => 14,
    #"anded1" => 15,
    #"input" => 16,
    #"conduct" => 17,
    #"suscept" => 18,
    #"STW" => 19,
    #"LTW" => 20,
    #"strength" => 21,
    #"32" => 22,
    #"128" => 23,
    #"rnd const" => 24,
    #"chem4" => 25,
    #"chem5" => 26,
    #"leak in" => 27,
    #"leak out" => 28,
    #"curr src leak in" => 29,
    #"TRUE" => 30,
    #"PLUS" => 31,
    #"MINUS" => 32,
    #"TIMES" => 33,
    #"INCR" => 34,
    #"DECR" => 35,
    #"FALSE" => 36,
    #"multiply" => 37,
    #"average" => 38,
    #"move twrds" => 39,
    #"random" => 40,
    #"<ERROR>" => 41
};

define table $svrule-opcode-descriptions =
{
    #"<end>" => "<end>",
    #"0" => "0",
    #"1" => "1",
    #"64" => "64",
    #"255" => "255",
    #"chem0" => "chem0",
    #"chem1" => "chem1",
    #"chem2" => "chem2",
    #"chem3" => "chem3",
    #"state" => "state",
    #"output" => "output",
    #"thres" => "thres",
    #"type0" => "type0",
    #"type1" => "type1",
    #"anded0" => "anded0",
    #"anded1" => "anded1",
    #"input" => "input",
    #"conduct" => "conduct",
    #"suscept" => "suscept",
    #"STW" => "STW",
    #"LTW" => "LTW",
    #"strength" => "strength",
    #"32" => "32",
    #"128" => "128",
    #"rnd const" => "rnd const",
    #"chem4" => "chem4",
    #"chem5" => "chem5",
    #"leak in" => "leak in",
    #"leak out" => "leak out",
    #"curr src leak in" => "curr src leak in",
    #"TRUE" => "TRUE",
    #"PLUS" => "PLUS",
    #"MINUS" => "MINUS",
    #"TIMES" => "TIMES",
    #"INCR" => "INCR",
    #"DECR" => "DECR",
    #"FALSE" => "FALSE",
    #"multiply" => "multiply",
    #"average" => "average",
    #"move twrds" => "move twrds",
    #"random" => "random",
    #"<ERROR>" => "<ERROR>"
};

define method convert-to-svrule-opcode ( value :: <byte> ) => (result :: <svrule-opcode>)
    find-key( $svrule-opcode-values, curry (\= , value ), failure: value );
end method convert-to-svrule-opcode;

define method convert-to-value ( class == <svrule-opcode>, type :: <svrule-opcode> )
    element( $svrule-opcode-values,
      type,
      default: type );
end method convert-to-value;

define method convert-to-description( class == <svrule-opcode>, type :: <svrule-opcode> ) => (result :: <string>)
    element($svrule-opcode-descriptions, 
      type, 
      default: format-to-string("Unknown opcode: %=", type));
end method convert-to-description;

