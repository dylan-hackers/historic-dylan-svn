Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define generic get-lobe-description( version, lobe :: <byte> ) => (result :: <string>);
define generic get-lobe-cell-description( version, lobe :: <byte>, cell :: <byte> ) => (result :: <string>);

define method get-lobe-description( version == #"creatures2", lobe :: <byte> ) => (result :: <string>)
    select(lobe)
      0 => "Perception i/ps";
      1 => "Drive i/ps";
      2 => "Stim source i/ps";
      3 => "Verb i/ps";
      4 => "Noun i/ps";
      5 => "General Sensory i/ps";
      6 => "Decision i/ps";
      7 => "Attention i/ps";
      8 => "Concept";
      9 => "Sandrabellum";
      otherwise => format-to-string("Brain Lobe %d", lobe);
    end select;
end method get-lobe-description;

// This really needs to look up the names from the current genome
/*
define method get-lobe-description( version == #"creatures3", lobe :: <byte> ) => (result :: <string>)
    select(lobe)
      0 => "00: Unknown";
      1 => "01: verb";
      2 => "02: noun";
      3 => "03: stim";
      4 => "04: Unknown";
      5 => "05: driv";
      6 => "06: situ";
      7 => "07: detl";
      8 => "08: Unknown";
      9 => "09: attn";
     10 => "10: decn";
      otherwise => format-to-string("Brain Lobe %d", lobe);
    end select;
end method get-lobe-description;
*/
define method get-lobe-description( version == #"creatures3", lobe :: <byte> ) => (result :: <string>)
    select(lobe)
      0 => "00: Unknown";
      1 => "01: verb";
      2 => "02: noun";
      3 => "03: stim";
      4 => "04: Unknown";
      5 => "05: driv";
      6 => "06: situ";
      7 => "07: detl";
      8 => "08: Unknown";
      9 => "09: attn";
     10 => "10: decn";
      otherwise => format-to-string("Brain Lobe %d", lobe);
    end select;
end method get-lobe-description;


define method get-lobe-cell-description( version, lobe :: <byte>, cell :: <byte> ) => (result :: <string>)
    format-to-string("Cell %d", cell);
end method get-lobe-cell-description;

define method get-lobe-cell-description( version, lobe == 0, cell :: <byte> ) => (result :: <string>)
    next-method();
end method get-lobe-cell-description;

define method get-lobe-cell-description( version == #"creatures2", lobe == 1, cell :: <byte> ) => (result :: <string>)
    case
      cell <= 16 => format-to-string("Cell %d (%s)", cell, convert-to-chemical-description( version, cell + 1 ) );
      otherwise => next-method();
    end case;
end method get-lobe-cell-description;

define method get-lobe-cell-description( version == #"creatures2", lobe == 2, cell :: <byte> ) => (result :: <string>)
    let noun = select(cell)
      0  => "Myself - norn";
      1  => "Hand";
      2  => "Button";
      3  => "Thing";
      4  => "Plant";
      5  => "Egg";
      6  => "Food";
      7  => "Drink";
      8  => "Dispensor";
      9  => "Instrument";
      10 => "Edge";
      11 => "Detritus";
      12 => "Soother";
      13 => "Toy";
      14 => "Cloud";
      15 => "BadPlant";
      16 => "Nest";
      17 => "BadBug";
      18 => "Bug";
      19 => "BadCritter";
      20 => "Critter";
      21 => "Seed";
      22 => "Leaf";
      23 => "Root";
      24 => "Flower";
      25 => "Fruit";
      26 => "Mover";
      27 => "Lift";
      28 => "Computer";
      29 => "MediaBox";
      30 => "Comp30";
      31 => "LeftRight";
      32 => "Incubator";
      33 => "Teleporter";
      34 => "Comp34";
      35 => "Machine";
      36 => "Norn";
      37 => "Grendel";
      38 => "Ettin";
      39 => "Shee";
      otherwise => format-to-string("Noun %d", cell);
    end select;

    format-to-string("Cell %d (%s)", cell, noun);
end method get-lobe-cell-description;


define method get-lobe-cell-description( version == #"creatures2", lobe == 3, cell :: <byte> ) => (result :: <string>)
    let verb = select(cell)
      0  => "Quiescent";
      1  => "push";
      2  => "pull";
      3  => "stop";
      4  => "come";
      5  => "run";
      6  => "get";
      7  => "drop";
      8  => "Say what you need";
      9  => "Rest";
      10 => "left";
      11 => "right";
      12 => "Eat";
      13 => "Hit";
      otherwise => format-to-string("Verb %d", cell);
    end select;

    format-to-string("Cell %d (%s)", cell, verb);
end method get-lobe-cell-description;

define method get-lobe-cell-description( version == #"creatures2", lobe == 4, cell :: <byte> ) => (result :: <string>)
    get-lobe-cell-description(version, 2, cell);
end method get-lobe-cell-description;

define method get-lobe-cell-description( version == #"creatures2", lobe == 5, cell :: <byte> ) => (result :: <string>)
    let word = select(cell)
      0  => "I've been patted";
      1  => "I've been slapped";
      2  => "I've bumped into a wall";
      3  => "I am near a wall";
      4  => "I am in a vehicle";
      5  => "User has spoken";
      6  => "Creature has spoken";
      7  => "Own kind has spoken";
      8  => "Audible event";
      9  => "Visible event";
      10 => "It is approaching";
      11 => "It is retreating";
      12 => "It is near me";
      13 => "It is active";
      14 => "It is an object";
      15 => "It is a creature";
      16 => "It is my sibling";
      17 => "It is my parent";
      18 => "It is my child";
      19 => "It is opposite sex";
      20 => "It has pushed me";
      21 => "It has hit me";
      22 => "spare4";
      23 => "spare5";
      24 => "spare6";
      25 => "spare7";
      26 => "spare8";
      27 => "spare9";
      28 => "Approaching an edge";
      29 => "Retreating from an edge";
      30 => "Falling through the air";
      31 => "Hitting the ground post fall";
     otherwise => format-to-string("Sense %d", cell);
    end select;

    format-to-string("Cell %d (%s)", cell, word);
end method get-lobe-cell-description;

define method get-lobe-cell-description( version == #"creatures2", lobe == 6, cell :: <byte> ) => (result :: <string>)
    get-lobe-cell-description(version, 3, cell);
end method get-lobe-cell-description;

define method get-lobe-cell-description( version == #"creatures2", lobe == 7, cell :: <byte> ) => (result :: <string>)
    get-lobe-cell-description(version, 2, cell);
end method get-lobe-cell-description;

define method get-lobe-cell-description( version == #"creatures3", lobe == 1, cell :: <byte> ) => (result :: <string>)
    let verb = select(cell)
      0  => "Quiescent";
      1  => "push";
      2  => "pull";
      3  => "stop";
      4  => "come";
      5  => "run";
      6  => "get";
      7  => "drop";
      8  => "Say what you need";
      9  => "Rest";
      10 => "left";
      11 => "right";
      12 => "Eat";
      13 => "Hit";
      otherwise => format-to-string("Verb %d", cell);
    end select;

    format-to-string("Cell %d (%s)", cell, verb);
end method get-lobe-cell-description;

define method get-lobe-cell-description( version == #"creatures3", lobe == 2, cell :: <byte> ) => (result :: <string>)
    let noun = select(cell)
      0  => "Myself - norn";
      1  => "Hand";
      2  => "Door";
      3  => "Seed";
      4  => "Plant";
      5  => "Weed";
      6  => "Leaf";
      7  => "Flower";
      8  => "Fruit";
      9  => "Manky";
      10 => "Detritus";
      11 => "Food";
      12 => "Button";
      13 => "Bug";
      14 => "Pest";
      15 => "Critter";
      16 => "Beast";
      17 => "Nest";
      18 => "Animal Egg";
      19 => "Weather";
      20 => "Bad";
      21 => "Toy";
      22 => "Incubator";
      23 => "Dispenser";
      24 => "Tool";
      25 => "Potion";
      26 => "Elevator";
      27 => "Teleporter";
      28 => "Machinery";
      29 => "Creature egg";
      30 => "Norn home";
      31 => "Grendel home";
      32 => "Ettin home";
      33 => "Gadget";
      34 => "Something34";
      35 => "Vehicle";
      36 => "Norn";
      37 => "Grendel";
      38 => "Ettin";
      39 => "Something39";
      otherwise => format-to-string("Noun %d", cell);
    end select;

    format-to-string("Cell %d (%s)", cell, noun);
end method get-lobe-cell-description;

/*
define method get-lobe-cell-description( version == #"creatures3", lobe == 3, cell :: <byte> ) => (result :: <string>)
    let word = select(cell)
      0  => "Disappointment";
      1  => "Hand pats me";
      2  => "Creature pats me";
      3  => "Hand slaps me";
      4  => "Creature slaps me";
      5  => "Object is approaching (deprecated)";
      6  => "Object is retreating (deprecated)";
      7  => "I've hit a wall";
      8  => "Visible event (deprecated)";
      9  => "Heard unrecognised word (deprecated)";
      10 => "Heard hand speak";
      11 => "Heard creature speak";
      12 => "I am Quiescent (Periodic)";
      13 => "I have pushed";
      14 => "I have pulled";
      15 => "I have stopped";
      16 => "I have approached (periodic)";
      17 => "I have retreated";
      18 => "I have gotten";
      19 => "I have dropped";
      20 => "I have said need";
      21 => "I have rested";
      22 => "I am sleeping (periodic)";
      23 => "I am travelling (periodic)";
      24 => "I've been pushed";
      25 => "I've been hit";
      26 => "I have eaten";
      27 => "Ac6";
      28 => "Involuntary 0";
      29 => "Involuntary 1";
      30 => "Involuntary 2";
      31 => "Involuntary 3";
      32 => "Involuntary 4";
      33 => "Involuntary 5";
      34 => "Involuntary 6";
      35 => "Involuntary 7";
      36 => "Approaching an edge (deprecated)";
      37 => "Retreating from an edge (deprecated)";
      38 => "Falling through the air (deprecated)";
      38 => "Hitting the ground post fall (deprecated)";
      39 => "I have collided";
      40 => "Hand has said yes";
      41 => "Creature has said yes";
      42 => "Hand has said no";
      43 => "Creature has said no";
      44 => "I have hit";
      45 => "I have mated";
      46 => "opposite sex tickle";
      47 => "same sex tickle";
      48 => "Go nowhere";
      49 => "Go in";
      50 => "Go out";
      51 => "Go up";
      52 => "Go down";
      53 => "Go left";
      54 => "Go right";
      55 => "Reached peak of smell 0";
      56 => "Reached peak of smell 1";
      57 => "Reached peak of smell 2";
      58 => "Reached peak of smell 3";
      59 => "Reached peak of smell 4";
      60 => "Reached peak of smell 5";
      61 => "Reached peak of smell 6";
      62 => "Reached peak of smell 7";
      63 => "Reached peak of smell 8";
      64 => "Reached peak of smell 9";
      65 => "Reached peak of smell 10";
      66 => "Reached peak of smell 11";
      67 => "Reached peak of smell 12";
      68 => "Reached peak of smell 13";
      69 => "Reached peak of smell 14";
      70 => "Reached peak of smell 15";
      71 => "Reached peak of smell 16";
      72 => "Reached peak of smell 17";
      73 => "Reached peak of smell 18";
      74 => "Reached peak of smell 19";
      75 => "Wait";
      76 => "Discomfort";
      77 => "Eaten plant";
      78 => "Eaten fruit";
      79 => "Eaten food";
      80 => "Eaten animal";
      81 => "Eaten detritus";
      82 => "Consume alcohol";
      83 => "Danger plant";
      84 => "Friendly plant";
      85 => "Play bug";
      86 => "Play critter";
      87 => "Hit critter";
      88 => "Play danger animal";
      89 => "Activate button";
      90 => "Activate machine";
      91 => "Got machine";
      92 => "Hit machine";
      93 => "Got creature egg";
      94 => "Travelled in lift";
      95 => "Travelled through meta door";
      96 => "Travelled through internal door";
      97 => "Played with toy";
     otherwise => format-to-string("Sense %d", cell);
    end select;

    format-to-string("Cell %d (%s)", cell, word);
end method get-lobe-cell-description;
*/

define method get-lobe-cell-description( version == #"creatures3", lobe == 3, cell :: <byte> ) => (result :: <string>)
    get-lobe-cell-description(version, 2, cell);
end method get-lobe-cell-description;

define method get-lobe-cell-description( version == #"creatures3", lobe == 5, cell :: <byte> ) => (result :: <string>)
    case
      cell <= 14 => format-to-string("Cell %d (%s)", cell, convert-to-chemical-description( version, cell + 148 ) );
      cell > 14 & cell <= 19 
        => format-to-string("Cell %d (%s)", cell, convert-to-chemical-description( version, cell - 15 + 199 ));
      otherwise => next-method();
    end case;
end method get-lobe-cell-description;

define method get-lobe-cell-description( version == #"creatures3", lobe == 9, cell :: <byte> ) => (result :: <string>)
    get-lobe-cell-description(version, 2, cell);
end method get-lobe-cell-description;

define method get-lobe-cell-description( version == #"creatures3", lobe == 10, cell :: <byte> ) => (result :: <string>)
    get-lobe-cell-description(version, 1, cell);
end method get-lobe-cell-description;

