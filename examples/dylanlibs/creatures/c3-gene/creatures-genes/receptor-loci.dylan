Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define generic get-receptor-attachment-description( version, organ :: <byte>, tissue :: <byte>, locus :: <byte> ) =>
    ( organ :: <string>, tissue :: <string>, locus :: <string> );
define generic get-receptor-organ-description( version, organ :: <byte> ) => (result :: <string>);
define generic get-receptor-tissue-description( version, organ :: <byte>, tissue :: <byte> ) => (result :: <string>);
define generic get-receptor-locus-description( version, organ :: <byte>, tissue :: <byte>, locus :: <byte> ) => (result :: <string>);

define generic get-receptor-organ-range(version);
define generic get-receptor-tissue-range( version, organ :: <byte> );
define generic get-receptor-locus-range( version, organ :: <byte>, tissue :: <byte> );

define method get-receptor-attachment-description( version, organ :: <byte>, 
  tissue :: <byte>, locus :: <byte> ) 
   => (organ :: <string>, tissue :: <string>, locus :: <string>)

   values(get-receptor-organ-description( version, organ ),
          get-receptor-tissue-description( version, organ, tissue ),
          get-receptor-locus-description( version, organ, tissue, locus ) );
end method get-receptor-attachment-description;

define method get-receptor-organ-description( version, organ :: <byte> ) => (result :: <string>)
    select( organ )
      0 => "Brain";
      1 => "Creature";
      2 => "Organ";
      otherwise => format-to-string("Unknown Organ: %d", organ);
    end select;
end method get-receptor-organ-description;

define method get-receptor-organ-description( version == #"creatures3", organ :: <byte> ) => (result :: <string>)
    select( organ )
      3 => "Current Reaction";      
      otherwise => next-method();
    end select;
end method get-receptor-organ-description;


define method get-receptor-tissue-description( version, organ :: <byte>, tissue :: <byte> ) => (result :: <string>)
    format-to-string("Unknown Tissue: %d", tissue);    
end method get-receptor-tissue-description;

define method get-receptor-locus-description( version, organ :: <byte>, tissue :: <byte>, locus :: <byte> ) => (result :: <string>)
    format-to-string("Unknown Locus: %d", locus);    
end method get-receptor-locus-description;

define method get-receptor-tissue-description( version, organ == 0, tissue :: <byte> ) => (result :: <string>)
    get-lobe-description(version, tissue);
end method get-receptor-tissue-description;

// All brain lobes have the same locus descriptions...
define method get-receptor-locus-description( version == #"creatures2", organ == 0, tissue :: <byte>, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "Cell Threshold";
      1  => "Cell leakage rate";
      2  => "Cell rest state";
      3  => "Dendrite 0 susceptibility decay rate";
      4  => "Dendrite 0 STW decay rate";
      5  => "Dendrite 0 LTW decay rate";
      6  => "Dendrite 0 strength gain rate";
      7  => "Dendrite 0 strength loss rate";
      8  => "Dendrite 0 susceptibility decay rate";
      9  => "Dendrite 0 STW decay rate";
      10 => "Dendrite 0 LTW decay rate";
      11 => "Dendrite 0 strength gain rate";
      12 => "Dendrite 0 strength loss rate";
      13 => "Chemical 0";
      14 => "Chemical 1";
      15 => "Chemical 2";
      16 => "Chemical 3";
      17 => "Chemical 4";
      18 => "Chemical 5";
      otherwise => format-to-string("%s state", get-lobe-cell-description(version, tissue, locus - 19) );
    end select;
end method get-receptor-locus-description;

define method get-receptor-locus-description( version == #"creatures3", organ == 0, tissue :: <byte>, locus :: <byte> ) => (result :: <string>)
    let (neuron, state) = floor/(locus, 4);
    format-to-string("%s - state %d",
                     get-lobe-cell-description(version, tissue, neuron), state);
end method get-receptor-locus-description;

define method get-receptor-tissue-description( version, organ == 1, tissue :: <byte> ) => (result :: <string>)
    select(tissue)
      0 => "Somatic";
      1 => "Circulatory";
      2 => "Reproductive";
      3 => "Immune";
      4 => "Sensorimotor";
      5 => "Drives";
      otherwise => next-method();
    end select;
end method get-receptor-tissue-description;

define method get-receptor-locus-description( version, organ == 1, tissue == 0, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "Become a child";
      1  => "Become an adolescent";
      2  => "Become a youth";
      3  => "Become an adult";
      4  => "Become old";
      5  => "Become senile";
      6  => "Die of old age";
      otherwise => next-method();
    end select;
end method get-receptor-locus-description;

define method get-receptor-locus-description( version, organ == 1, tissue == 1, locus :: <byte> ) => (result :: <string>)
    format-to-string("Floating receptor/emitter %d", locus);
end method get-receptor-locus-description;
define method get-receptor-locus-description( version, organ == 1, tissue == 2, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "Become fertile if high";
      1  => "receptive to sperm if > 0";
      2  => "Chance of mutation";
      3  => "Degree of mutation";
      otherwise => next-method();
    end select;
end method get-receptor-locus-description;

define method get-receptor-locus-description( version, organ == 1, tissue == 3, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "Die if non-zero";
      otherwise => next-method();
    end select;
end method get-receptor-locus-description;

define method get-receptor-locus-description( version, organ == 1, tissue == 4, locus :: <byte> ) => (result :: <string>)
    case
      locus <= 7  => format-to-string("Involuntary action %d", locus);
      locus <= 24 => format-to-string("Gait %d", locus - 8);
      otherwise => next-method();
    end case;
end method get-receptor-locus-description;

define method get-receptor-locus-description( version == #"creatures2", organ == 1, tissue == 5, locus :: <byte> ) => (result :: <string>)
    case
      locus <= 16  => convert-to-chemical-description( version, locus + 1 );
      otherwise => next-method();
    end case;
end method get-receptor-locus-description;

define method get-receptor-locus-description( version == #"creatures3", organ == 1, tissue == 5, locus :: <byte> ) => (result :: <string>)
    case
      locus <= 14  => convert-to-chemical-description( version, locus + 148 );
      locus > 14 & locus <= 19 
        => convert-to-chemical-description( version, locus - 15 + 199 );
      otherwise => next-method();
    end case;
end method get-receptor-locus-description;

define method get-receptor-locus-description( version == #"creatures3", organ == 3, tissue == 0, locus :: <byte> ) => (result :: <string>)
    case
      locus == 0  => "Reaction Rate";
      otherwise => next-method();
    end case;
end method get-receptor-locus-description;


define method get-receptor-tissue-description( version, organ == 2, tissue :: <byte> ) => (result :: <string>)
    "Organ";
end method get-receptor-tissue-description;

define method get-receptor-tissue-description( version == #"creatures3", organ == 3, tissue :: <byte> ) => (result :: <string>)
    "No Tissue";
end method get-receptor-tissue-description;

define method get-receptor-locus-description( version, organ == 2, tissue :: <byte>, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "Clock Rate";
      1  => "Repair Rate";
      2  => "Injury";
      otherwise => next-method();
    end select;
end method get-receptor-locus-description;

define method get-receptor-organ-range(version)
    range(from: 0, to: 2);
end method get-receptor-organ-range;

define method get-receptor-organ-range(version == #"creatures3")
    range(from: 0, to: 3);
end method get-receptor-organ-range;

define method get-receptor-tissue-range( version, organ :: <byte> )
    range(from: 0, to: select( organ )
                         0 => 255;
                         1 => 5;
                         2 => 0;
                         otherwise => 255;
                       end select);
end method get-receptor-tissue-range;

define method get-receptor-tissue-range( version == #"creatures3", organ == 3)
  #(0);
end method get-receptor-tissue-range;

define method get-receptor-locus-range( version, organ :: <byte>, tissue :: <byte> )
    range(from: 0, to: 255);
end method get-receptor-locus-range;

define method get-receptor-locus-range( version, organ == 0, tissue :: <byte> )
    range(from: 0, to: 255);
end method get-receptor-locus-range;

define method get-receptor-locus-range( version, organ == 1, tissue :: <byte> )
    range(from: 0, to: select( tissue )
                         0 => 6;
                         1 => 255;
                         2 => 3;
                         3 => 0;
                         4 => 255;
                         5 => 255;
                         otherwise => 255;
                       end select);
end method get-receptor-locus-range;

define method get-receptor-locus-range( version, organ == 2, tissue :: <byte> )
    range(from: 0, to: 2);
end method get-receptor-locus-range;


define method get-receptor-locus-range( version == #"creatures3", organ == 3, tissue :: <byte> )
    #(0);
end method get-receptor-locus-range;

