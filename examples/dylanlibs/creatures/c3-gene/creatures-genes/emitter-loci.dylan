Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define generic get-emitter-attachment-description( version, organ :: <byte>, tissue :: <byte>, locus :: <byte> ) =>
    ( organ :: <string>, tissue :: <string>, locus :: <string> );
define generic get-emitter-organ-description( version, organ :: <byte> ) => (result :: <string>);
define generic get-emitter-tissue-description( version, organ :: <byte>, tissue :: <byte> ) => (result :: <string>);
define generic get-emitter-locus-description( version, organ :: <byte>, tissue :: <byte>, locus :: <byte> ) => (result :: <string>);

define generic get-emitter-organ-range( version );
define generic get-emitter-tissue-range( version, organ :: <byte> );
define generic get-emitter-locus-range( version, organ :: <byte>, tissue :: <byte> );

define method get-emitter-attachment-description( version, organ :: <byte>, 
  tissue :: <byte>, locus :: <byte> ) 
   => (organ :: <string>, tissue :: <string>, locus :: <string>)

   values(get-emitter-organ-description( version, organ ),
          get-emitter-tissue-description( version, organ, tissue ),
          get-emitter-locus-description( version, organ, tissue, locus ) );
end method get-emitter-attachment-description;

define method get-emitter-organ-description( version, organ :: <byte> ) => (result :: <string>)
    select( organ )
      0 => "Brain";
      1 => "Creature";
      2 => "Organ";
      otherwise => format-to-string("Unknown Organ: %d", organ);
    end select;
end method get-emitter-organ-description;

define method get-emitter-tissue-description( version, organ :: <byte>, tissue :: <byte> ) => (result :: <string>)
    format-to-string("Unknown Tissue: %d", tissue);    
end method get-emitter-tissue-description;

define method get-emitter-locus-description( version, organ :: <byte>, tissue :: <byte>, locus :: <byte> ) => (result :: <string>)
    format-to-string("Unknown Locus: %d", locus);    
end method get-emitter-locus-description;

define method get-emitter-tissue-description( version, organ == 0, tissue :: <byte> ) => (result :: <string>)
    get-lobe-description(version, tissue);
end method get-emitter-tissue-description;

// All brain lobes have the same locus descriptions...
define method get-emitter-locus-description( version == #"creatures2", organ == 0, tissue :: <byte>, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "Lobe Activity";
      1  => "# Loose dendrites/cells type 0";
      2  => "# Loose dendrites/cells type 1";
      otherwise => format-to-string("%s output", get-lobe-cell-description(version, tissue, locus - 3) );
    end select;
end method get-emitter-locus-description;

define method get-emitter-locus-description( version == #"creatures3", organ == 0, tissue :: <byte>, locus :: <byte> ) => (result :: <string>)
    let (neuron, state) = floor/(locus, 4);
    format-to-string("%s - state %d",
                     get-lobe-cell-description(version, tissue, neuron), state);
end method get-emitter-locus-description;


define method get-emitter-tissue-description( version, organ == 1, tissue :: <byte> ) => (result :: <string>)
    select(tissue)
      0 => "Somatic";
      1 => "Circulatory";
      2 => "Reproductive";
      3 => "Immune";
      4 => "Sensorimotor";
      5 => "Drives";
      otherwise => next-method();
    end select;
end method get-emitter-tissue-description;

define method get-emitter-locus-description( version, organ == 1, tissue == 0, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "Muscle energy used";
      otherwise => next-method();
    end select;
end method get-emitter-locus-description;

define method get-emitter-locus-description( version, organ == 1, tissue == 1, locus :: <byte> ) => (result :: <string>)
    format-to-string("Floating receptor/emitter %d", locus);
end method get-emitter-locus-description;

define method get-emitter-locus-description( version == #"creatures2", organ == 1, tissue == 2, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "I am fertile";
      1  => "I am pregnant";
      otherwise => next-method();
    end select;
end method get-emitter-locus-description;

define method get-emitter-locus-description( version == #"creatures3", organ == 1, tissue == 2, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "I am fertile";
      1  => "I am pregnant";
      2  => "I am ovulating";
      3  => "I am receptive to sperm";
      4  => "Chance of mutation";
      5  => "Degree of mutation";
      otherwise => next-method();
    end select;
end method get-emitter-locus-description;

define method get-emitter-locus-description( version, organ == 1, tissue == 3, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "I'm dead";
      otherwise => next-method();
    end select;
end method get-emitter-locus-description;

define method get-emitter-locus-description( version, organ == 1, tissue == 4, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "Permanently active";
      1  => "I'm asleep";
      2  => "Air is this cold";
      3  => "Air is this hot";
      4  => "Light level";
      5  => "Crowdedness";
      6  => "Ambient Radiation";
      7  => "Time of day";
      8  => "Season";
      9  => "Air quality";
      10 => "Upwards slope";
      11 => "Downwards slope";
      12 => "Headwind speed";
      13 => "Tailwind speed";
      otherwise => next-method();
    end select;
end method get-emitter-locus-description;

define method get-emitter-locus-description( version == #"creatures2", organ == 1, tissue == 5, locus :: <byte> ) => (result :: <string>)
    case
      locus <= 16  => convert-to-chemical-description( version, locus + 1 );
      otherwise => next-method();
    end case;
end method get-emitter-locus-description;

/* CA
define method get-emitter-locus-description( version == #"creatures3", organ == 1, tissue == 5, locus :: <byte> ) => (result :: <string>)
    case
      locus <= 14  => convert-to-chemical-description( version, locus + 148 );
      otherwise => next-method();
    end case;
end method get-emitter-locus-description;
*/
define method get-emitter-locus-description( version == #"creatures3", organ == 1, tissue == 5, locus :: <byte> ) => (result :: <string>)
    case
      locus <= 14  => convert-to-chemical-description( version, locus + 148 );
      locus > 14 & locus <= 19 
        => convert-to-chemical-description( version, locus - 15 + 199 );
        
      otherwise => next-method();
    end case;
end method get-emitter-locus-description;


define method get-emitter-tissue-description( version, organ == 2, tissue :: <byte> ) => (result :: <string>)
    "Organ";
end method get-emitter-tissue-description;

define method get-emitter-locus-description( version, organ == 2, tissue :: <byte>, locus :: <byte> ) => (result :: <string>)
    select(locus)
      0  => "Clock Rate";
      1  => "Repair Rate";
      2  => "Injury";
      otherwise => next-method();
    end select;
end method get-emitter-locus-description;

define method get-emitter-organ-range(version)
    range(from: 0, to: 2);
end method get-emitter-organ-range;

define method get-emitter-tissue-range( version, organ :: <byte> )
    range(from: 0, to: select( organ )
                         0 => 255;
                         1 => 5;
                         2 => 0;
                         otherwise => 255;
                       end select);
end method get-emitter-tissue-range;

define method get-emitter-locus-range( version, organ :: <byte>, tissue :: <byte> )
    range(from: 0, to: 255);
end method get-emitter-locus-range;

define method get-emitter-locus-range( version, organ == 0, tissue :: <byte> )
    range(from: 0, to: 255);
end method get-emitter-locus-range;

define method get-emitter-locus-range( version, organ == 1, tissue :: <byte> )
    range(from: 0, to: select( tissue )
                         0 => 0;
                         1 => 255;
                         2 => 5;
                         3 => 0;
                         4 => 13;
                         5 => 19;
                         otherwise => 255;
                       end select);
end method get-emitter-locus-range;

define method get-emitter-locus-range( version, organ == 2, tissue :: <byte> )
    range(from: 0, to: 2);
end method get-emitter-locus-range;

