Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <stimulus-gene> (<gene>)
    slot gene-stimulus-type :: <stimulus> = 0;
    slot gene-stimulus-significance :: <byte> = 0;
    slot gene-stimulus-sensory-neuron :: <general-sensory> = 0;
    slot gene-stimulus-intensity :: <byte> = 0;
    slot gene-stimulus-modulate? :: <boolean> = #f;
    slot gene-stimulus-add-offset? :: <boolean> = #f;
    slot gene-stimulus-sensed-asleep? :: <boolean> = #f;
    slot gene-stimulus-other-feature :: <byte> = 0;
    slot gene-stimulus-chemical1 :: <byte> = 0;
    slot gene-stimulus-chemical1-amount :: <byte> = 0;
    slot gene-stimulus-chemical2 :: <byte> = 0;
    slot gene-stimulus-chemical2-amount :: <byte> = 0;
    slot gene-stimulus-chemical3 :: <byte> = 0;
    slot gene-stimulus-chemical3-amount :: <byte> = 0;
    slot gene-stimulus-chemical4 :: <byte> = 0;
    slot gene-stimulus-chemical4-amount :: <byte> = 0;
end class <stimulus-gene>;

define class <c2-stimulus-gene> (<stimulus-gene>)
end class <c2-stimulus-gene>;


define class <c3-stimulus-gene> (<stimulus-gene>)
end class <c3-stimulus-gene>;


/* Implement gene protocol */
define method create-gene ( version == #"creatures2", type == 2, subtype == 0 ) => (gene :: <gene>)
    make( <c2-stimulus-gene>, version: version );
end method create-gene;

define method create-gene ( version == #"creatures3", type == 2, subtype == 0 ) => (gene :: <gene>)
    make( <c3-stimulus-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <stimulus-gene> ) => (type :: <byte>)
    2;
end method gene-type;

define method gene-subtype ( gene :: <stimulus-gene> ) => (subtype :: <byte>)
    0;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <stimulus-gene> ) => (name :: <string>)
    "Stimulus";
end method gene-type-name;

define method do-read-gene( version, fs, gene :: <stimulus-gene> ) => ()
    next-method();

    gene.gene-stimulus-type := read-element( fs );
    gene.gene-stimulus-significance := read-element( fs );
    gene.gene-stimulus-sensory-neuron := read-element( fs );
    gene.gene-stimulus-intensity := read-element( fs );
    
    let feature = read-element( fs );
    gene.gene-stimulus-modulate? := logbit?(0, feature);
    gene.gene-stimulus-add-offset? := logbit?(1, feature);
    gene.gene-stimulus-sensed-asleep? := logbit?(2, feature);
    gene.gene-stimulus-other-feature := logand(248, feature);

    gene.gene-stimulus-chemical1 := read-element( fs );
    gene.gene-stimulus-chemical1-amount := read-element( fs );
    gene.gene-stimulus-chemical2 := read-element( fs );
    gene.gene-stimulus-chemical2-amount := read-element( fs );
    gene.gene-stimulus-chemical3 := read-element( fs );
    gene.gene-stimulus-chemical3-amount := read-element( fs );
    gene.gene-stimulus-chemical4 := read-element( fs );
    gene.gene-stimulus-chemical4-amount := read-element( fs );
end method do-read-gene;

define method do-write-gene( version, fs, gene :: <stimulus-gene> ) => ()
    next-method();

    write-element( fs, gene.gene-stimulus-type );
    write-element( fs, gene.gene-stimulus-significance );
    write-element( fs, gene.gene-stimulus-sensory-neuron );
    write-element( fs, gene.gene-stimulus-intensity );

    write-element( fs, logior( if(gene.gene-stimulus-modulate?)      #b00000001 else 0 end if,
                               if(gene.gene-stimulus-add-offset?)    #b00000010 else 0 end if,
                               if(gene.gene-stimulus-sensed-asleep?) #b00000100 else 0 end if,
                               gene.gene-stimulus-other-feature ) );

    write-element( fs, gene.gene-stimulus-chemical1 );
    write-element( fs, gene.gene-stimulus-chemical1-amount );
    write-element( fs, gene.gene-stimulus-chemical2 );
    write-element( fs, gene.gene-stimulus-chemical2-amount );
    write-element( fs, gene.gene-stimulus-chemical3 );
    write-element( fs, gene.gene-stimulus-chemical3-amount );
    write-element( fs, gene.gene-stimulus-chemical4 );
    write-element( fs, gene.gene-stimulus-chemical4-amount );
end method do-write-gene;

install-gene-class("Stimulus", <stimulus-gene>, 2, 0);
