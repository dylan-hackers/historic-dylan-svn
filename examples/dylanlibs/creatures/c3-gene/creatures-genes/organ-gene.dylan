Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <organ-gene> (<gene>)
    slot gene-organ-clock-rate :: <byte> = 0;
    slot gene-organ-life-force-repair-rate :: <byte> = 0;
    slot gene-organ-life-force-start :: <byte> = 0;
    slot gene-organ-biotick-start :: <byte> = 0;
    slot gene-organ-atp-damage-coefficient :: <byte> = 0;
end class <organ-gene>;

/* Implement gene protocol */
define method create-gene ( version, type == 3, subtype == 0 ) => (gene :: <gene>)
    make( <organ-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <organ-gene> ) => (type :: <byte>)
    3;
end method gene-type;

define method gene-subtype ( gene :: <organ-gene> ) => (subtype :: <byte>)
   0;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <organ-gene> ) => (name :: <string>)
    "Organ";
end method gene-type-name;

define method do-read-gene( version, fs, gene :: <organ-gene> ) => ()
    next-method();

    gene.gene-organ-clock-rate := read-element( fs );
    gene.gene-organ-life-force-repair-rate := read-element( fs );
    gene.gene-organ-life-force-start := read-element( fs );
    gene.gene-organ-biotick-start := read-element( fs );
    gene.gene-organ-atp-damage-coefficient := read-element( fs );
end method do-read-gene;


define method do-write-gene( version, fs, gene :: <organ-gene> ) => ()
    next-method();

    write-element( fs, gene.gene-organ-clock-rate );
    write-element( fs, gene.gene-organ-life-force-repair-rate );
    write-element( fs, gene.gene-organ-life-force-start );
    write-element( fs, gene.gene-organ-biotick-start );
    write-element( fs, gene.gene-organ-atp-damage-coefficient );
end method do-write-gene;

install-gene-class("Organ", <organ-gene>, 3, 0);
