Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <reaction-gene> (<gene>)
    slot gene-reaction-lhs1-amount = 0;
    slot gene-reaction-lhs1 = 0;
    slot gene-reaction-lhs2-amount = 0;
    slot gene-reaction-lhs2 = 0;
    slot gene-reaction-rhs1-amount = 0;
    slot gene-reaction-rhs1 = 0;
    slot gene-reaction-rhs2-amount = 0;
    slot gene-reaction-rhs2 = 0;
    slot gene-reaction-rate = 0;
end class <reaction-gene>;

/* Implement gene protocol */
define method create-gene ( version, type == 1, subtype == 2 ) => (gene :: <gene>)
    make( <reaction-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <reaction-gene> ) => (type :: <byte>)
    1;
end method gene-type;

define method gene-subtype ( gene :: <reaction-gene> ) => (subtype :: <byte>)
    2;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <reaction-gene> ) => (name :: <string>)
    "Reaction";
end method gene-type-name;

define method do-read-gene( version, fs, gene :: <reaction-gene> ) => ()
    next-method();

    gene.gene-reaction-lhs1-amount := read-element( fs );
    gene.gene-reaction-lhs1 := read-element( fs );
    gene.gene-reaction-lhs2-amount := read-element( fs );
    gene.gene-reaction-lhs2 := read-element( fs );

    gene.gene-reaction-rhs1-amount := read-element( fs );
    gene.gene-reaction-rhs1 := read-element( fs );
    gene.gene-reaction-rhs2-amount := read-element( fs );
    gene.gene-reaction-rhs2 := read-element( fs );

    gene.gene-reaction-rate := read-element( fs );
end method do-read-gene;

define method do-write-gene( version, fs, gene :: <reaction-gene> ) => ()
    next-method();

    write-element( fs, gene.gene-reaction-lhs1-amount);
    write-element( fs, gene.gene-reaction-lhs1);
    write-element( fs, gene.gene-reaction-lhs2-amount);
    write-element( fs, gene.gene-reaction-lhs2);

    write-element( fs, gene.gene-reaction-rhs1-amount);
    write-element( fs, gene.gene-reaction-rhs1);
    write-element( fs, gene.gene-reaction-rhs2-amount);
    write-element( fs, gene.gene-reaction-rhs2);

    write-element( fs, gene.gene-reaction-rate);
end method do-write-gene;

install-gene-class("Chemical Reaction", <reaction-gene>, 1, 2);

