Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <initial-concentration-gene> (<gene>)
    slot gene-initial-concentration-chemical :: <chemical> = 0;
    slot gene-initial-concentration-amount :: <byte> = 0;
end class <initial-concentration-gene>;

/* Implement gene protocol */
define method create-gene ( version, type == 1, subtype == 4 ) => (gene :: <gene>)
    make( <initial-concentration-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <initial-concentration-gene> ) => (type :: <byte>)
    1;
end method gene-type;

define method gene-subtype ( gene :: <initial-concentration-gene> ) => (subtype :: <byte>)
    4;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <initial-concentration-gene> ) => (name :: <string>)
    "Initial Concentration";
end method gene-type-name;

define method gene-short-description( gene :: <initial-concentration-gene> ) => (name :: <string>)
    format-to-string("Initial Concentration: %s", 
      convert-to-chemical-description( gene.gene-genome-version, gene.gene-initial-concentration-chemical) );
end method gene-short-description;

define method do-read-gene( version, fs, gene :: <initial-concentration-gene> ) => ()
    next-method();

    gene.gene-initial-concentration-chemical := read-element( fs );
    gene.gene-initial-concentration-amount := read-element( fs );

end method do-read-gene;

define method do-write-gene( version, fs, gene :: <initial-concentration-gene> ) => ()
    next-method();

    write-element( fs, gene.gene-initial-concentration-chemical );
    write-element( fs, gene.gene-initial-concentration-amount );

end method do-write-gene;

install-gene-class("Initial Concentration", <initial-concentration-gene>, 1, 4);
