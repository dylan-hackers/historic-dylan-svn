Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <pigment-bleed-gene> (<gene>)
    slot gene-pigment-bleed-rotation :: <byte> = 128;
    slot gene-pigment-bleed-swap :: <byte> = 128;
end class <pigment-bleed-gene>;

/* Implement gene protocol */
define method create-gene ( version, type == 2, subtype == 7 ) => (gene :: <gene>)
    make( <pigment-bleed-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <pigment-bleed-gene> ) => (type :: <byte>)
    2;
end method gene-type;

define method gene-subtype ( gene :: <pigment-bleed-gene> ) => (subtype :: <byte>)
   7;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <pigment-bleed-gene> ) => (name :: <string>)
    "Pigment Bleed";
end method gene-type-name;

define method do-read-gene( version, fs, gene :: <pigment-bleed-gene> ) => ()
    next-method();
    gene.gene-pigment-bleed-rotation := read-element( fs );
    gene.gene-pigment-bleed-swap := read-element( fs );
end method do-read-gene;

define method do-write-gene( version, fs, gene :: <pigment-bleed-gene> ) => ()
    next-method();
    write-element( fs, gene.gene-pigment-bleed-rotation );
    write-element( fs, gene.gene-pigment-bleed-swap );
end method do-write-gene;

install-gene-class("Pigment Bleed", <pigment-bleed-gene>, 2, 7);
