Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <pigment-gene> (<gene>)
    slot gene-pigment-color :: <pigmentation> = #"red";
    slot gene-pigment-intensity :: <byte> = 128;
end class <pigment-gene>;

/* Implement gene protocol */
define method create-gene ( version, type == 2, subtype == 6 ) => (gene :: <gene>)
    make( <pigment-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <pigment-gene> ) => (type :: <byte>)
    2;
end method gene-type;

define method gene-subtype ( gene :: <pigment-gene> ) => (subtype :: <byte>)
    6;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <pigment-gene> ) => (name :: <string>)
    "Pigment";
end method gene-type-name;

define method gene-short-description( gene :: <pigment-gene> ) => (name :: <string>)
    format-to-string(
      "Pigment gene %s", 
      convert-to-description( <pigmentation>, gene.gene-pigment-color ) );
end method gene-short-description;

define method do-read-gene( version, fs, gene :: <pigment-gene> ) => ()
    next-method();
    gene.gene-pigment-color := convert-to-pigmentation( read-element( fs ) );
    gene.gene-pigment-intensity := read-element( fs );
end method do-read-gene;

define method do-write-gene( version, fs, gene :: <pigment-gene> ) => ()
    next-method();
    write-element( fs, convert-to-value( <pigmentation>, gene.gene-pigment-color ) );
    write-element( fs, gene.gene-pigment-intensity );
end method do-write-gene;

install-gene-class("Pigment", <pigment-gene>, 2, 6);

