Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <appearance-gene> (<gene>)
    slot gene-appearance-body-part :: <body-part> = #"head";
    slot gene-appearance-breed :: <byte> = 0;
    slot gene-appearance-species :: <genus> = #"norn";
end class <appearance-gene>;

/* Implement gene protocol */
define method create-gene ( version, type == 2, subtype == 2 ) => (gene :: <gene>)
    make( <appearance-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <appearance-gene> ) => (type :: <byte>)
    2;
end method gene-type;

define method gene-subtype ( gene :: <appearance-gene> ) => (subtype :: <byte>)
    2;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <appearance-gene> ) => (name :: <string>)
    "Appearance";
end method gene-type-name;

define method gene-short-description( gene :: <appearance-gene> ) => (name :: <string>)
    format-to-string(
      "Appearance gene %s", 
      convert-to-description( <genus>, gene.gene-appearance-species ) );
end method gene-short-description;

define method do-read-gene( version, fs, gene :: <appearance-gene> ) => ()
    next-method();

    gene.gene-appearance-body-part := convert-to-body-part( read-element( fs ) );
    gene.gene-appearance-breed := read-element( fs );
    gene.gene-appearance-species := convert-to-genus( read-element( fs ) );
end method do-read-gene;

define method do-write-gene( version, fs, gene :: <appearance-gene> ) => ()
    next-method();

    write-element( fs, convert-to-value( <body-part>, gene.gene-appearance-body-part ) );
    write-element( fs, gene.gene-appearance-breed );
    write-element( fs, convert-to-value( <genus>, gene.gene-appearance-species ) );
end method do-write-gene;

install-gene-class("Appearance", <appearance-gene>, 2, 2);

