Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <instinct-gene> (<gene>)
    slot gene-instinct-lobe1 :: <byte> = 0;
    slot gene-instinct-lobe1-cell :: <byte> = 0;

    slot gene-instinct-lobe2 :: <byte> = 0;
    slot gene-instinct-lobe2-cell :: <byte> = 0;

    slot gene-instinct-lobe3 :: <byte> = 0;
    slot gene-instinct-lobe3-cell :: <byte> = 0;

    constant virtual slot gene-instinct-lobe1-description :: <string>;
    constant virtual slot gene-instinct-lobe1-cell-description :: <string>;
    constant virtual slot gene-instinct-lobe2-description :: <string>;
    constant virtual slot gene-instinct-lobe2-cell-description :: <string>;
    constant virtual slot gene-instinct-lobe3-description :: <string>;
    constant virtual slot gene-instinct-lobe3-cell-description :: <string>;

    slot gene-instinct-decision :: <byte> = 0;
    slot gene-instinct-chemical :: <byte> = 0;
    slot gene-instinct-chemical-amount :: <byte> = 0;
end class <instinct-gene>;

define class <c2-instinct-gene> (<instinct-gene>)
end class <c2-instinct-gene>;

define class <c3-instinct-gene> (<instinct-gene>)
end class <c3-instinct-gene>;

/* Implement gene protocol */
define method create-gene ( version == #"creatures2", type == 2, subtype == 5 ) => (gene :: <gene>)
    make( <c2-instinct-gene>, version: version );
end method create-gene;

define method create-gene ( version == #"creatures3", type == 2, subtype == 5 ) => (gene :: <gene>)
    make( <c3-instinct-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <instinct-gene> ) => (type :: <byte>)
    2;
end method gene-type;

define method gene-subtype ( gene :: <instinct-gene> ) => (subtype :: <byte>)
    5;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <instinct-gene> ) => (name :: <string>)
    "Instinct";
end method gene-type-name;

define method gene-instinct-lobe1-description ( gene :: <instinct-gene> ) => (result :: <string>)
    get-lobe-description(gene.gene-genome-version, gene.gene-instinct-lobe1);
end method gene-instinct-lobe1-description;

define method gene-instinct-lobe1-cell-description ( gene :: <instinct-gene> ) => (result :: <string>)
    get-lobe-cell-description(gene.gene-genome-version, gene.gene-instinct-lobe1, gene.gene-instinct-lobe1-cell);
end method gene-instinct-lobe1-cell-description;

define method gene-instinct-lobe2-description ( gene :: <instinct-gene> ) => (result :: <string>)
    get-lobe-description(gene.gene-genome-version, gene.gene-instinct-lobe2);
end method gene-instinct-lobe2-description;

define method gene-instinct-lobe2-cell-description ( gene :: <instinct-gene> ) => (result :: <string>)
    get-lobe-cell-description(gene.gene-genome-version, gene.gene-instinct-lobe2, gene.gene-instinct-lobe2-cell);
end method gene-instinct-lobe2-cell-description;


define method gene-instinct-lobe3-description ( gene :: <instinct-gene> ) => (result :: <string>)
    get-lobe-description(gene.gene-genome-version, gene.gene-instinct-lobe3);
end method gene-instinct-lobe3-description;

define method gene-instinct-lobe3-cell-description ( gene :: <instinct-gene> ) => (result :: <string>)
    get-lobe-cell-description(gene.gene-genome-version, gene.gene-instinct-lobe3, gene.gene-instinct-lobe3-cell);
end method gene-instinct-lobe3-cell-description;

define method do-read-gene( version, fs, gene :: <instinct-gene> ) => ()
    next-method();
    gene.gene-instinct-lobe1 := read-element( fs );
    gene.gene-instinct-lobe1-cell := read-element( fs );

    gene.gene-instinct-lobe2 := read-element( fs );
    gene.gene-instinct-lobe2-cell := read-element( fs );

    gene.gene-instinct-lobe3 := read-element( fs );
    gene.gene-instinct-lobe3-cell := read-element( fs );

    gene.gene-instinct-decision := read-element( fs );
    gene.gene-instinct-chemical := read-element( fs );
    gene.gene-instinct-chemical-amount := read-element( fs );

end method do-read-gene;

define method do-write-gene( version, fs, gene :: <instinct-gene> ) => ()
    next-method();
    write-element( fs, gene.gene-instinct-lobe1 );
    write-element( fs, gene.gene-instinct-lobe1-cell );

    write-element( fs, gene.gene-instinct-lobe2 );
    write-element( fs, gene.gene-instinct-lobe2-cell );

    write-element( fs, gene.gene-instinct-lobe3 );
    write-element( fs, gene.gene-instinct-lobe3-cell );

    write-element( fs, gene.gene-instinct-decision );
    write-element( fs, gene.gene-instinct-chemical );
    write-element( fs, gene.gene-instinct-chemical-amount );
end method do-write-gene;

install-gene-class("Instinct", <instinct-gene>, 2, 5);

