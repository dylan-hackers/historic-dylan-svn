Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <brain-tract-gene> (<gene>)
    slot gene-brain-tract-unknown1 :: <byte> = 0;
    slot gene-brain-tract-source-lobe :: <string>;
    slot gene-brain-tract-source-lower :: <integer> = 0;
    slot gene-brain-tract-source-upper :: <integer> = 0;
    slot gene-brain-tract-source-amount :: <integer> = 0;

    slot gene-brain-tract-destination-lobe :: <string>;
    slot gene-brain-tract-destination-lower :: <integer> = 0;
    slot gene-brain-tract-destination-upper :: <integer> = 0;
    slot gene-brain-tract-destination-amount :: <integer> = 0;

    slot gene-brain-tract-unknown = #f;
end class <brain-tract-gene>;

/* Implement gene protocol */
define method create-gene ( version == #"creatures3", type == 0, subtype == 2 ) => (gene :: <gene>)
    make( <brain-tract-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <brain-tract-gene> ) => (type :: <byte>)
    0;
end method gene-type;

define method gene-subtype ( gene :: <brain-tract-gene> ) => (subtype :: <byte>)
    2;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <brain-tract-gene> ) => (name :: <string>)
    "Brain Tract";
end method gene-type-name;

define method do-read-gene( version == #"creatures3", fs, gene :: <brain-tract-gene> ) => ()
    next-method();

    gene.gene-brain-tract-unknown1 := read-16-bits( fs, endian: #"big" );

    gene.gene-brain-tract-source-lobe := as(<string>, read(fs, 4));
    gene.gene-brain-tract-source-lower := read-16-bits( fs, endian: #"big" );
    gene.gene-brain-tract-source-upper := read-16-bits( fs, endian: #"big" );
    gene.gene-brain-tract-source-amount := read-16-bits( fs, endian: #"big" );

    gene.gene-brain-tract-destination-lobe := as(<string>, read(fs, 4));
    gene.gene-brain-tract-destination-lower := read-16-bits( fs, endian: #"big" );
    gene.gene-brain-tract-destination-upper := read-16-bits( fs, endian: #"big" );
    gene.gene-brain-tract-destination-amount := read-16-bits( fs, endian: #"big" );

    gene.gene-brain-tract-unknown := read(fs, #x6a);
end method do-read-gene;

define method do-write-gene( version == #"creatures3", fs, gene :: <brain-tract-gene> ) => ()
    next-method();

    write-16-bits( fs, gene.gene-brain-tract-unknown1, endian: #"big" );

    write( fs, gene.gene-brain-tract-source-lobe );
    write-16-bits( fs, gene.gene-brain-tract-source-lower, endian: #"big" );
    write-16-bits( fs, gene.gene-brain-tract-source-upper, endian: #"big" );
    write-16-bits( fs, gene.gene-brain-tract-source-amount, endian: #"big" );

    write( fs, gene.gene-brain-tract-destination-lobe );
    write-16-bits( fs, gene.gene-brain-tract-destination-lower, endian: #"big" );
    write-16-bits( fs, gene.gene-brain-tract-destination-upper, endian: #"big" );
    write-16-bits( fs, gene.gene-brain-tract-destination-amount, endian: #"big" );

    write( fs, gene.gene-brain-tract-unknown );
end method do-write-gene;

install-gene-class("Brain Tract", <brain-tract-gene>, 0, 2);

