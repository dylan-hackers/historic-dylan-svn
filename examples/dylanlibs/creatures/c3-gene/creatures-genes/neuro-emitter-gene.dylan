Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <neuro-emitter-gene> (<gene>)
    slot gene-neuro-emitter-lobe1 :: <byte> = 0;
    slot gene-neuro-emitter-lobe1-cell :: <byte> = 0;
    slot gene-neuro-emitter-lobe2 :: <byte> = 0;
    slot gene-neuro-emitter-lobe2-cell :: <byte> = 0;
    slot gene-neuro-emitter-lobe3 :: <byte> = 0;
    slot gene-neuro-emitter-lobe3-cell :: <byte> = 0;

    slot gene-neuro-emitter-sample-rate :: <byte> = 0;

    slot gene-neuro-emitter-chemical1 :: <byte> = 0;
    slot gene-neuro-emitter-chemical1-amount :: <byte> = 0;
    slot gene-neuro-emitter-chemical2 :: <byte> = 0;
    slot gene-neuro-emitter-chemical2-amount :: <byte> = 0;
    slot gene-neuro-emitter-chemical3 :: <byte> = 0;
    slot gene-neuro-emitter-chemical3-amount :: <byte> = 0;
    slot gene-neuro-emitter-chemical4 :: <byte> = 0;
    slot gene-neuro-emitter-chemical4-amount :: <byte> = 0;
end class <neuro-emitter-gene>;

/* Implement gene protocol */
define method create-gene ( version == #"creatures3", type == 1, subtype == 5 ) => (gene :: <gene>)
    make( <neuro-emitter-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <neuro-emitter-gene> ) => (type :: <byte>)
    1;
end method gene-type;

define method gene-subtype ( gene :: <neuro-emitter-gene> ) => (subtype :: <byte>)
    5;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <neuro-emitter-gene> ) => (name :: <string>)
    "NeuroEmitter";
end method gene-type-name;

define method do-read-gene( version == #"creatures3", fs, gene :: <neuro-emitter-gene> ) => ()
    next-method();

    gene.gene-neuro-emitter-lobe1 := read-element( fs );
    gene.gene-neuro-emitter-lobe1-cell := read-element( fs );
    gene.gene-neuro-emitter-lobe2 := read-element( fs );
    gene.gene-neuro-emitter-lobe2-cell := read-element( fs );
    gene.gene-neuro-emitter-lobe3 := read-element( fs );
    gene.gene-neuro-emitter-lobe3-cell := read-element( fs );

    gene.gene-neuro-emitter-sample-rate := read-element( fs );

    gene.gene-neuro-emitter-chemical1 := read-element( fs );
    gene.gene-neuro-emitter-chemical1-amount := read-element( fs );
    gene.gene-neuro-emitter-chemical2 := read-element( fs );
    gene.gene-neuro-emitter-chemical2-amount := read-element( fs );
    gene.gene-neuro-emitter-chemical3 := read-element( fs );
    gene.gene-neuro-emitter-chemical3-amount := read-element( fs );
    gene.gene-neuro-emitter-chemical4 := read-element( fs );
    gene.gene-neuro-emitter-chemical4-amount := read-element( fs );
end method do-read-gene;

define method do-write-gene( version == #"creatures3", fs, gene :: <neuro-emitter-gene> ) => ()
    next-method();

    write-element( fs, gene.gene-neuro-emitter-lobe1 );
    write-element( fs, gene.gene-neuro-emitter-lobe1-cell );
    write-element( fs, gene.gene-neuro-emitter-lobe2 );
    write-element( fs, gene.gene-neuro-emitter-lobe2-cell );
    write-element( fs, gene.gene-neuro-emitter-lobe3 );
    write-element( fs, gene.gene-neuro-emitter-lobe3-cell );

    write-element( fs, gene.gene-neuro-emitter-sample-rate );

    write-element( fs, gene.gene-neuro-emitter-chemical1 );
    write-element( fs, gene.gene-neuro-emitter-chemical1-amount );
    write-element( fs, gene.gene-neuro-emitter-chemical2 );
    write-element( fs, gene.gene-neuro-emitter-chemical2-amount );
    write-element( fs, gene.gene-neuro-emitter-chemical3 );
    write-element( fs, gene.gene-neuro-emitter-chemical3-amount );
    write-element( fs, gene.gene-neuro-emitter-chemical4 );
    write-element( fs, gene.gene-neuro-emitter-chemical4-amount );
end method do-write-gene;

install-gene-class("NeuroEmitter", <neuro-emitter-gene>, 1, 5);
