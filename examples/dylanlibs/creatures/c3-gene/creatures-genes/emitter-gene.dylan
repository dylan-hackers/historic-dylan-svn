Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <emitter-gene> (<gene>)
    slot gene-emitter-organ :: <byte> = 0;
    slot gene-emitter-tissue :: <byte> = 0;
    slot gene-emitter-locus :: <byte> = 0;
    slot gene-emitter-chemical :: <byte> = 0;
    slot gene-emitter-threshold :: <byte> = 0;
    slot gene-emitter-sample-rate :: <byte> = 0;
    slot gene-emitter-gain :: <byte> = 0;
    slot gene-emitter-is-digital? :: <boolean> = #f;
    slot gene-emitter-clear-source? :: <boolean> = #f;
    slot gene-emitter-is-inverted? :: <boolean> = #f;
    slot gene-emitter-other-flags :: <byte> = 0;
    constant virtual slot gene-emitter-organ-description :: <string>;
    constant virtual slot gene-emitter-tissue-description :: <string>;
    constant virtual slot gene-emitter-locus-description :: <string>;
end class <emitter-gene>;

/* Implement gene protocol */
define method create-gene ( version, type == 1, subtype == 1 ) => (gene :: <gene>)
    make( <emitter-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <emitter-gene> ) => (type :: <byte>)
    1;
end method gene-type;

define method gene-subtype ( gene :: <emitter-gene> ) => (subtype :: <byte>)
    1;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <emitter-gene> ) => (name :: <string>)
    "Emitter";
end method gene-type-name;

define method gene-emitter-organ-description( gene :: <emitter-gene> ) => (result :: <string>)
    get-emitter-organ-description( gene.gene-genome-version, gene.gene-emitter-organ );
end method gene-emitter-organ-description;

define method gene-emitter-tissue-description( gene :: <emitter-gene> ) => (result :: <string>)
    get-emitter-tissue-description( gene.gene-genome-version, gene.gene-emitter-organ, gene.gene-emitter-tissue );
end method gene-emitter-tissue-description;

define method gene-emitter-locus-description( gene :: <emitter-gene> ) => (result :: <string>)
    get-emitter-locus-description( gene.gene-genome-version, gene.gene-emitter-organ, gene.gene-emitter-tissue, gene.gene-emitter-locus );
end method gene-emitter-locus-description;

define method gene-emitter-attachment-description( gene :: <emitter-gene> ) => 
    ( organ :: <string>, tissue :: <string>, locus :: <string>)
    get-emitter-attachment-description( gene.gene-genome-version, gene.gene-emitter-organ, gene.gene-emitter-tissue, gene.gene-emitter-locus );
end method gene-emitter-attachment-description;

define method do-read-gene( version, fs, gene :: <emitter-gene> ) => ()
    next-method();
    gene.gene-emitter-organ := read-element( fs ) ;
    gene.gene-emitter-tissue := read-element( fs );
    gene.gene-emitter-locus := read-element( fs );

    gene.gene-emitter-chemical := read-element( fs );
    gene.gene-emitter-threshold := read-element( fs );
    gene.gene-emitter-sample-rate := read-element( fs );
    gene.gene-emitter-gain := read-element( fs );

    let flags = read-element( fs );
    gene.gene-emitter-is-digital? := logbit?(1, flags);
    gene.gene-emitter-clear-source? := logbit?(0, flags);
    gene.gene-emitter-is-inverted? := logbit?(2, flags);
    gene.gene-emitter-other-flags := logand(248, flags);
end method do-read-gene;

define method do-write-gene( version, fs, gene :: <emitter-gene> ) => ()
    next-method();
    write-element( fs, gene.gene-emitter-organ );
    write-element( fs, gene.gene-emitter-tissue );
    write-element( fs, gene.gene-emitter-locus );

    write-element( fs, gene.gene-emitter-chemical );
    write-element( fs, gene.gene-emitter-threshold );
    write-element( fs, gene.gene-emitter-sample-rate );
    write-element( fs, gene.gene-emitter-gain );

    write-element( fs, logior( if(gene.gene-emitter-is-digital?)   #b00000010 else 0 end if,
                               if(gene.gene-emitter-clear-source?) #b00000001 else 0 end if,
                               if(gene.gene-emitter-is-inverted?)   #b00000100 else 0 end if,
                               gene.gene-emitter-other-flags)); 
end method do-write-gene;

install-gene-class("Emitter", <emitter-gene>, 1, 1);
