Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <receptor-gene> (<gene>)
    slot gene-receptor-organ :: <byte> = 0;
    slot gene-receptor-tissue :: <byte> = 0;
    slot gene-receptor-locus :: <byte> = 0;
    slot gene-receptor-chemical :: <byte> = 0;
    slot gene-receptor-threshold :: <byte> = 0;
    slot gene-receptor-nominal :: <byte> = 0;
    slot gene-receptor-gain :: <byte> = 0;
    slot gene-receptor-is-digital? :: <boolean> = #f;
    slot gene-receptor-is-inverted? :: <boolean> = #f;
    slot gene-receptor-other-flags :: <byte> = 0;
    constant virtual slot gene-receptor-organ-description :: <string>;
    constant virtual slot gene-receptor-tissue-description :: <string>;
    constant virtual slot gene-receptor-locus-description :: <string>;
end class <receptor-gene>;


/* Implement gene protocol */
define method create-gene ( version, type == 1, subtype == 0 ) => (gene :: <gene>)
    make( <receptor-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <receptor-gene> ) => (type :: <byte>)
    1;
end method gene-type;

define method gene-subtype ( gene :: <receptor-gene> ) => (subtype :: <byte>)
    0;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <receptor-gene> ) => (name :: <string>)
    "Receptor";
end method gene-type-name;

define method gene-receptor-organ-description( gene :: <receptor-gene> ) => (result :: <string>)
    get-receptor-organ-description( gene.gene-genome-version, gene.gene-receptor-organ );
end method gene-receptor-organ-description;

define method gene-receptor-tissue-description( gene :: <receptor-gene> ) => (result :: <string>)
    get-receptor-tissue-description( gene.gene-genome-version, gene.gene-receptor-organ, gene.gene-receptor-tissue );
end method gene-receptor-tissue-description;

define method gene-receptor-locus-description( gene :: <receptor-gene> ) => (result :: <string>)
    get-receptor-locus-description( gene.gene-genome-version, gene.gene-receptor-organ, gene.gene-receptor-tissue, gene.gene-receptor-locus );
end method gene-receptor-locus-description;

define method gene-receptor-attachment-description( gene :: <receptor-gene> ) => (organ :: <string>, tissue :: <string>, locus :: <string>)
    get-receptor-attachment-description( gene.gene-genome-version, gene.gene-receptor-organ, gene.gene-receptor-tissue, gene.gene-receptor-locus );
end method gene-receptor-attachment-description;

define method do-read-gene( version, fs, gene :: <receptor-gene> ) => ()
    next-method();

    gene.gene-receptor-organ := read-element( fs );
    gene.gene-receptor-tissue := read-element( fs );
    gene.gene-receptor-locus := read-element( fs );
    gene.gene-receptor-chemical := read-element( fs );
    gene.gene-receptor-threshold := read-element( fs );
    gene.gene-receptor-nominal := read-element( fs );
    gene.gene-receptor-gain := read-element( fs );

    let flags = read-element( fs );
    gene.gene-receptor-is-digital? := logbit?(1, flags);
    gene.gene-receptor-is-inverted? := logbit?(0, flags);
    gene.gene-receptor-other-flags := logand(252, flags);
end method do-read-gene;

define method do-write-gene( version, fs, gene :: <receptor-gene> ) => ()
    next-method();

    write-element( fs, gene.gene-receptor-organ );
    write-element( fs, gene.gene-receptor-tissue );
    write-element( fs, gene.gene-receptor-locus );
    write-element( fs, gene.gene-receptor-chemical );
    write-element( fs, gene.gene-receptor-threshold );
    write-element( fs, gene.gene-receptor-nominal );
    write-element( fs, gene.gene-receptor-gain );

    write-element( fs, logior( if(gene.gene-receptor-is-digital?)  #b00000010 else 0 end if,
                               if(gene.gene-receptor-is-inverted?) #b00000001 else 0 end if,
                               gene.gene-receptor-other-flags)); 
end method do-write-gene;

install-gene-class("Receptor", <receptor-gene>, 1, 0);
