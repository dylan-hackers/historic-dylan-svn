Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <unknown-gene> (<gene>)
    constant slot %gene-type :: <byte>, required-init-keyword: type:;
    constant slot %gene-subtype :: <byte>, required-init-keyword: subtype:;
    constant slot gene-unknown-data = make(<stretchy-vector>);
end class <unknown-gene>;

/* Implement gene protocol */
define method create-gene ( version, type :: <byte>, subtype :: <byte> ) => (gene :: <gene>)
    make( <unknown-gene>, type: type, subtype: subtype, version: version );
end method create-gene;

define method gene-type ( gene :: <unknown-gene> ) => (type :: <byte>)
    gene.%gene-type;
end method gene-type;

define method gene-subtype ( gene :: <unknown-gene> ) => (subtype :: <byte>)
    gene.%gene-subtype;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <unknown-gene> ) => (name :: <string>)
    format-to-string("%d - %d", 
      gene.gene-type,
      gene.gene-subtype);
end method gene-type-name;

define method gene-short-description( gene :: <unknown-gene> ) => (name :: <string>)
    format-to-string("Unknown gene %d - %d", 
      gene.gene-type,
      gene.gene-subtype);
end method gene-short-description;

define method do-read-gene( version, fs, gene :: <unknown-gene> ) => ()
    next-method();
    let marker = read( fs, 4);
    while(as(<string>, marker) ~= "gene" & as(<string>, marker) ~= "gend")
      add! ( gene.gene-unknown-data, first(marker) );
      adjust-stream-position(fs, -3);
      marker := read( fs, 4);
    end while;
    adjust-stream-position(fs, -4);
end method do-read-gene;

define method do-write-gene( version, fs, gene :: <unknown-gene> ) => ()
    next-method();
    write( fs, gene.gene-unknown-data );
end method do-write-gene;



