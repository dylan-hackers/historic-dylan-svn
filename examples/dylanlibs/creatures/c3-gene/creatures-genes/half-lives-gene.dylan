Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <half-lives-gene> (<gene>)
    constant slot gene-half-lives :: limited(<vector>, of: <byte>) = make(limited(<vector>, of: <byte>), size: 256);
end class <half-lives-gene>;

/* Implement gene protocol */
define method create-gene ( version, type == 1, subtype == 3 ) => (gene :: <gene>)
    make( <half-lives-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <half-lives-gene> ) => (type :: <byte>)
    1;
end method gene-type;

define method gene-subtype ( gene :: <half-lives-gene> ) => (subtype :: <byte>)
    3;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <half-lives-gene> ) => (name :: <string>)
    "Half-Lives";
end method gene-type-name;

define method do-read-gene( version, fs, gene :: <half-lives-gene> ) => ()
    next-method();
    
    read-into!( fs, gene.gene-half-lives.size, gene.gene-half-lives );
end method do-read-gene;

define method do-write-gene( version, fs, gene :: <half-lives-gene> ) => ()
    next-method();
    write( fs, gene.gene-half-lives );
end method do-write-gene;

install-gene-class("Half Lives", <half-lives-gene>, 1, 3);
