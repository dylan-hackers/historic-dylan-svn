Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

// Note that it derives from the organ gene...
define class <brain-organ-gene> (<organ-gene>)
end class <brain-organ-gene>;

/* Implement gene protocol */
define method create-gene ( version, type == 0, subtype == 1 ) => (gene :: <gene>)
    make( <brain-organ-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <brain-organ-gene> ) => (type :: <byte>)
    0;
end method gene-type;

define method gene-subtype ( gene :: <brain-organ-gene> ) => (subtype :: <byte>)
   1;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <brain-organ-gene> ) => (name :: <string>)
    "Brain Organ";
end method gene-type-name;

install-gene-class("Brain Organ", <brain-organ-gene>, 0, 1);
