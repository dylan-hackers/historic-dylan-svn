Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <genus-gene> (<gene>)
    slot gene-genus-species :: <genus> = #"norn";
end class <genus-gene>;

define class <c2-genus-gene> (<genus-gene>)
    slot gene-genus-mum :: <string> = "toby";
    slot gene-genus-dad :: <string> = "sjgl";
end class <c2-genus-gene>;

define class <c3-genus-gene> (<genus-gene>)
  slot gene-genus-data = #f;
end class <c3-genus-gene>;

// Implement gene protocol 
define method create-gene( version == #"creatures2", type == 2, subtype == 1 ) => (gene :: <gene>)
  make(<c2-genus-gene>, version: version);
end method create-gene;

define method create-gene( version == #"creatures3", type == 2, subtype == 1 ) => (gene :: <gene>)
  make(<c3-genus-gene>, version: version);
end method create-gene;

define method gene-type ( gene :: <genus-gene> ) => (type :: <byte>)
    2;
end method gene-type;

define method gene-subtype ( gene :: <genus-gene> ) => (subtype :: <byte>)
    1;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <genus-gene> ) => (name :: <string>)
    "Genus";
end method gene-type-name;

define method gene-short-description( gene :: <genus-gene> ) => (name :: <string>)
    format-to-string(
      "Genus gene %s", 
      convert-to-description( <genus>, gene.gene-genus-species ) );
end method gene-short-description;

define method do-read-gene( version, fs, gene :: <genus-gene> ) => ()
    next-method();
    gene.gene-genus-species := convert-to-genus( read-element( fs ) );
end method do-read-gene;

define method do-read-gene( version == #"creatures2", fs, gene :: <genus-gene> ) => ()
    next-method();
    gene.gene-genus-mum := as(<string>, read( fs, 4));
    gene.gene-genus-dad := as(<string>, read( fs, 4));
end method do-read-gene;

define method do-read-gene( version == #"creatures3", fs, gene :: <genus-gene> ) => ()
    next-method();
    gene.gene-genus-data := read( fs, 64);
end method do-read-gene;

define method do-write-gene( version, fs, gene :: <genus-gene> ) => ()
    next-method();
    write-element( fs, convert-to-value( <genus>, gene.gene-genus-species ) );
end method do-write-gene;

define method do-write-gene( version == #"creatures2", fs, gene :: <genus-gene> ) => ()
    next-method();
    write( fs, gene.gene-genus-mum );
    write( fs, gene.gene-genus-dad );
end method do-write-gene;

define method do-write-gene( version == #"creatures3", fs, gene :: <genus-gene> ) => ()
    next-method();
    write( fs, gene.gene-genus-data );
end method do-write-gene;

install-gene-class("Genus", <genus-gene>, 2, 1);

