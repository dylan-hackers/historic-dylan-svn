Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <gait-gene> (<gene>)
    slot gene-gait :: <gait> = #"normal";
    slot gene-gait-data :: limited(<vector>, of: <byte>) = make(limited(<vector>, of: <byte>), size: 8);
end class <gait-gene>;

/* Implement gene protocol */
define method create-gene ( version, type == 2, subtype == 4 ) => (gene :: <gene>)
    make( <gait-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <gait-gene> ) => (type :: <byte>)
    2;
end method gene-type;

define method gene-subtype ( gene :: <gait-gene> ) => (subtype :: <byte>)
    4;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <gait-gene> ) => (name :: <string>)
    "Gait";
end method gene-type-name;

define method gene-short-description( gene :: <gait-gene> ) => (name :: <string>)
    format-to-string(
      "Gait gene %s", 
      convert-to-description( <gait>, gene.gene-gait ) );
end method gene-short-description;

define method do-read-gene( version, fs, gene :: <gait-gene> ) => ()
    next-method();
    
    gene.gene-gait := convert-to-gait( read-element( fs ) );
    read-into!( fs, gene.gene-gait-data.size, gene.gene-gait-data );

end method do-read-gene;

define method do-write-gene( version, fs, gene :: <gait-gene> ) => ()
    next-method();
    write-element( fs, convert-to-value( <gait>, gene.gene-gait ) );
    write( fs, gene.gene-gait-data );
end method do-write-gene;

install-gene-class("Gait", <gait-gene>, 2, 4);
