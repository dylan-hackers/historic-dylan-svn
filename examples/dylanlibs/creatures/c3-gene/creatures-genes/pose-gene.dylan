Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <pose-gene> (<gene>)
    slot gene-pose :: <byte> = 0;
    slot gene-pose-data :: limited(<vector>, of: <byte>);
end class <pose-gene>;

define method initialize( gene :: <pose-gene>, #key)
  next-method();

  let pose-size = if(gene.gene-genome-version = #"creatures3")
                    16
                  else
                    15
                  end if;
  gene.gene-pose-data :=  make(limited(<vector>, of: <byte>), size: pose-size);
end method initialize;

/* Implement gene protocol */
define method create-gene ( version, type == 2, subtype == 3 ) => (gene :: <gene>)
    make( <pose-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <pose-gene> ) => (type :: <byte>)
    2;
end method gene-type;

define method gene-subtype ( gene :: <pose-gene> ) => (subtype :: <byte>)
    3;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <pose-gene> ) => (name :: <string>)
    "Pose";
end method gene-type-name;

define method do-read-gene( version, fs, gene :: <pose-gene> ) => ()
    next-method();
    
    gene.gene-pose := read-element( fs );
    read-into!( fs, gene.gene-pose-data.size, gene.gene-pose-data );

end method do-read-gene;

define method do-write-gene( version, fs, gene :: <pose-gene> ) => ()
    next-method();
    write-element( fs, gene.gene-pose );
    write( fs, gene.gene-pose-data );
end method do-write-gene;

install-gene-class("Pose", <pose-gene>, 2, 3);

