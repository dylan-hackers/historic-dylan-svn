Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <facial-expression-gene> (<gene>)
    slot gene-expression-number :: <integer> = 0;
    slot gene-expression-weight :: <byte> = 0;
    slot gene-expression-drive1 :: <byte> = 0;
    slot gene-expression-drive1-amount :: <byte> = 0;
    slot gene-expression-drive2 :: <byte> = 0;
    slot gene-expression-drive2-amount :: <byte> = 0;
    slot gene-expression-drive3 :: <byte> = 0;
    slot gene-expression-drive3-amount :: <byte> = 0;
    slot gene-expression-drive4 :: <byte> = 0;
    slot gene-expression-drive4-amount :: <byte> = 0;
end class <facial-expression-gene>;

/* Implement gene protocol */
define method create-gene ( version == #"creatures3", type == 2, subtype == 8 ) => (gene :: <gene>)
    make( <facial-expression-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <facial-expression-gene> ) => (type :: <byte>)
    2;
end method gene-type;

define method gene-subtype ( gene :: <facial-expression-gene> ) => (subtype :: <byte>)
    8;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <facial-expression-gene> ) => (name :: <string>)
    "Facial Expression";
end method gene-type-name;

define method read-16-bits( fs, #key endian = #"little" )
  let total-size1 = read-element( fs );
  let total-size2 = read-element( fs );
  if(endian == #"little")
    total-size1 + total-size2 * 256;
  else
    total-size2 + total-size1 * 256;
  end;    
end method read-16-bits;

define method do-read-gene( version == #"creatures3", fs, gene :: <facial-expression-gene> ) => ()
    next-method();

    gene.gene-expression-number := read-16-bits( fs );
    gene.gene-expression-weight := read-element( fs );
    gene.gene-expression-drive1 := read-element( fs );
    gene.gene-expression-drive1-amount := read-element( fs );
    gene.gene-expression-drive2 := read-element( fs );
    gene.gene-expression-drive2-amount := read-element( fs );
    gene.gene-expression-drive3 := read-element( fs );
    gene.gene-expression-drive3-amount := read-element( fs );
    gene.gene-expression-drive4 := read-element( fs );
    gene.gene-expression-drive4-amount := read-element( fs );
end method do-read-gene;

define function write-16-bits( fs, number, #key endian = #"little" )
    let (byte2, byte1) = truncate/( number, 256 );
    if(endian = #"little")
      write-element( fs, byte1 );
      write-element( fs, byte2 );
    else
      write-element( fs, byte2 );
      write-element( fs, byte1 );
    end;
end function write-16-bits;

define method do-write-gene( version == #"creatures3", fs, gene :: <facial-expression-gene> ) => ()
    next-method();

    write-16-bits( fs, gene.gene-expression-number );
    write-element( fs, gene.gene-expression-weight );
    write-element( fs, gene.gene-expression-drive1 );
    write-element( fs, gene.gene-expression-drive1-amount );
    write-element( fs, gene.gene-expression-drive2 );
    write-element( fs, gene.gene-expression-drive2-amount );
    write-element( fs, gene.gene-expression-drive3 );
    write-element( fs, gene.gene-expression-drive3-amount );
    write-element( fs, gene.gene-expression-drive4 );
    write-element( fs, gene.gene-expression-drive4-amount );
end method do-write-gene;

install-gene-class("Facial Expression", <facial-expression-gene>, 2, 8);
