Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define constant <svrule> = limited(<vector>, of: <svrule-opcode>);
define function make-blank-svrule() => (svrule :: <svrule>)
    make(<svrule>, size: 12, fill: #"<end>" );
end function make-blank-svrule;
define class <brain-lobe-dendrite> (<object>)
    slot dendrite-source-lobe :: <byte> = 0;
    slot dendrite-min :: <byte> = 0;
    slot dendrite-max :: <byte> = 0;
    slot dendrite-spread :: <byte> = 0;
    slot dendrite-fanout :: <byte> = 0;
    slot dendrite-min-ltw :: <byte> = 0;
    slot dendrite-max-ltw :: <byte> = 0;
    slot dendrite-min-strength :: <byte> = 0;
    slot dendrite-max-strength :: <byte> = 0;
    slot dendrite-migrate-flag :: <byte> = 0;
    slot dendrite-relax-suscept :: <byte> = 0;
    slot dendrite-relax-stw :: <byte> = 0;
    slot dendrite-ltw-gain-rate :: <byte> = 0;
    slot dendrite-strength-gain :: <byte> = 0;
    slot dendrite-strength-gain-rule :: <svrule> = make-blank-svrule();
    slot dendrite-strength-loss :: <byte> = 0;
    slot dendrite-strength-loss-rule :: <svrule> = make-blank-svrule();
    slot dendrite-strength-suscept-rule :: <svrule> = make-blank-svrule();
    slot dendrite-strength-backprop-rule :: <svrule> = make-blank-svrule();
    slot dendrite-strength-forwardprop-rule :: <svrule> = make-blank-svrule();
    slot dendrite-strength-relax-rule :: <svrule> = make-blank-svrule();
end class <brain-lobe-dendrite>;

define class <brain-lobe-gene> (<gene>)
end class <brain-lobe-gene>;

define class <c2-brain-lobe-gene> (<brain-lobe-gene>)
    slot gene-brain-lobe-x-position :: <byte> = 0;
    slot gene-brain-lobe-y-position :: <byte> = 0;
    slot gene-brain-lobe-width :: <byte> = 0;
    slot gene-brain-lobe-height :: <byte> = 0;
    slot gene-brain-lobe-perception-flag :: <byte> = 0;
    slot gene-brain-lobe-nominal-threshold :: <byte> = 0;
    slot gene-brain-lobe-leakage-rate :: <byte> = 0;
    slot gene-brain-lobe-rest-state :: <byte> = 0;
    slot gene-brain-lobe-input-gain :: <byte> = 0;
    slot gene-brain-lobe-svrule :: <svrule> = make-blank-svrule();
    slot gene-brain-lobe-winner-takes-all? :: <boolean> = #f;
    slot gene-brain-lobe-other-flags :: <byte> = 0;
//    slot gene-brain-lobe-dendrites :: limited(<vector>, of: <brain-lobe-dendrite>) = make(limited(<vector>, of: <brain-lobe-dendrite>), size: 2);
    slot gene-brain-lobe-dendrites :: limited(<vector>, of: false-or(<brain-lobe-dendrite>)) = make(limited(<vector>, of: false-or(<brain-lobe-dendrite>)), size: 2);
end class <c2-brain-lobe-gene>;

define class <c3-brain-lobe-gene> (<brain-lobe-gene>)
  slot gene-brain-lobe-name :: <string>;
  slot gene-brain-lobe-x-position :: <byte> = 0;
  slot gene-brain-lobe-y-position :: <byte> = 0;
  slot gene-brain-lobe-width :: <byte> = 0;
  slot gene-brain-lobe-height :: <byte> = 0;
  slot gene-brain-lobe-unknown = #f;
end class <c3-brain-lobe-gene>;

define method initialize( gene :: <c2-brain-lobe-gene>, #key)
    gene.gene-brain-lobe-dendrites[0] := make(<brain-lobe-dendrite>);
    gene.gene-brain-lobe-dendrites[1] := make(<brain-lobe-dendrite>);
end method initialize;

/* Implement gene protocol */
define method create-gene ( version == #"creatures2", type == 0, subtype == 0 ) => (gene :: <gene>)
    make( <c2-brain-lobe-gene>, version: version );
end method create-gene;

define method create-gene ( version == #"creatures3", type == 0, subtype == 0 ) => (gene :: <gene>)
    make( <c3-brain-lobe-gene>, version: version );
end method create-gene;

define method gene-type ( gene :: <brain-lobe-gene> ) => (type :: <byte>)
    0;
end method gene-type;

define method gene-subtype ( gene :: <brain-lobe-gene> ) => (subtype :: <byte>)
   0;
end method gene-subtype;

// Returns a textual description of the type of the gene.
define method gene-type-name( gene :: <brain-lobe-gene> ) => (name :: <string>)
    "Brain Lobe";
end method gene-type-name;

define method do-read-gene( version, fs, gene :: <c3-brain-lobe-gene> ) => ()
  next-method();

  gene.gene-brain-lobe-name := as(<string>, read( fs, 4));

  gene.gene-brain-lobe-unknown := read( fs, #x75 );
  let data = gene.gene-brain-lobe-unknown;
  gene.gene-brain-lobe-x-position := data[3] + 256 * data[2];
  gene.gene-brain-lobe-y-position := data[5] + 256 * data[4];
  gene.gene-brain-lobe-width := data[6];
  gene.gene-brain-lobe-height := data[7];
end method do-read-gene;

define method do-read-gene( version, fs, gene :: <c2-brain-lobe-gene> ) => ()
    local method read-svrule-into( fs, collection :: <svrule> )
        read-into!( fs, collection.size, collection );
        map-into( collection, convert-to-svrule-opcode, collection );
    end method read-svrule-into;

    next-method();

    gene.gene-brain-lobe-x-position := read-element( fs );
    gene.gene-brain-lobe-y-position := read-element( fs );
    gene.gene-brain-lobe-width := read-element( fs );
    gene.gene-brain-lobe-height := read-element( fs );
    gene.gene-brain-lobe-perception-flag := read-element( fs );
    gene.gene-brain-lobe-nominal-threshold := read-element( fs );
    gene.gene-brain-lobe-leakage-rate := read-element( fs );
    gene.gene-brain-lobe-rest-state := read-element( fs );
    gene.gene-brain-lobe-input-gain := read-element( fs );
    read-svrule-into( fs, gene.gene-brain-lobe-svrule );
    let flags = read-element( fs );
    gene.gene-brain-lobe-winner-takes-all? := logbit?(0, flags);
    gene.gene-brain-lobe-other-flags := logand(254, flags);

    for(dendrite in gene.gene-brain-lobe-dendrites)
      dendrite.dendrite-source-lobe := read-element( fs );
      dendrite.dendrite-min := read-element( fs );
      dendrite.dendrite-max := read-element( fs );
      dendrite.dendrite-spread := read-element( fs );
      dendrite.dendrite-fanout := read-element( fs );
      dendrite.dendrite-min-ltw := read-element( fs );
      dendrite.dendrite-max-ltw := read-element( fs );
      dendrite.dendrite-min-strength := read-element( fs );
      dendrite.dendrite-max-strength := read-element( fs );
      dendrite.dendrite-migrate-flag := read-element( fs );
      dendrite.dendrite-relax-suscept := read-element( fs );
      dendrite.dendrite-relax-stw := read-element( fs );
      dendrite.dendrite-ltw-gain-rate := read-element( fs );
      dendrite.dendrite-strength-gain := read-element( fs );
      read-svrule-into( fs, dendrite.dendrite-strength-gain-rule );
      dendrite.dendrite-strength-loss := read-element( fs );
      read-svrule-into( fs, dendrite.dendrite-strength-loss-rule );
      read-svrule-into( fs, dendrite.dendrite-strength-suscept-rule );
      read-svrule-into( fs, dendrite.dendrite-strength-relax-rule );
      read-svrule-into( fs, dendrite.dendrite-strength-backprop-rule );
      read-svrule-into( fs, dendrite.dendrite-strength-forwardprop-rule );
    end for;

end method do-read-gene;

define method do-write-gene( version, fs, gene :: <c3-brain-lobe-gene> ) => ()
  next-method();

  write(fs, gene.gene-brain-lobe-name );
  write(fs, gene.gene-brain-lobe-unknown );

end method do-write-gene;

define method do-write-gene( version, fs, gene :: <c2-brain-lobe-gene> ) => ()
    local method write-svrule( fs, collection :: <svrule> )
        write( fs, map( curry(convert-to-value, <svrule-opcode>) , collection) );
    end method write-svrule;


    next-method();

    write-element( fs, gene.gene-brain-lobe-x-position );
    write-element( fs, gene.gene-brain-lobe-y-position );
    write-element( fs, gene.gene-brain-lobe-width );
    write-element( fs, gene.gene-brain-lobe-height );
    write-element( fs, gene.gene-brain-lobe-perception-flag );
    write-element( fs, gene.gene-brain-lobe-nominal-threshold );
    write-element( fs, gene.gene-brain-lobe-leakage-rate );
    write-element( fs, gene.gene-brain-lobe-rest-state );
    write-element( fs, gene.gene-brain-lobe-input-gain );
    write-svrule( fs, gene.gene-brain-lobe-svrule );

    write-element( fs, logior( if(gene.gene-brain-lobe-winner-takes-all?) #b00000001 else 0 end if,
                               gene.gene-brain-lobe-other-flags ) );

    for(dendrite in gene.gene-brain-lobe-dendrites)
      write-element( fs, dendrite.dendrite-source-lobe );
      write-element( fs, dendrite.dendrite-min );
      write-element( fs, dendrite.dendrite-max );
      write-element( fs, dendrite.dendrite-spread );
      write-element( fs, dendrite.dendrite-fanout );
      write-element( fs, dendrite.dendrite-min-ltw );
      write-element( fs, dendrite.dendrite-max-ltw );
      write-element( fs, dendrite.dendrite-min-strength );
      write-element( fs, dendrite.dendrite-max-strength );
      write-element( fs, dendrite.dendrite-migrate-flag );
      write-element( fs, dendrite.dendrite-relax-suscept );
      write-element( fs, dendrite.dendrite-relax-stw );
      write-element( fs, dendrite.dendrite-ltw-gain-rate );
      write-element( fs, dendrite.dendrite-strength-gain );
      write-svrule( fs, dendrite.dendrite-strength-gain-rule );
      write-element( fs, dendrite.dendrite-strength-loss );
      write-svrule( fs, dendrite.dendrite-strength-loss-rule );
      write-svrule( fs, dendrite.dendrite-strength-suscept-rule );
      write-svrule( fs, dendrite.dendrite-strength-relax-rule );
      write-svrule( fs, dendrite.dendrite-strength-backprop-rule );
      write-svrule( fs, dendrite.dendrite-strength-forwardprop-rule );      
    end for;

end method do-write-gene;

install-gene-class("Brain Lobe", <brain-lobe-gene>, 0, 0);
