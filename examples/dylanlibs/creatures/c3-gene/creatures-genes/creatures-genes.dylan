Module:    creatures-genes
Synopsis:  Classes for reading Creatures genetic files
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <gene> (<object>)
    slot gene-genome-version = #f, init-keyword: version:;
    slot gene-index :: <integer> = 0, init-keyword: index:;
    constant virtual slot gene-type;
    constant virtual slot gene-subtype;
    slot gene-sequence-number :: <integer> = 0;
    slot gene-duplicate-number :: <integer> = 0;
    slot gene-switch-on-stage :: <switch-on-stage> = #"embryo";
    slot gene-allow-duplicates? :: <boolean> = #t;
    slot gene-allow-mutations? :: <boolean> = #t;
    slot gene-allow-deletions? :: <boolean> = #t;
    slot gene-is-dormant? :: <boolean> = #f;
    slot gene-switch-on-gender :: <gender> = #"both";
    slot gene-mutation-percentage :: <integer> = 128;
    slot gene-other-flags :: <integer> = 0;
    slot gene-variant :: <integer> = 0;
    constant virtual slot gene-type-name :: <string>;
    constant virtual slot gene-short-description :: <string>;
end class <gene>;

// Add a method to this generic to create the actual type of gene
define generic create-gene ( version, type :: <byte>, subtype :: <byte> ) => (gene :: <gene>);

// Add a method to this generic to read the gene using the given file stream.
// Make sure you call next-method() to populate the header information.
define generic read-gene( fs, gene :: <gene> ) => ();       
define generic do-read-gene( version, fs, gene :: <gene> ) => ();
define generic write-gene( fs, gene :: <gene> ) => ();
define generic do-write-gene( version, fs, gene :: <gene> ) => ();


define method gene-short-description( gene :: <gene> ) => (name :: <string>)
    gene.gene-type-name;
end method gene-short-description;

define method read-gene( fs, gene :: <gene> ) => ()
  do-read-gene( gene.gene-genome-version, fs, gene );
end method read-gene;

define method do-read-gene( version, fs, gene :: <gene> ) => ()
    gene.gene-sequence-number := read-element( fs );
    gene.gene-duplicate-number := read-element( fs );
    gene.gene-switch-on-stage := convert-to-switch-on-stage( read-element( fs ) );

    let flags = read-element( fs );
    gene.gene-allow-duplicates? := logbit?(1, flags);
    gene.gene-allow-mutations? := logbit?(0, flags);
    gene.gene-allow-deletions? := logbit?(2, flags);
    gene.gene-is-dormant? := logbit?(5, flags);
    gene.gene-switch-on-gender := convert-to-gender( flags );
    gene.gene-other-flags := logand(192, flags);

    gene.gene-mutation-percentage := read-element( fs );
    if(version = #"creatures3")
      gene.gene-variant := read-element( fs );
    end if;

end method do-read-gene;

define method write-gene( fs, gene :: <gene> ) => ()
  do-write-gene( gene.gene-genome-version, fs, gene);
end method write-gene;

define method do-write-gene( version, fs, gene :: <gene> ) => ()
    write-element( fs, gene.gene-type );
    write-element( fs, gene.gene-subtype );
    write-element( fs, gene.gene-sequence-number );
    write-element( fs, gene.gene-duplicate-number );
    write-element( fs, convert-to-value( <switch-on-stage>, gene.gene-switch-on-stage ) );

    let bit = logior(if(gene.gene-allow-duplicates?) #b00000010 else 0 end if,
                     if(gene.gene-allow-deletions?)  #b00000100 else 0 end if,
                     if(gene.gene-allow-mutations?)  #b00000001 else 0 end if,
                     if(gene.gene-is-dormant?)       #b00100000 else 0 end if,
                     convert-to-value( <gender>, gene.gene-switch-on-gender ),
                     gene.gene-other-flags);
    write-element( fs, bit );
    write-element( fs, gene.gene-mutation-percentage );
    if(version == #"creatures3")
      write-element( fs, gene.gene-variant );
    end if;

end method do-write-gene;


// Currently used genome versions are:
//   #"creatures2"
//   #"creatures3"
//   #"unknown"
define method load-genome( filename )
/* Load a genome from a file. A genome in this case is considered 
   to be a sequence of genes. 
*/
    let genome = make(<stretchy-vector>);
    let success? = with-open-file ( fs = filename, element-type: <byte> )
      let version-marker = as(<string>, read( fs, 4));
      let version = if(version-marker = "dna2")
          #"creatures2"
        elseif( version-marker = "dna3" )
          #"creatures3"
        else
          #"unknown"
        end if;

      let index = 0;
      let gene-marker :: <string> = as(<string>, read(fs, 4));
      while(gene-marker = "gene")
        let type = read-element( fs );
        let subtype = read-element( fs );

        let gene = create-gene( version, type, subtype );
        gene.gene-index := index;
        index := index + 1;
        read-gene ( fs, gene );

        add!( genome, gene );    
        gene-marker := as(<string>, read(fs, 4));
      end while;
      gene-marker = "gend";
    end;
    values(genome, success?);
end method load-genome;

define method save-genome( genome , filename )
    if(genome.size > 0)
      with-open-file ( fs = filename, element-type: <byte>, direction: #"output" )
        if(genome[0].gene-genome-version = #"creatures2")
          write( fs, "dna2");
        elseif(genome[0].gene-genome-version = #"creatures3")
          write( fs, "dna3");
        end if;
      
        for(gene in genome)
          write( fs, "gene" );        
          write-gene( fs, gene );
        end for;

        write(fs, "gend");
      end;
    end if;
end method save-genome;

define method make-genome()
    let genome = make(<stretchy-vector>);
    add!(genome, make(<genus-gene>));
    add!(genome, make(<brain-lobe-gene>));
    add!(genome, make(<brain-lobe-gene>));
    add!(genome, make(<brain-lobe-gene>));
    add!(genome, make(<brain-lobe-gene>));
    add!(genome, make(<brain-lobe-gene>));
    add!(genome, make(<brain-lobe-gene>));
    add!(genome, make(<brain-lobe-gene>));
    add!(genome, make(<brain-lobe-gene>));
    genome;
end method make-genome;

define class <genome-item> (<object>)
    slot item-index, required-init-keyword: index:;
    slot item-gene, required-init-keyword: gene:;
    slot item-gno = #f, init-keyword: gno:;
    slot item-extra-gno = #f, init-keyword: extra-gno:;
end class <genome-item>;

