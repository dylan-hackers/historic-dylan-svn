Module:    gene-compare
Synopsis:  A program to compare Creature gene files.
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define command-table *file-command-table* (*global-command-table*)
  menu-item "Open First Genome..." = rcurry(on-genome-open, 0),
    documentation: "Open the first .gen file.";
  menu-item "Compare With Second Genome..." = rcurry(on-genome-open, 1),
    documentation: "Open the second .gen file.";
  menu-item "Toggle Details..." = on-toggle-details,
    documentation: "Toggle details on and off.";
  separator;
  menu-item "Exit" = exit-frame,
    documentation: "Exit gene-compare.";
end command-table;

define command-table *help-command-table* (*global-command-table*)
  menu-item "About..." = <help-on-version>,
    documentation: "Display program information, version number and copyright.";
end command-table;

define command-table *gene-compare-command-table* (*global-command-table*)
  menu-item "File" = *file-command-table*;
  menu-item "Help" = *help-command-table*;
end;

define frame <gene-compare-frame> (<simple-frame>)
  constant slot document-genomes = make(<vector>, size: 2);
  slot frame-display-details? = #f;

  pane results-pane (frame)
    make(<rich-text-gadget>, auto-scroll?: #t);

  layout (frame)
    vertically()
      frame.results-pane;
    end;

  command-table (frame) *gene-compare-command-table*;
  status-bar (frame) make(<status-bar>);
  keyword title: = "gene-compare";
  keyword width: = 640;
  keyword height: = 400;
end frame <gene-compare-frame>;

define method handle-event( frame :: <gene-compare-frame>, event :: <frame-mapped-event> ) => ()
  let hwnd = frame.results-pane.window-handle;
  with-stack-structure( cf :: <LPCHARFORMAT> )
    cf.cbSize-value := size-of(<CHARFORMAT>);
    cf.dwMask-value := $CFM-FACE;
    cf.szFaceName-value[0] := as(<integer>, 'C');
    cf.szFaceName-value[1] := as(<integer>, 'o');
    cf.szFaceName-value[2] := as(<integer>, 'u');
    cf.szFaceName-value[3] := as(<integer>, 'r');
    cf.szFaceName-value[4] := as(<integer>, 'i');
    cf.szFaceName-value[5] := as(<integer>, 'e');
    cf.szFaceName-value[6] := as(<integer>, 'r');
    cf.szFaceName-value[7] := 0;
    SendMessage(hwnd, $EM-SETCHARFORMAT, $SCF-ALL, pointer-address(cf));
  end;
end method handle-event;  

define constant $genome-file-filters = #[
    #["Genome Files (*.gen)", "*.gen"],
    #["All Files (*.*)", "*.*"]
];


define method load-genome-from-file( filename )
  let (genes, success?) = load-genome( filename );
  values(make(<genome>, genes: genes, filename: filename),
         success?);
end method load-genome-from-file;
  
define method on-genome-open( frame :: <gene-compare-frame>, genome-number )
  let file = choose-file(direction: #"input", owner: frame, filters: $genome-file-filters);
  when(file)
    block()
      let (genome, success?) = load-genome-from-file(file);
      unless(success?)
        notify-user("Warning: Could not load complete genome file.");
      end;  

      frame.document-genomes[genome-number] := genome;
      when(genome-number == 0)
        frame.document-genomes[1] := #f;
      end;
    exception( e :: <condition> )
      notify-user("Error: Could not load genome file.");
    end;
  end;
  update-display( frame );   
end method;

define method do-execute-command( frame :: <gene-compare-frame>, 
    command :: <help-on-version> ) => ()
  notify-user(concatenate("gene-compare V1.0.2\n"    
    "Copyright (c) 1999, Chris Double.\n"
    "All Rights Reserved.\n",
    "Using creatures-genes.dll V", creatures-genes-version(), "\n",
    "Using double-rich-text-gadget.dll V", rich-text-gadget-version(), "\n",                          
    "http://www.double.co.nz/creatures"), owner: frame);
end method;

define method satisfies-count( predicate, collection )
  let count :: <integer> = 0;
  for(item in collection)
    when(predicate(item))
      count := count + 1;
    end;
  finally
    count;
  end;
end;

define method stream-genome-summary( genome :: <genome>, stream :: <stream> )
  local method gene-count( class )
    satisfies-count(rcurry(instance?, class), genome.genome-genes)
  end;

  format(stream, "%s\r\n", 
    if(genome.genome-genes.size > 0)
      select(genome.genome-genes[0].gene-genome-version)
        #"creatures2" => "Version 2 genome";
        #"creatures3" => "Version 3 genome";
        otherwise => "Unknown Version for genome";
      end;
    else
      "Version 3 genome";
    end);
  
  when(genome.genome-filename)
    format(stream, "%s\r\n", genome.genome-filename);
  end when;

  format(stream, "Total number of genes: %d\r\n", genome.genome-genes.size);
  format(stream, "Total number of lobes: %d\r\n", 
    gene-count(<brain-lobe-gene>));
  format(stream, "Total number of brain organs: %d\r\n", 
    gene-count(<brain-organ-gene>));
  format(stream, "Total number of brain tracts: %d\r\n", 
    gene-count(<brain-tract-gene>));
  format(stream, "Total number of receptors: %d\r\n", 
    gene-count(<receptor-gene>));
  format(stream, "Total number of emitters: %d\r\n", 
    gene-count(<emitter-gene>));
  format(stream, "Total number of neuroemitters: %d\r\n", 
    gene-count(<neuro-emitter-gene>));
  format(stream, "Total number of reactions: %d\r\n", 
    gene-count(<reaction-gene>));
  format(stream, "Total number of halflives: %d\r\n", 
    gene-count(<half-lives-gene>));
  format(stream, "Total number of initial concentrations: %d\r\n", 
    gene-count(<initial-concentration-gene>));
  format(stream, "Total number of stimuli: %d\r\n", 
    gene-count(<stimulus-gene>));
  format(stream, "Total number of genus records: %d\r\n", 
    gene-count(<genus-gene>));
  format(stream, "Total number of appearance genes: %d\r\n", 
    gene-count(<appearance-gene>));
  format(stream, "Total number of poses: %d\r\n", 
    gene-count(<pose-gene>));
  format(stream, "Total number of gaits: %d\r\n", 
    gene-count(<gait-gene>));
  format(stream, "Total number of instincts: %d\r\n", 
    gene-count(<instinct-gene>));
  format(stream, "Total number of pigments: %d\r\n", 
    gene-count(<pigment-gene>));
  format(stream, "Total number of pigment bleeds: %d\r\n", 
    gene-count(<pigment-bleed-gene>));
  format(stream, "Total number of facial expressions: %d\r\n", 
    gene-count(<facial-expression-gene>));
  format(stream, "Total number of organs: %d\r\n", 
    gene-count(<organ-gene>) - gene-count(<brain-organ-gene>));
end method stream-genome-summary;

define method stream-genome-details( genome :: <genome>, stream )
  local method gene-sort-method( gene-a :: <gene>, gene-b :: <gene>)
    let value-a = gene-a.gene-index * 10000 + 
      gene-a.gene-sequence-number * 1000 +
      gene-a.gene-duplicate-number * 1000 +
      gene-a.gene-variant * 1000;
    let value-b = gene-b.gene-index * 10000 + 
      gene-b.gene-sequence-number * 1000 +
      gene-b.gene-duplicate-number * 1000 +
      gene-b.gene-variant * 1000;

    value-a < value-b;
  end method;
          
  local method stream-the-gene-type( predicate, description )
    let genes = sort(choose(predicate, genome.genome-genes), test: gene-sort-method);
    format(stream, "***** %s ****\r\n", description);
    for(gene in genes)
      stream-gene( gene, genome, stream );
      format(stream, "\r\n");
    end for;
  end method;

  stream-the-gene-type(rcurry(instance?, <brain-lobe-gene>), "LOBES");
  stream-the-gene-type(rcurry(instance?, <brain-organ-gene>), "BRAIN ORGANS");
  stream-the-gene-type(rcurry(instance?, <brain-tract-gene>), "BRAIN TRACTS");
  stream-the-gene-type(rcurry(instance?, <receptor-gene>), "RECEPTORS");
  stream-the-gene-type(rcurry(instance?, <emitter-gene>), "EMITTERS");
  stream-the-gene-type(rcurry(instance?, <neuro-emitter-gene>), "NEURO EMITTERS");
  stream-the-gene-type(rcurry(instance?, <reaction-gene>), "REACTIONS");
  stream-the-gene-type(rcurry(instance?, <half-lives-gene>), "HALF LIVES");
  stream-the-gene-type(rcurry(instance?, <initial-concentration-gene>), "INITIAL CONCENTRATIONS");
  stream-the-gene-type(rcurry(instance?, <stimulus-gene>), "STIMULI");
  stream-the-gene-type(rcurry(instance?, <genus-gene>), "GENUS RECORDS");
  stream-the-gene-type(rcurry(instance?, <appearance-gene>), "APPEARANCES");
  stream-the-gene-type(rcurry(instance?, <pose-gene>), "POSES");
  stream-the-gene-type(rcurry(instance?, <gait-gene>), "GAITS");
  stream-the-gene-type(rcurry(instance?, <instinct-gene>), "INSTINCTS");
  stream-the-gene-type(rcurry(instance?, <pigment-gene>), "PIGMENTS");
  stream-the-gene-type(rcurry(instance?, <pigment-bleed-gene>), "PIGMENT BLEEDS");
  stream-the-gene-type(rcurry(instance?, <facial-expression-gene>), "FACIAL EXPRESSIONS");
  stream-the-gene-type(method(x) instance?(x, <organ-gene>) & ~instance?(x, <brain-organ-gene>) end, "ORGANS");
end method stream-genome-details;


define method stream-genome-differences( original-1 :: <genome>, original-2 :: <genome>, differences, new-in-lhs, new-in-rhs, stream )
  local method gene-sort-method( gene-a :: <gene>, gene-b :: <gene>)
    let value-a = gene-a.gene-index * 10000 + 
      gene-a.gene-sequence-number * 1000 +
      gene-a.gene-duplicate-number * 1000 +
      gene-a.gene-variant * 1000;
    let value-b = gene-b.gene-index * 10000 + 
      gene-b.gene-sequence-number * 1000 +
      gene-b.gene-duplicate-number * 1000 +
      gene-b.gene-variant * 1000;

    value-a < value-b;
  end method;

  local method differences-sort-method( a :: <pair>, b :: <pair>)
    let gene-a :: <gene> = a.head;
    let gene-b :: <gene> = b.head;
    let value-a = gene-a.gene-index * 10000 + 
      gene-a.gene-sequence-number * 1000 +
      gene-a.gene-duplicate-number * 1000 +
      gene-a.gene-variant * 1000;
    let value-b = gene-b.gene-index * 10000 + 
      gene-b.gene-sequence-number * 1000 +
      gene-b.gene-duplicate-number * 1000 +
      gene-b.gene-variant * 1000;

    value-a < value-b;
  end method;
  
          
  local method stream-the-gene-type( predicate, description )
    let differences-genes = sort(choose(compose(predicate, head), differences), test: differences-sort-method);
    let new-lhs-genes = sort(choose(predicate, new-in-lhs), test: gene-sort-method);
    let new-rhs-genes = sort(choose(predicate, new-in-rhs), test: gene-sort-method);
    format(stream, "***** %s ****\r\n", description);
    for(item in differences-genes)
      stream-gene( item.head, original-1, stream, note: "Different in file 1");
      format(stream, "\r\n");
      stream-gene( item.tail, original-2, stream, note: "Different in file 2");
      format(stream, "\r\n");
    end for;
    for(gene in new-lhs-genes)
      stream-gene( gene, original-1, stream, note: "New in file 1");
      format(stream, "\r\n");
    end for;
    for(gene in new-rhs-genes)
      stream-gene( gene, original-2, stream, note: "New in file 2");
      format(stream, "\r\n");
    end for;
  end method;

  stream-the-gene-type(rcurry(instance?, <brain-lobe-gene>), "LOBES");
  stream-the-gene-type(rcurry(instance?, <brain-organ-gene>), "BRAIN ORGANS");
  stream-the-gene-type(rcurry(instance?, <brain-tract-gene>), "BRAIN TRACTS");
  stream-the-gene-type(rcurry(instance?, <receptor-gene>), "RECEPTORS");
  stream-the-gene-type(rcurry(instance?, <emitter-gene>), "EMITTERS");
  stream-the-gene-type(rcurry(instance?, <neuro-emitter-gene>), "NEURO EMITTERS");
  stream-the-gene-type(rcurry(instance?, <reaction-gene>), "REACTIONS");
  stream-the-gene-type(rcurry(instance?, <half-lives-gene>), "HALF LIVES");
  stream-the-gene-type(rcurry(instance?, <initial-concentration-gene>), "INITIAL CONCENTRATIONS");
  stream-the-gene-type(rcurry(instance?, <stimulus-gene>), "STIMULI");
  stream-the-gene-type(rcurry(instance?, <genus-gene>), "GENUS RECORDS");
  stream-the-gene-type(rcurry(instance?, <appearance-gene>), "APPEARANCES");
  stream-the-gene-type(rcurry(instance?, <pose-gene>), "POSES");
  stream-the-gene-type(rcurry(instance?, <gait-gene>), "GAITS");
  stream-the-gene-type(rcurry(instance?, <instinct-gene>), "INSTINCTS");
  stream-the-gene-type(rcurry(instance?, <pigment-gene>), "PIGMENTS");
  stream-the-gene-type(rcurry(instance?, <pigment-bleed-gene>), "PIGMENT BLEEDS");
  stream-the-gene-type(rcurry(instance?, <facial-expression-gene>), "FACIAL EXPRESSIONS");
  stream-the-gene-type(method(x) instance?(x, <organ-gene>) & ~instance?(x, <brain-organ-gene>) end, "ORGANS");
end method stream-genome-differences;


define method update-display( frame :: <gene-compare-frame> )
  local method stream-seperator( stream ) 
    for(i from 0 below 72)
      format(stream, "*");
    finally
      format(stream, "\r\n");
    end;
  end;

  with-busy-cursor(frame)
    let stream = make(<string-stream>, direction: #"output");
    if(frame.document-genomes[0] & ~frame.document-genomes[1])      
      for(genome in frame.document-genomes)  
        when(genome & genome.genome-genes & genome.genome-genes.size > 0)
          stream-genome-summary(genome, stream);
          when(frame.frame-display-details?)
            stream-genome-details(genome, stream);
          end;
          stream-seperator( stream );
        end when;
      end for;
    elseif(frame.document-genomes[0] & frame.document-genomes[1])
      stream-genome-summary(frame.document-genomes[0], stream);
      stream-seperator( stream );
      stream-genome-summary(frame.document-genomes[1], stream);
      stream-seperator( stream );
      let (differences, new-in-lhs, new-in-rhs) = 
        compute-differences(frame.document-genomes[0], frame.document-genomes[1]);
      let different-genome = make(<genome>, 
        genes: concatenate(map(head, differences), new-in-lhs, new-in-rhs)); 
      format(stream, "Differences between two genomes:\r\n");
      stream-genome-summary(different-genome, stream);
      stream-seperator( stream );      
      stream-genome-differences(frame.document-genomes[0], 
        frame.document-genomes[1], 
        differences, new-in-lhs, new-in-rhs, stream);
      stream-seperator( stream );      
    end;
    frame.results-pane.gadget-value := stream.stream-contents;
    update-gadget(frame.results-pane);
  end;
end method;

define method on-toggle-details( frame :: <gene-compare-frame> )
  frame.frame-display-details? := ~frame.frame-display-details?;
  update-display( frame );
end;

define method main () => ()
  start-frame(make(<gene-compare-frame>));
end method main;

begin
  main();
end;
