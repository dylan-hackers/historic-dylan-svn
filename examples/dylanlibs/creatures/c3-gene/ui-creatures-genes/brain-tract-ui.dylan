Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <brain-tract-gene-pane> (<gene-pane>)
  pane unknown1-pane (pane)
        make(<amount-entry-pane>,
          gene: pane.pane-gene,
          getter: gene-brain-tract-unknown1,
          setter: gene-brain-tract-unknown1-setter);

  pane source-name-pane (frame)
    make(<text-field>,    
      text: frame.pane-gene.gene-brain-tract-source-lobe,      
      value-changed-callback: method(g) frame.pane-gene.gene-brain-tract-source-lobe := g.gadget-value end);

  pane source-first-pane (pane)
        make(<amount-entry-pane>,
          gene: pane.pane-gene,
          getter: gene-brain-tract-source-lower,
          setter: gene-brain-tract-source-lower-setter);

  pane source-last-pane (pane)
        make(<amount-entry-pane>,
          gene: pane.pane-gene,
          getter: gene-brain-tract-source-upper,
          setter: gene-brain-tract-source-upper-setter);

  pane source-amount-pane (pane)
        make(<amount-entry-pane>,
          gene: pane.pane-gene,
          getter: gene-brain-tract-source-amount,
          setter: gene-brain-tract-source-amount-setter);

  pane source-pane (pane)
    make(<group-box>, label: "Source Lobe",
      child: vertically()
               labelling("Name:") pane.source-name-pane end;
               labelling("First cell:") pane.source-first-pane end;
               labelling("Last cell:") pane.source-last-pane end;
               labelling("No. of dendrites") pane.source-amount-pane end;
             end);

  pane destination-name-pane (frame)
    make(<text-field>,    
      text: frame.pane-gene.gene-brain-tract-destination-lobe,      
      value-changed-callback: method(g) frame.pane-gene.gene-brain-tract-destination-lobe := g.gadget-value end);

  pane destination-first-pane (pane)
        make(<amount-entry-pane>,
          gene: pane.pane-gene,
          getter: gene-brain-tract-destination-lower,
          setter: gene-brain-tract-destination-lower-setter);

  pane destination-last-pane (pane)
        make(<amount-entry-pane>,
          gene: pane.pane-gene,
          getter: gene-brain-tract-destination-upper,
          setter: gene-brain-tract-destination-upper-setter);

  pane destination-amount-pane (pane)
        make(<amount-entry-pane>,
          gene: pane.pane-gene,
          getter: gene-brain-tract-destination-amount,
          setter: gene-brain-tract-destination-amount-setter);

  pane destination-pane (pane)
    make(<group-box>, label: "Destination Lobe",
      child: vertically()
               labelling("Name:") pane.destination-name-pane end;
               labelling("First cell:") pane.destination-first-pane end;
               labelling("Last cell:") pane.destination-last-pane end;
               labelling("No. of dendrites") pane.destination-amount-pane end;
             end);

  pane unknown-data-pane (pane)
    make(<text-editor>,   
      read-only?: #t,
      text: 
        begin
          let count = 0;
          let total-counts = 0;
          let text = "0000: ";
          for( value in pane.pane-gene.gene-brain-tract-unknown )
            text := concatenate(text, " ", integer-to-string(value, base: 16, size: 2));
            count := count + 1;
            if(count == 8)
              total-counts := total-counts + 1;
              text := concatenate( text, "\n", 
                integer-to-string(total-counts * 8, base: 16, size: 4), ": ");
              count := 0;
            end if;
          finally 
            text;
          end for;
        end);

  pane brain-tract-gene-main-pane (pane)
    make(<tab-control>, pages: vector(
      make(<tab-control-page>,
        label: "Tract",
        child: vertically()
                 labelling("Unknown:") pane.unknown1-pane end;
                 pane.source-pane;
                 pane.destination-pane
               end),
      make(<tab-control-page>,
        label: "Unknown data",
        child: vertically()
                 pane.unknown-data-pane;
               end)));

  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Brain Tract Data",
                        child: frame.brain-tract-gene-main-pane);                   
    end;

end pane <brain-tract-gene-pane>;

define method make-pane-for-gene( gene :: <brain-tract-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<brain-tract-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


