Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <brain-lobe-gene-pane> (<gene-pane>)

  pane lobe-position-pane (frame)
      make(<amount-selection-table-pane>,
        gene: frame.pane-gene,
        descriptions: #["X", "Y", "Width", "Height"],
        getters: vector(gene-brain-lobe-x-position,
                        gene-brain-lobe-y-position,
                        gene-brain-lobe-width,
                        gene-brain-lobe-height),
        setters: vector(gene-brain-lobe-x-position-setter,
                        gene-brain-lobe-y-position-setter,
                        gene-brain-lobe-width-setter,
                        gene-brain-lobe-height-setter));

  pane perception-flag-pane (frame)
    make(<description-selection-pane>,
      gene: frame.pane-gene,
      getter: gene-brain-lobe-perception-flag,
      setter: gene-brain-lobe-perception-flag-setter,
      descriptions: #("No", "Yes", "Mutually Exclusive"),
      converter: curry(element, #("No", "Yes", "Mutually Exclusive")));

  pane general-tab-pane (frame)
    vertically()
      make(<group-box>,         
        label: "Lobe Position and Size", 
        child: frame.lobe-position-pane);
      make(<group-box>, 
        label: "Data copied to perception lobe?", 
        child: frame.perception-flag-pane);
    end;
    
  pane cell-body-dynamics-pane (frame)
    vertically()
      make(<amount-selection-table-pane>,
        gene: frame.pane-gene,
        descriptions: #("Nominal Threshold", 
                        "Leakage rate",
                        "Rest state",
                        "Input gain lo-hi"),
        getters: vector(gene-brain-lobe-nominal-threshold,
                        gene-brain-lobe-leakage-rate,
                        gene-brain-lobe-rest-state,
                        gene-brain-lobe-input-gain),
        setters: vector(gene-brain-lobe-nominal-threshold-setter,
                        gene-brain-lobe-leakage-rate-setter,
                        gene-brain-lobe-rest-state-setter,
                        gene-brain-lobe-input-gain-setter));
      make(<bitflag-pane>, 
        gene: frame.pane-gene,
        getter: gene-brain-lobe-winner-takes-all?,
        setter: gene-brain-lobe-winner-takes-all?-setter,
        description: "Winner takes all");

    end;
  
  pane neuron-state-rule-pane (frame)
    make(<svrule-edit-pane>,
      gene: frame.pane-gene,
      getter: gene-brain-lobe-svrule,
      setter: gene-brain-lobe-svrule-setter );

  pane cell-body-tab-pane (frame)
    vertically()
      make(<group-box>, label: "Cell Body Dynamics", child: frame.cell-body-dynamics-pane);
      make(<group-box>, label: "Neuron State Rule", child: frame.neuron-state-rule-pane);
    end;

  pane dendrite0-growth-tab-pane (frame)
    vertically()
      make(<dendrite-growth-pane>, dendrite: frame.pane-gene.gene-brain-lobe-dendrites[0], version: frame.pane-gene.gene-genome-version);   
    end;

  pane dendrite0-dynamics-tab-pane (frame)
    vertically()
      make(<dendrite-dynamics-pane>, dendrite: frame.pane-gene.gene-brain-lobe-dendrites[0]);   
    end;

  pane dendrite1-growth-tab-pane (frame)
    vertically()
      make(<dendrite-growth-pane>, dendrite: frame.pane-gene.gene-brain-lobe-dendrites[1], version: frame.pane-gene.gene-genome-version);   
    end;

  pane dendrite1-dynamics-tab-pane (frame)
    vertically()
      make(<dendrite-dynamics-pane>, dendrite: frame.pane-gene.gene-brain-lobe-dendrites[1]);   
    end;

  pane all-tabs-pane (frame)
    make(<tab-control>, pages: vector(make(<tab-control-page>, 
                                           label: "General",
                                           child: frame.general-tab-pane),
                                      make(<tab-control-page>, 
                                           label: "Cell Body",
                                           child: frame.cell-body-tab-pane),
                                      make(<tab-control-page>,
                                           label: "Dendrite 0",
                                           child:
                                             make(<tab-control>,
                                               pages:
                                                 vector(
                                      make(<tab-control-page>, 
                                           label: "Growth",
                                           child: frame.dendrite0-growth-tab-pane),
                                      make(<tab-control-page>, 
                                           label: "Dynamics",
                                           child: frame.dendrite0-dynamics-tab-pane)))),
                                      make(<tab-control-page>,
                                        label: "Dendrite 1",
                                        child: make(<tab-control>, pages: vector(
                                        
                                        
                                      make(<tab-control-page>, 
                                           label: "Growth",
                                           child: frame.dendrite1-growth-tab-pane),
                                      make(<tab-control-page>, 
                                           label: "Dynamics",
                                           child: frame.dendrite1-dynamics-tab-pane)
                                      )))));
     
  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Brain Lobe Data",
      child: frame.all-tabs-pane  );                  
    end;

end pane <brain-lobe-gene-pane>;

define method make-pane-for-gene( gene :: <c2-brain-lobe-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<brain-lobe-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;

define pane <c3-brain-lobe-gene-pane> (<gene-pane>)
  pane name-pane (frame)
    make(<text-field>,    
      text: frame.pane-gene.gene-brain-lobe-name,      
      value-changed-callback: method(g) frame.pane-gene.gene-brain-lobe-name := g.gadget-value end);

  pane unknown-pane (frame)
    make(<text-editor>,   
      read-only?: #t,
      text: 
        begin
          let count = 0;
          let total-counts = 0;
          let text = "0000: ";
          for( value in frame.pane-gene.gene-brain-lobe-unknown )
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


  pane lobe-gene-main-pane (frame)
    vertically(max-width: 100)
      labelling("Lobe Name:") frame.name-pane end;
      make(<label>, label: "Lobe Data:");
      frame.unknown-pane;
    end;
           
  layout (frame)
    vertically(x-alignment: #"center")
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Brain Lobe Data",
                        child: frame.lobe-gene-main-pane);                   
    end;

end pane <c3-brain-lobe-gene-pane>;


define method make-pane-for-gene( gene :: <c3-brain-lobe-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<c3-brain-lobe-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


