Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <c2-genus-gene-pane> (<gene-pane>)

  pane genus-pane (frame)
    make(<option-box>, 
      items: key-sequence($genus-values),
      value: frame.pane-gene.gene-genus-species,
      label-key: curry(convert-to-description, <genus>),
      value-changed-callback: method(g) frame.pane-gene.gene-genus-species := g.gadget-value end);

  pane mum-moniker-pane (frame)
    make(<text-field>,    
      text: frame.pane-gene.gene-genus-mum,      
      value-changed-callback: method(g) frame.pane-gene.gene-genus-mum := g.gadget-value end);

  pane dad-moniker-pane (frame)
    make(<text-field>,
      text: frame.pane-gene.gene-genus-dad,      
      value-changed-callback: method(g) frame.pane-gene.gene-genus-dad := g.gadget-value end);

  pane moniker-table-pane (frame)
      make(<table-layout>,
       columns: 2,
       x-alignment: #[#"right", #"left"],
       y-alignment: #"center",
       x-ratios: #(1, 4),
       max-width: 100,
       children: vector(make(<label>, label: "Mum:"), frame.mum-moniker-pane,
                   make(<label>, label: "Dad:"), frame.dad-moniker-pane));

  pane genus-gene-main-pane (frame)
    vertically(max-width: 100)
      frame.genus-pane;
      frame.moniker-table-pane;
    end;
           
  layout (frame)
    vertically(x-alignment: #"center")
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Genus Data",
                        child: frame.genus-gene-main-pane);                   
    end;

end pane <c2-genus-gene-pane>;

define pane <c3-genus-gene-pane> (<gene-pane>)

  pane genus-pane (frame)
    make(<option-box>, 
      items: key-sequence($genus-values),
      value: frame.pane-gene.gene-genus-species,
      label-key: curry(convert-to-description, <genus>),
      value-changed-callback: method(g) frame.pane-gene.gene-genus-species := g.gadget-value end);

  pane data-pane (frame)
    make(<text-editor>,    
      word-wrap?: #t,
      text: frame.pane-gene.gene-genus-data,      
      value-changed-callback: method(g) frame.pane-gene.gene-genus-data := g.gadget-value end);

  pane genus-gene-main-pane (frame)
    vertically(max-width: 100)
      frame.genus-pane;
      labelling("Parent data:") frame.data-pane end;
    end;
           
  layout (frame)
    vertically(x-alignment: #"center")
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Genus Data",
                        child: frame.genus-gene-main-pane);                   
    end;

end pane <c3-genus-gene-pane>;


define method make-pane-for-gene( gene :: <c2-genus-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<c2-genus-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;

define method make-pane-for-gene( gene :: <c3-genus-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<c3-genus-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;

