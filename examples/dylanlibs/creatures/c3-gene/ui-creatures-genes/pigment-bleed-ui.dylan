Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <pigment-bleed-gene-pane> (<gene-pane>)
  pane pigment-bleed-gene-main-pane (pane)
    vertically()
      make(<amount-selection-table-pane>,
        gene: pane.pane-gene,
        descriptions: #("Rotation", "Swap"),
        getters: vector(gene-pigment-bleed-rotation, gene-pigment-bleed-swap),
        setters: vector(gene-pigment-bleed-rotation-setter, gene-pigment-bleed-swap-setter));
        
    end;
      
  layout (pane)
    vertically()
      make(<gene-header-pane>, gene: pane.pane-gene);
      make(<group-box>, label: "Pigment Bleed Data",
                        child: pane.pigment-bleed-gene-main-pane);                   
    end;

end pane <pigment-bleed-gene-pane>;

define method make-pane-for-gene( gene :: <pigment-bleed-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<pigment-bleed-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


