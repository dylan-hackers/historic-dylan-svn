Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <receptor-gene-pane> (<gene-pane>)
  pane receptor-gene-main-pane (frame)
    vertically(max-width: 200)
      make(<receptor-loci-pane>, gene: frame.pane-gene, genome: frame.pane-genome);

      make(<chemical-pane>,
        gene: frame.pane-gene,
        chemical-getter: gene-receptor-chemical,
        chemical-setter: gene-receptor-chemical-setter);

      make(<amount-selection-table-pane>,
        gene: frame.pane-gene,
        descriptions: #("Threshold",
                        "Nominal",
                        "Gain"),
        getters: vector(gene-receptor-threshold,
                        gene-receptor-nominal,
                        gene-receptor-gain),
        setters: vector(gene-receptor-threshold-setter,
                        gene-receptor-nominal-setter,
                        gene-receptor-gain-setter));
     make(<bitflag-pane>, 
        gene: frame.pane-gene,
        getter: gene-receptor-is-inverted?,
        setter: gene-receptor-is-inverted?-setter,
        description: "Output REDUCES with increased stimulation");

     make(<bitflag-pane>, 
        gene: frame.pane-gene,
        getter: gene-receptor-is-digital?,
        setter: gene-receptor-is-digital?-setter,
        description: "Digital (o/p=nominal+-gain if sig > threshold)");
    end;
      
  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Receptor Data",
                        child: frame.receptor-gene-main-pane);                   
    end;

end pane <receptor-gene-pane>;

define method make-pane-for-gene( gene :: <receptor-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<receptor-gene-pane>, gene: gene, genome: sort(genome, test: method(lhs, rhs) lhs.item-index < rhs.item-index end));
end method make-pane-for-gene;


