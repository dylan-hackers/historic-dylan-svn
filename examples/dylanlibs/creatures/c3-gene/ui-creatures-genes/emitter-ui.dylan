Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <emitter-gene-pane> (<gene-pane>)

  pane emitter-gene-main-pane (pane)
    vertically(max-width: 200)
      make(<emitter-loci-pane>, gene: pane.pane-gene, genome: pane.pane-genome);

      make(<chemical-pane>,
        gene: pane.pane-gene,
        chemical-getter: gene-emitter-chemical,
        chemical-setter: gene-emitter-chemical-setter);

      make(<amount-selection-table-pane>,
        gene: pane.pane-gene,
        descriptions: #("Sample Rate",
                        "Gain",
                        "Threshold"),
        getters: vector(gene-emitter-sample-rate,
                        gene-emitter-gain,
                        gene-emitter-threshold),
        setters: vector(gene-emitter-sample-rate-setter,
                        gene-emitter-gain-setter,
                        gene-emitter-threshold-setter));

     make(<bitflag-pane>, 
        gene: pane.pane-gene,
        getter: gene-emitter-is-digital?,
        setter: gene-emitter-is-digital?-setter,
        description: "Digital");

     make(<bitflag-pane>, 
        gene: pane.pane-gene,
        getter: gene-emitter-clear-source?,
        setter: gene-emitter-clear-source?-setter,
        description: "Clear source byte after reading");

     make(<bitflag-pane>, 
        gene: pane.pane-gene,
        getter: gene-emitter-is-inverted?,
        setter: gene-emitter-is-inverted?-setter,
        description: "Invert input signal (s=255-s)");
   end;

          
  layout (pane)
    vertically()
      make(<gene-header-pane>, gene: pane.pane-gene);
      make(<group-box>, label: "Emitter Data",
                        child: pane.emitter-gene-main-pane);                   
    end;

end pane <emitter-gene-pane>;

define method make-pane-for-gene( gene :: <emitter-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<emitter-gene-pane>, gene: gene, genome: sort(genome, test: method(lhs, rhs) lhs.item-index < rhs.item-index end));
end method make-pane-for-gene;


