Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <organ-gene-pane> (<gene-pane>)

  pane organ-gene-main-pane (frame)
    vertically()
      make(<amount-selection-table-pane>,
        gene: frame.pane-gene,
        descriptions: #("Clockrate",
                        "Lifeforce repair rate",
                        "Lifeforce start value",
                        "Biotick start",
                        "ATP damage coefficient"),
        getters: vector(gene-organ-clock-rate,
                        gene-organ-life-force-repair-rate,
                        gene-organ-life-force-start,
                        gene-organ-biotick-start,
                        gene-organ-atp-damage-coefficient),
        setters: vector(gene-organ-clock-rate-setter,
                        gene-organ-life-force-repair-rate-setter,
                        gene-organ-life-force-start-setter,
                        gene-organ-biotick-start-setter,
                        gene-organ-atp-damage-coefficient-setter));
    end;
      
      
  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Organ Data",
                        child: frame.organ-gene-main-pane);                   
    end;

end pane <organ-gene-pane>;

define method make-pane-for-gene( gene :: <organ-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<organ-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


