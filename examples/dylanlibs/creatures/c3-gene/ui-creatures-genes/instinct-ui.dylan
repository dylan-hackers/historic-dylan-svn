Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define method make-instinct-lobe-cell-converter( pane )
  curry(get-lobe-cell-description, 
        pane.pane-gene.gene-genome-version, 
        pane.lobe-pane.description-pane.gadget-value);
end method;


define pane <instinct-lobe-cell-pane> (<gene-pane>)
  constant slot pane-lobe-getter, required-init-keyword: lobe-getter:;
  constant slot pane-lobe-setter, required-init-keyword: lobe-setter:;
  constant slot pane-cell-getter, required-init-keyword: cell-getter:;
  constant slot pane-cell-setter, required-init-keyword: cell-setter:;
  constant slot pane-lobe-converter = #f, init-keyword: lobe-converter:;

  pane lobe-pane (pane)
    make(<description-selection-pane>,
         gene: pane.pane-gene,
         value-changed-callback: method(p) 
           let lobe = p.description-pane.gadget-value;
           update-description-selection( pane.cell-pane, make-instinct-lobe-cell-converter(pane));
         end method,           
         getter: pane.pane-lobe-getter,
         setter: pane.pane-lobe-setter,
         converter: pane.pane-lobe-converter | 
           curry(get-lobe-description, pane.pane-gene.gene-genome-version));

  pane cell-pane (pane)
    make(<description-selection-pane>,
         gene: pane.pane-gene,
         getter: pane.pane-cell-getter,
         setter: pane.pane-cell-setter,
             converter: make-instinct-lobe-cell-converter(pane));

  layout (pane)
    horizontally()
      pane.lobe-pane;
      pane.cell-pane;
    end;

end pane <instinct-lobe-cell-pane>;

define pane <c2-instinct-gene-pane> (<gene-pane>)

  pane lobe1-pane (frame)
    make(<instinct-lobe-cell-pane>,
      gene: frame.pane-gene,
      lobe-getter: gene-instinct-lobe1,
      lobe-setter: gene-instinct-lobe1-setter,
      cell-getter: gene-instinct-lobe1-cell,
      cell-setter: gene-instinct-lobe1-cell-setter);

  pane lobe2-pane (frame)
    make(<instinct-lobe-cell-pane>,
      gene: frame.pane-gene,
      lobe-getter: gene-instinct-lobe2,
      lobe-setter: gene-instinct-lobe2-setter,
      cell-getter: gene-instinct-lobe2-cell,
      cell-setter: gene-instinct-lobe2-cell-setter);

  pane lobe3-pane (frame)
    make(<instinct-lobe-cell-pane>,
      gene: frame.pane-gene,
      lobe-getter: gene-instinct-lobe3,
      lobe-setter: gene-instinct-lobe3-setter,
      cell-getter: gene-instinct-lobe3-cell,
      cell-setter: gene-instinct-lobe3-cell-setter);

  pane lobe-cells-pane (frame)
    vertically()
      frame.lobe1-pane;
      frame.lobe2-pane;
      frame.lobe3-pane;
    end;

  pane decision-cell-pane (frame)
    make(<option-box>, 
      items: range(from: 0, to: 255),
      value: frame.pane-gene.gene-instinct-decision,
      label-key: curry(get-lobe-cell-description, frame.pane-gene.gene-genome-version, 6),
      value-changed-callback: method(g) frame.pane-gene.gene-instinct-decision := g.gadget-value end);

  pane chemical-pane (frame)
    make(<chemical-pane>,
      gene: frame.pane-gene,
      chemical-getter: gene-instinct-chemical,
      chemical-setter: gene-instinct-chemical-setter);

  pane amount-pane (frame)
    make(<amount-selection-table-pane>,
      gene: frame.pane-gene,
      descriptions: #("Chemical amount"),
      getters: vector(gene-instinct-chemical-amount),
      setters: vector(gene-instinct-chemical-amount-setter));

  pane instinct-gene-main-pane (frame)
    vertically()
      make(<group-box>, label: "When this is true (lobe/cell)",
                        child: frame.lobe-cells-pane);                   

      make(<label>, label: "...and you do this");
      frame.decision-cell-pane;
      make(<label>, label: "...reward/punish like this");
      frame.chemical-pane;
      frame.amount-pane;
    end;
          
  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Instinct Data",
                        child: frame.instinct-gene-main-pane);                   
    end;

end pane <c2-instinct-gene-pane>;


define pane <c3-instinct-gene-pane> (<gene-pane>)

  pane lobe1-pane (frame)
    make(<instinct-lobe-cell-pane>,
      gene: frame.pane-gene,
      lobe-getter: gene-instinct-lobe1,
      lobe-setter: gene-instinct-lobe1-setter,
      cell-getter: gene-instinct-lobe1-cell,
      cell-setter: gene-instinct-lobe1-cell-setter);

  pane lobe2-pane (frame)
    make(<instinct-lobe-cell-pane>,
      gene: frame.pane-gene,
      lobe-getter: gene-instinct-lobe2,
      lobe-setter: gene-instinct-lobe2-setter,
      cell-getter: gene-instinct-lobe2-cell,
      cell-setter: gene-instinct-lobe2-cell-setter);

  pane lobe3-pane (frame)
    make(<instinct-lobe-cell-pane>,
      gene: frame.pane-gene,
      lobe-getter: gene-instinct-lobe3,
      lobe-setter: gene-instinct-lobe3-setter,
      cell-getter: gene-instinct-lobe3-cell,
      cell-setter: gene-instinct-lobe3-cell-setter);

  pane lobe-cells-pane (frame)
    vertically()
      frame.lobe1-pane;
      frame.lobe2-pane;
      frame.lobe3-pane;
    end;

  pane decision-cell-pane (frame)
    make(<option-box>, 
      items: range(from: 0, to: 255),
      value: frame.pane-gene.gene-instinct-decision,
      label-key: curry(get-lobe-cell-description, frame.pane-gene.gene-genome-version, 10),
      value-changed-callback: method(g) frame.pane-gene.gene-instinct-decision := g.gadget-value end);

  pane chemical-pane (frame)
    make(<drive-pane>,
      gene: frame.pane-gene,
      drive-getter: gene-instinct-chemical,
      drive-setter: gene-instinct-chemical-setter);

  pane amount-pane (frame)
    make(<amount-selection-table-pane>,
      gene: frame.pane-gene,
      descriptions: #("Unknown amount"),
      getters: vector(gene-instinct-chemical-amount),
      setters: vector(gene-instinct-chemical-amount-setter));

  pane instinct-gene-main-pane (frame)
    vertically()
      make(<group-box>, label: "When this is true (lobe/cell)",
                        child: frame.lobe-cells-pane);                   

      make(<label>, label: "...and this drive is high");
      frame.chemical-pane;
      make(<label>, label: "...then do this");
      frame.decision-cell-pane;
      frame.amount-pane;
    end;
          
  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Instinct Data",
                        child: frame.instinct-gene-main-pane);                   
    end;

end pane <c3-instinct-gene-pane>;

define method make-pane-for-gene( gene :: <c2-instinct-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<c2-instinct-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;

define method make-pane-for-gene( gene :: <c3-instinct-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<c3-instinct-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


