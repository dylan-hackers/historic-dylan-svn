Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define function set-half-life-amount( pane, value )
    let chemical = pane.chemical-list-pane.gadget-value;
    pane.pane-gene.gene-half-lives[chemical] := value;
    pane.amount-value-pane.gadget-value := value;
    pane.amount-slider-pane.gadget-value := value;
end;

define function get-half-life-amount( pane )
  let chemical = pane.chemical-list-pane.gadget-value;
  pane.pane-gene.gene-half-lives[chemical];
end;

define pane <half-lives-gene-pane> (<gene-pane>)
  pane chemical-list-pane (pane)
    make(<list-box>,
      min-height: 100,
      items: range(from: 0, to: 255),
      label-key: curry(convert-to-chemical-description, pane.pane-gene.gene-genome-version),
      value-changed-callback: 
        method(g) 
          pane.amount-value-pane.gadget-value := get-half-life-amount(pane);
          pane.amount-slider-pane.gadget-value := get-half-life-amount(pane);
        end);
      

  pane amount-value-pane (pane)
    make(<text-field>, 
      min-width: $byte-input-size,
      value-type: <integer>,
      value: 0,
      value-changed-callback: 
        method(g)
          set-half-life-amount(pane, g.gadget-value);
        end);

  pane amount-slider-pane (pane)
    make(<scroll-bar>,
      orientation: #"horizontal",
      value-range: range(from: 0, to: 255),
      value-changing-callback: method(g)
        set-half-life-amount(pane, g.gadget-value);
      end,
      value-changed-callback: method(g)
        set-half-life-amount(pane, g.gadget-value);
      end,
      slug-size: 1,
      value: 0);


  pane chemical-amount-pane (pane)
    make(<table-layout>,
      columns: 3,
      x-alignment: #[#"right", #"left", #"left"],
      y-alignment: #"center",
      x-ratios: #(4, 5, 1),
      children: vector(make(<label>, label: "Half Life"),
                       pane.amount-slider-pane,
                       pane.amount-value-pane));
      
  pane half-lives-gene-main-pane (pane)
    vertically()
      pane.chemical-list-pane;
      pane.chemical-amount-pane;
    end;

  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Half Lives Data",
                        child: frame.half-lives-gene-main-pane);                   
    end;

end pane <half-lives-gene-pane>;

define method make-pane-for-gene( gene :: <half-lives-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<half-lives-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


