Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <pigment-gene-pane> (<gene-pane>)
  pane color-pane (frame)
    make(<option-box>, 
      items: key-sequence($pigmentation-values),
      value: frame.pane-gene.gene-pigment-color,
      label-key: curry(convert-to-description, <pigmentation>),
      value-changed-callback: method(g) frame.pane-gene.gene-pigment-color := g.gadget-value end);

  pane intensity-value-pane (frame)
    make(<text-field>, 
      value-type: <integer>,
      text: format-to-string("%d", frame.pane-gene.gene-pigment-intensity),
      value-changed-callback: method(g)
        frame.pane-gene.gene-pigment-intensity := g.gadget-value;
        frame.intensity-slider-pane.gadget-value := frame.pane-gene.gene-pigment-intensity;
      end);
        
  pane intensity-slider-pane (frame)
    make(<scroll-bar>,
      orientation: #"horizontal",
      value-range: range(from: 0, to: 255),
      value-changing-callback: method(g)
        frame.pane-gene.gene-pigment-intensity := g.gadget-value;
        frame.intensity-value-pane.gadget-value := frame.pane-gene.gene-pigment-intensity;
      end,
      value-changed-callback: method(g)
        frame.pane-gene.gene-pigment-intensity := g.gadget-value;
        frame.intensity-value-pane.gadget-value := frame.pane-gene.gene-pigment-intensity;
      end,
      slug-size: 1,
      value: frame.pane-gene.gene-pigment-intensity);           

  pane pigment-gene-main-pane (frame)
    vertically()
      frame.color-pane;
      make(<amount-selection-table-pane>,
        gene: frame.pane-gene,
        descriptions: #("Intensity"),
        getters: vector(gene-pigment-intensity),
        setters: vector(gene-pigment-intensity-setter));
    end;
      
      
  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Pigment Data",
                        child: frame.pigment-gene-main-pane);                   
    end;

end pane <pigment-gene-pane>;

define method make-pane-for-gene( gene :: <pigment-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<pigment-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


