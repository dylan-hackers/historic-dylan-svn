Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <appearance-gene-pane> (<gene-pane>)
  pane species-pane (frame)
    make(<option-box>, 
      items: key-sequence($genus-values),
      value: frame.pane-gene.gene-appearance-species,
      label-key: curry(convert-to-description, <genus>),
      value-changed-callback: method(g) frame.pane-gene.gene-appearance-species := g.gadget-value end);

  pane body-part-pane (frame)
    make(<option-box>, 
      items: key-sequence($body-part-values),
      value: frame.pane-gene.gene-appearance-body-part,
      label-key: curry(convert-to-description, <body-part>),
      value-changed-callback: method(g) frame.pane-gene.gene-appearance-body-part := g.gadget-value end);

  pane breed-pane (frame)
    make(<description-selection-pane>,
      gene: frame.pane-gene,
      getter: gene-appearance-breed,
      setter: gene-appearance-breed-setter,
      descriptions: map(curry(format-to-string, "%c"), map(curry(as, <character>), range(from: 65 + 0, to: 65 + 25))),
      converter: compose(curry(format-to-string, "%c"), curry(as, <character>), curry(\+, 65) ) );

  pane appearance-gene-main-pane (frame)
    make(<table-layout>,
     columns: 2,
     x-alignment: #[#"right", #"left"],
     y-alignment: #"center",
     x-ratios: #(1, 4),
     max-width: 200,
     children: vector(make(<label>, label: "Species:"), frame.species-pane,
                      make(<label>, label: "Body Part:"), frame.body-part-pane,
                      make(<label>, label: "Breed:"), frame.breed-pane));
      
  layout (frame)
    vertically(x-alignment: #"center")
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Appearance Data",
                        child: frame.appearance-gene-main-pane);                   
    end;

end pane <appearance-gene-pane>;

define method make-pane-for-gene( gene :: <appearance-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<appearance-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


