Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define constant $gait-pose-descriptions = 
    concatenate(#("<empty>"), $pose-descriptions);

define method gait-convert-to-pose-description( type :: <byte> ) => (result :: <string>)
    element($gait-pose-descriptions, 
      type, 
      default: format-to-string("Unknown Pose: %=", type));
end method gait-convert-to-pose-description;

define method make-gait-pose-getter( index )
    method(gene) 
      let value = gene.gene-gait-data[index];
      if(value = 0)
        0
      else
        value + 1;
      end if;
    end
end method make-gait-pose-getter;

define method make-gait-pose-setter( index )
    method(a, gene) 
      gene.gene-gait-data[index] := if( a = 0)
                                      0
                                    else
                                      a - 1
                                    end if;
    end
end method make-gait-pose-setter;

define pane <gait-gene-pane> (<gene-pane>)

  pane gait-pane (pane)
    make(<option-box>, 
      items: key-sequence($gait-values),
      value: pane.pane-gene.gene-gait,
      label-key: curry(convert-to-description, <gait>),
      value-changed-callback: method(g) pane.pane-gene.gene-gait := g.gadget-value end);

  pane pose1-pane (pane)
    make(<description-selection-pane>,
      gene: pane.pane-gene,
      getter: make-gait-pose-getter(0),
      setter: make-gait-pose-setter(0),
      descriptions: $gait-pose-descriptions,
      converter: gait-convert-to-pose-description);

  pane pose2-pane (pane)
    make(<description-selection-pane>,
      gene: pane.pane-gene,
      getter: make-gait-pose-getter(1),
      setter: make-gait-pose-setter(1),
      descriptions: $gait-pose-descriptions,
      converter: gait-convert-to-pose-description);

  pane pose3-pane (pane)
    make(<description-selection-pane>,
      gene: pane.pane-gene,
      getter: make-gait-pose-getter(2),
      setter: make-gait-pose-setter(2),
      descriptions: $gait-pose-descriptions,
      converter: gait-convert-to-pose-description);

  pane pose4-pane (pane)
    make(<description-selection-pane>,
      gene: pane.pane-gene,
      getter: make-gait-pose-getter(3),
      setter: make-gait-pose-setter(3),
      descriptions: $gait-pose-descriptions,
      converter: gait-convert-to-pose-description);

  pane pose5-pane (pane)
    make(<description-selection-pane>,
      gene: pane.pane-gene,
      getter: make-gait-pose-getter(4),
      setter: make-gait-pose-setter(4),
      descriptions: $gait-pose-descriptions,
      converter: gait-convert-to-pose-description);

  pane pose6-pane (pane)
    make(<description-selection-pane>,
      gene: pane.pane-gene,
      getter: make-gait-pose-getter(5),
      setter: make-gait-pose-setter(5),
      descriptions: $gait-pose-descriptions,
      converter: gait-convert-to-pose-description);

  pane pose7-pane (pane)
    make(<description-selection-pane>,
      gene: pane.pane-gene,
      getter: make-gait-pose-getter(6),
      setter: make-gait-pose-setter(6),
      descriptions: $gait-pose-descriptions,
      converter: gait-convert-to-pose-description);

  pane pose8-pane (pane)
    make(<description-selection-pane>,
      gene: pane.pane-gene,
      getter: make-gait-pose-getter(7),
      setter: make-gait-pose-setter(7),
      descriptions: $gait-pose-descriptions,
      converter: gait-convert-to-pose-description);


  pane gait-gene-main-pane (pane)
    vertically( spacing: 2 )
      pane.gait-pane;
      make(<label>, label: "Up to 8 Poses for this gait:");
      pane.pose1-pane;
      pane.pose2-pane;
      pane.pose3-pane;
      pane.pose4-pane;
      pane.pose5-pane;
      pane.pose6-pane;
      pane.pose7-pane;
      pane.pose8-pane;
    end;     
      
  layout (pane)
    vertically()
      make(<gene-header-pane>, gene: pane.pane-gene);
      make(<group-box>, label: "Pose Data",
                        child: pane.gait-gene-main-pane);                   
    end;

end pane <gait-gene-pane>;

define method make-pane-for-gene( gene :: <gait-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<gait-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;

