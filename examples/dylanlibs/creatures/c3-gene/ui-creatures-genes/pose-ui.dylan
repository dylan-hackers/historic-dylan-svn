Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <pose-gene-pane> (<gene-pane>)

  pane pose-type-pane (pane)
    make(<description-selection-pane>,
      gene: pane.pane-gene,
      getter: gene-pose,
      setter: gene-pose-setter,
      descriptions: $pose-descriptions,
      converter: convert-to-pose-description);

  pane pose-string-pane (pane)
    make(<string-entry-pane>, 
      gene: pane.pane-gene,
      getter: compose(curry(as, <string>), gene-pose-data),
      setter: method(s, gene) gene-pose-data-setter(as(limited(<vector>, of: <byte>), s), gene) end);

  pane pose-gene-main-pane (pane)
    vertically( spacing: 2 )
      pane.pose-type-pane;
      horizontally()
        make(<label>, label: "Pose string (15):");
        pane.pose-string-pane;
      end;
    end;     
      
  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Pose Data",
                        child: frame.pose-gene-main-pane);                   
    end;

end pane <pose-gene-pane>;

define method make-pane-for-gene( gene :: <pose-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<pose-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;

