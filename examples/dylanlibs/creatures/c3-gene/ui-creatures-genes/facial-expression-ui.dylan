Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define constant $c3-drive-descriptions =
  map(curry(get-emitter-locus-description, #"creatures3", 1, 5),
      range(from: 0, to: 19));

define function convert-c3-drive-description( number )
    element($c3-drive-descriptions,
      number,
      default: format-to-string("Unknown drive number: %d", number))
end;

define pane <facial-expression-gene-pane> (<gene-pane>)
  pane expression-number-pane (pane)
    make(<text-field>,
      value-type: <integer>,
      text: format-to-string("%d", pane.pane-gene.gene-expression-number),      
      value-changed-callback: method(g) pane.pane-gene.gene-expression-number := g.gadget-value end);

  pane expression-weight-pane (pane)
      make(<amount-selection-pane>, 
        description: "Weight",
        gene: pane.pane-gene,
        amount-getter: gene-expression-weight,
        amount-setter: gene-expression-weight-setter);

  pane drive1-pane (frame)
    make(<description-and-amount-pane>,
      gene: frame.pane-gene,
      type-getter: gene-expression-drive1,
      type-setter: gene-expression-drive1-setter,
      amount-getter: gene-expression-drive1-amount,
      amount-setter: gene-expression-drive1-amount-setter,
      descriptions: $c3-drive-descriptions,
      converter: convert-c3-drive-description);

  pane drive2-pane (frame)
    make(<description-and-amount-pane>,
      gene: frame.pane-gene,
      type-getter: gene-expression-drive2,
      type-setter: gene-expression-drive2-setter,
      amount-getter: gene-expression-drive2-amount,
      amount-setter: gene-expression-drive2-amount-setter,
      descriptions: $c3-drive-descriptions,
      converter: convert-c3-drive-description);


  pane drive3-pane (frame)
    make(<description-and-amount-pane>,
      gene: frame.pane-gene,
      type-getter: gene-expression-drive3,
      type-setter: gene-expression-drive3-setter,
      amount-getter: gene-expression-drive3-amount,
      amount-setter: gene-expression-drive3-amount-setter,
      descriptions: $c3-drive-descriptions,
      converter: convert-c3-drive-description);

  pane drive4-pane (frame)
    make(<description-and-amount-pane>,
      gene: frame.pane-gene,
      type-getter: gene-expression-drive4,
      type-setter: gene-expression-drive4-setter,
      amount-getter: gene-expression-drive4-amount,
      amount-setter: gene-expression-drive4-amount-setter,
      descriptions: $c3-drive-descriptions,
      converter: convert-c3-drive-description);


  pane expression-gene-main-pane (frame)
    vertically()
/*      make(<group-box>, label: "Stimulus", 
           child:  vertically() */
                    labelling("Expression Number:") frame.expression-number-pane end;
                    frame.expression-weight-pane;
/*                  end); */
/*      make(<group-box>, label: "Drives",
           child: vertically() */
                    frame.drive1-pane;
                    frame.drive2-pane;
                    frame.drive3-pane;
                    frame.drive4-pane;
/*                  end); */
    end;

  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Facial Expression Data",
                        child: frame.expression-gene-main-pane);                   
    end;

end pane <facial-expression-gene-pane>;

define method make-pane-for-gene( gene :: <facial-expression-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<facial-expression-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


