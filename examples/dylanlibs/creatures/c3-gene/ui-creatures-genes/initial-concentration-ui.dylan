Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <initial-concentration-gene-pane> (<gene-pane>)

  pane initial-concentration-gene-main-pane (frame)
    make(<chemical-and-amount-pane>,
      gene: frame.pane-gene,
      chemical-getter: gene-initial-concentration-chemical,
      chemical-setter: gene-initial-concentration-chemical-setter,
      amount-getter: gene-initial-concentration-amount,
      amount-setter: gene-initial-concentration-amount-setter);
   
  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Initial Concentration Data",
                        child: frame.initial-concentration-gene-main-pane);                   
    end;

end pane <initial-concentration-gene-pane>;

define method make-pane-for-gene( gene :: <initial-concentration-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<initial-concentration-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


