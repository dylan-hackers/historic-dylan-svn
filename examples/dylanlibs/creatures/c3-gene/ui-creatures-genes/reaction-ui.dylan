Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <reaction-gene-pane> (<gene-pane>)
  pane lhs1-amount-pane (frame)
    make-amount-entry-pane(frame.pane-gene, gene-reaction-lhs1-amount);

  pane lhs2-amount-pane (frame)
    make-amount-entry-pane(frame.pane-gene, gene-reaction-lhs2-amount);

  pane rhs1-amount-pane (frame)
    make-amount-entry-pane(frame.pane-gene, gene-reaction-rhs1-amount);

  pane rhs2-amount-pane (frame)
    make-amount-entry-pane(frame.pane-gene, gene-reaction-rhs2-amount);

  pane rate-pane (frame)
    make(<amount-selection-table-pane>,
         gene: frame.pane-gene,
         descriptions: #("Reaction Rate"),
        getters: vector(gene-reaction-rate),
        setters: vector(gene-reaction-rate-setter));

  pane lhs1-chemical-pane (frame)
    make-chemical-pane( frame.pane-gene, gene-reaction-lhs1 );

  pane lhs2-chemical-pane (frame)
    make-chemical-pane( frame.pane-gene, gene-reaction-lhs2 );

  pane rhs1-chemical-pane (frame)
    make-chemical-pane( frame.pane-gene, gene-reaction-rhs1 );

  pane rhs2-chemical-pane (frame)
    make-chemical-pane( frame.pane-gene, gene-reaction-rhs2 );

  pane reaction-gene-main-pane (frame)
    vertically()
      horizontally()
        frame.lhs1-amount-pane;
        frame.lhs1-chemical-pane;
      end;
      make(<label>, label: "+");
      horizontally()
        frame.lhs2-amount-pane;
        frame.lhs2-chemical-pane;
      end;
      make(<label>, label: "=");
      horizontally()
        frame.rhs1-amount-pane;
        frame.rhs1-chemical-pane;
      end;
      make(<label>, label: "+");
      horizontally()
        frame.rhs2-amount-pane;
        frame.rhs2-chemical-pane;
      end;
      frame.rate-pane;
    end;
      
      
  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Reaction Data",
                        child: frame.reaction-gene-main-pane);                   
    end;

end pane <reaction-gene-pane>;

define method make-pane-for-gene( gene :: <reaction-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<reaction-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


