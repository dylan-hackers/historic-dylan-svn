Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define pane <dendrite-growth-pane> ()
  constant slot pane-dendrite, required-init-keyword: dendrite:;
  constant slot pane-version, required-init-keyword: version:;

  pane dendrite-source-pane (pane)
    make(<description-selection-pane>,
         gene: pane.pane-dendrite,
         getter: dendrite-source-lobe,
         setter: dendrite-source-lobe-setter,
         descriptions: map(curry(get-lobe-description, pane.pane-version), range(from: 0, to: 255)),
         converter: curry(get-lobe-description, pane.pane-version));
         
  pane dendrite-properties-pane (pane)
    vertically()
      horizontally()
        make(<label>, label: "Minimum No.");
        make(<amount-entry-pane>,
          gene: pane.pane-dendrite,
          getter: dendrite-min,
          setter: dendrite-min-setter);
      end;
      horizontally()
        make(<label>, label: "Maximum No.");
        make(<amount-entry-pane>,
          gene: pane.pane-dendrite,
          getter: dendrite-max,
          setter: dendrite-max-setter);
      end;
      make(<description-selection-pane>,
        gene: pane.pane-dendrite,
        getter: dendrite-spread,
        setter: dendrite-spread-setter,
        descriptions: #["flat (---)", "normal (/\\)", "Saw (|\\)", "waS (/|)"],
        converter: curry(element, #("flat (---)", "normal (/\\)", "Saw (|\\)", "waS (/|)")));

    end;

  pane dendrite-initial-attachment-pane (pane)
    vertically()
      make(<amount-selection-table-pane>,
        gene: pane.pane-dendrite,
        descriptions: #("Fanout",
                        "Min LTW",
                        "Max LTW",
                        "Min Strength",
                        "Max Strength"),
        getters: vector(dendrite-fanout,
                        dendrite-min-ltw,
                        dendrite-max-ltw,
                        dendrite-min-strength,
                        dendrite-max-strength),
        setters: vector(dendrite-fanout-setter,
                        dendrite-min-ltw-setter,
                        dendrite-max-ltw-setter,
                        dendrite-min-strength-setter,
                        dendrite-max-strength-setter));
    end;

  pane dendrite-migration-rules-pane (pane)
    make(<description-selection-pane>,
         gene: pane.pane-dendrite,
         getter: dendrite-migrate-flag,
         setter: dendrite-migrate-flag-setter,
         descriptions: #["Dendrites do not migrate",
                         "Migrate if ANY dendrite is loose and this cell is firing",
                         "Migrate when ALL dens are loose"],
         converter: curry(element, #["Dendrites do not migrate",
                                     "Migrate if ANY dendrite is loose and this cell is firing",
                                     "Migrate when ALL dens are loose"]));

  pane dendrite-main-pane (pane)    
     vertically( )
        make(<group-box>, 
          label: "Source Lobe", 
          child: pane.dendrite-source-pane);
        make(<group-box>, 
          label: "Dendrite Properties", 
          child: pane.dendrite-properties-pane);
        make(<group-box>, 
          label: "Initial Attachment", child: pane.dendrite-initial-attachment-pane);
        make(<group-box>, 
          label: "Migration Rules", child: pane.dendrite-migration-rules-pane);
    end;

  layout (pane)
    pane.dendrite-main-pane;
    
end pane <dendrite-growth-pane>;

define pane <dendrite-dynamics-pane> ()
  constant slot pane-dendrite, required-init-keyword: dendrite:;

  pane atrophy-pane (pane)
    vertically()
      make(<amount-selection-table-pane>,
        gene: pane.pane-dendrite,
        descriptions: #("Relax Susceptibility",
                        "Relax STW",
                        "LTW Gain rate"),
        getters: vector(dendrite-relax-suscept,
                        dendrite-relax-stw,
                        dendrite-ltw-gain-rate),
        setters: vector(dendrite-relax-suscept-setter,
                        dendrite-relax-stw-setter,
                        dendrite-ltw-gain-rate-setter));
    end;

  pane suscept-svrule-pane (pane)
    make(<svrule-edit-pane>,
      description: "Susceptibility...",
      gene: pane.pane-dendrite,
      getter: dendrite-strength-suscept-rule,
      setter: dendrite-strength-suscept-rule-setter );

  pane reinforcement-svrule-pane (pane)
    make(<svrule-edit-pane>,
      description: "Reinforcement...",
      gene: pane.pane-dendrite,
      getter: dendrite-strength-relax-rule,
      setter: dendrite-strength-relax-rule-setter );

  pane forward-svrule-pane (pane)
    make(<svrule-edit-pane>,
      description: "Forward...",
      gene: pane.pane-dendrite,
      getter: dendrite-strength-forwardprop-rule,
      setter: dendrite-strength-forwardprop-rule-setter );

  pane back-svrule-pane (pane)
    make(<svrule-edit-pane>,
      description: "Back...",
      gene: pane.pane-dendrite,
      getter: dendrite-strength-backprop-rule,
      setter: dendrite-strength-backprop-rule-setter );

  pane strength-pane (pane)
    vertically()
        make(<amount-selection-pane>, 
          description: "Gain every",
          gene: pane.pane-dendrite,
          amount-getter: dendrite-strength-gain,
          amount-setter: dendrite-strength-gain-setter);
        horizontally()
          make(<svrule-edit-pane>,
            description: "If...",
            gene: pane.pane-dendrite,
            getter: dendrite-strength-gain-rule,
            setter: dendrite-strength-gain-rule-setter );
          make(<label>, label: ">0");
        end;
        make(<amount-selection-pane>, 
          description: "Lose every",
          gene: pane.pane-dendrite,
          amount-getter: dendrite-strength-loss,
          amount-setter: dendrite-strength-loss-setter);
        horizontally()
          make(<svrule-edit-pane>,
            description: "If...",
           gene: pane.pane-dendrite,
             getter: dendrite-strength-loss-rule,
            setter: dendrite-strength-loss-rule-setter );
          make(<label>, label: ">0");
      end;        
    end;
  
  pane dynamics-main-pane (pane)    
    vertically()
      make(<group-box>, label: "Atophy and consolidation", child: pane.atrophy-pane);
      pane.suscept-svrule-pane;
      pane.reinforcement-svrule-pane;
      pane.forward-svrule-pane;
      pane.back-svrule-pane;
      make(<group-box>, label: "Strength Adjustment", child: pane.strength-pane);
    end;

  layout (pane)
    pane.dynamics-main-pane;

end pane <dendrite-dynamics-pane>;


