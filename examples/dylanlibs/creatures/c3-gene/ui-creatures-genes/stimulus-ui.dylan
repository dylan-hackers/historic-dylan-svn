Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define constant $c2-stimulus-general-sensory-descriptions = 
    concatenate(#("None"), $c2-general-sensory-descriptions,
      map(method(x) format-to-string("Unknown General Sensory: %=", x) end,
          range(from: $c2-general-sensory-descriptions.size, 
                to: 253 - $c2-general-sensory-descriptions.size)));

define constant $c3-stimulus-general-sensory-descriptions = 
    map(curry(format-to-string, "Unknown sensory neuron: %d"), range(from: 0, to: 255));

define method stimulus-convert-to-general-sensory-description( version == #"creatures2", type :: <general-sensory> ) => (result :: <string>)
    element($c2-stimulus-general-sensory-descriptions, 
      type, 
      default: format-to-string("Unknown General Sensory: %=", type));
end method stimulus-convert-to-general-sensory-description;

define method stimulus-convert-to-general-sensory-description( version == #"creatures3", type :: <general-sensory> ) => (result :: <string>)
    element($c3-stimulus-general-sensory-descriptions, 
      type, 
      default: format-to-string("Unknown General Sensory: %=", type));
end method stimulus-convert-to-general-sensory-description;

define method stimulus-gene-stimulus-sensory-neuron( gene )
    if(gene.gene-stimulus-sensory-neuron = 255) 
      0
    else
      gene.gene-stimulus-sensory-neuron + 1;
    end if;
end method stimulus-gene-stimulus-sensory-neuron;

define method stimulus-gene-stimulus-sensory-neuron-setter( type, gene )
    gene.gene-stimulus-sensory-neuron := if(type = 0) 255 else type - 1 end if;
end method stimulus-gene-stimulus-sensory-neuron-setter;

define pane <c2-stimulus-gene-pane> (<gene-pane>)
  pane chemical1-pane (frame)
    make(<chemical-and-amount-pane>,
      gene: frame.pane-gene,
      chemical-getter: gene-stimulus-chemical1,
      chemical-setter: gene-stimulus-chemical1-setter,
      amount-getter: gene-stimulus-chemical1-amount,
      amount-setter: gene-stimulus-chemical1-amount-setter);

  pane chemical2-pane (frame)
    make(<chemical-and-amount-pane>,
      gene: frame.pane-gene,
      chemical-getter: gene-stimulus-chemical2,
      chemical-setter: gene-stimulus-chemical2-setter,
      amount-getter: gene-stimulus-chemical2-amount,
      amount-setter: gene-stimulus-chemical2-amount-setter);


  pane chemical3-pane (frame)
    make(<chemical-and-amount-pane>,
      gene: frame.pane-gene,
      chemical-getter: gene-stimulus-chemical3,
      chemical-setter: gene-stimulus-chemical3-setter,
      amount-getter: gene-stimulus-chemical3-amount,
      amount-setter: gene-stimulus-chemical3-amount-setter);

  pane chemical4-pane (frame)
    make(<chemical-and-amount-pane>,
      gene: frame.pane-gene,
      chemical-getter: gene-stimulus-chemical4,
      chemical-setter: gene-stimulus-chemical4-setter,
      amount-getter: gene-stimulus-chemical4-amount,
      amount-setter: gene-stimulus-chemical4-amount-setter);

  pane stimulus-pane (frame)
     make(<description-and-amount-pane>,
      gene: frame.pane-gene,
      type-getter: gene-stimulus-type,
      type-setter: gene-stimulus-type-setter,
      amount-getter: gene-stimulus-significance,
      amount-setter: gene-stimulus-significance-setter,
      amount-description: "Significance",
      descriptions: if(frame.pane-gene.gene-genome-version == #"creatures2")
                      $c2-stimulus-descriptions 
                    else 
                      $c3-stimulus-descriptions
                    end if,
      converter: curry(convert-to-stimulus-description, frame.pane-gene.gene-genome-version));

pane sensory-pane (frame)
     make(<description-and-amount-pane>,
      gene: frame.pane-gene,
      type-getter: stimulus-gene-stimulus-sensory-neuron,
      type-setter: stimulus-gene-stimulus-sensory-neuron-setter,
      amount-getter: gene-stimulus-intensity,
      amount-setter: gene-stimulus-intensity-setter,
      amount-description: "Intensity",
      descriptions: if(frame.pane-gene.gene-genome-version == #"creatures2")
                      $c2-stimulus-general-sensory-descriptions 
                    else 
                      $c2-stimulus-general-sensory-descriptions
                    end if,
      converter: curry(stimulus-convert-to-general-sensory-description, frame.pane-gene.gene-genome-version));

  pane stimulus-lhs-pane (frame)
    vertically()
      make(<label>, label: "Stimulus");
      frame.stimulus-pane;
      make(<label>, label: "Sensory Neuron");
      frame.sensory-pane;
      make(<bitflag-pane>, 
        gene: frame.pane-gene,
        getter: gene-stimulus-modulate?,
        setter: gene-stimulus-modulate?-setter,
        description: "Modulate using sensory signal");
      make(<bitflag-pane>, 
        gene: frame.pane-gene,
        getter: gene-stimulus-add-offset?,
        setter: gene-stimulus-add-offset?-setter,
        description: "Add offset to neu (eg. word#)");
      make(<bitflag-pane>, 
        gene: frame.pane-gene,
        getter: gene-stimulus-sensed-asleep?,
        setter: gene-stimulus-sensed-asleep?-setter,
        description: "Detected even if asleep");
    end;

  pane stimulus-rhs-pane (frame)
    vertically()
      frame.chemical1-pane;
      frame.chemical2-pane;
      frame.chemical3-pane;
      frame.chemical4-pane;
    end;

  pane stimulus-gene-main-pane (frame)
    make(<tab-control>, pages: vector(make(<tab-control-page>, 
                                           label: "Stimulus",
                                           child: frame.stimulus-lhs-pane),
                                      make(<tab-control-page>, 
                                           label: "Chemicals Injected",
                                           child: frame.stimulus-rhs-pane)
                                      ));

  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Stimulus Data",
                        child: frame.stimulus-gene-main-pane);                   
    end;

end pane <c2-stimulus-gene-pane>;

define function c3-stimulus-chemical-translator (value)
  case
    (value >= 0 & value <= 107) => value + 148;
    (value >= 108 & value <=254) => value - 107;
    255 => 0;
    otherwise => value;
  end case;
end;
  
define pane <c3-stimulus-gene-pane> (<gene-pane>)
  pane chemical1-pane (frame)
    make(<chemical-and-amount-pane>,
      translator: c3-stimulus-chemical-translator,
      pivot: 124,
      gene: frame.pane-gene,
      chemical-getter: gene-stimulus-chemical1,
      chemical-setter: gene-stimulus-chemical1-setter,
      amount-getter: gene-stimulus-chemical1-amount,
      amount-setter: gene-stimulus-chemical1-amount-setter);

  pane chemical2-pane (frame)
    make(<chemical-and-amount-pane>,
      translator: c3-stimulus-chemical-translator,
      pivot: 124,
      gene: frame.pane-gene,
      chemical-getter: gene-stimulus-chemical2,
      chemical-setter: gene-stimulus-chemical2-setter,
      amount-getter: gene-stimulus-chemical2-amount,
      amount-setter: gene-stimulus-chemical2-amount-setter);


  pane chemical3-pane (frame)
    make(<chemical-and-amount-pane>,
      translator: c3-stimulus-chemical-translator,
      pivot: 124,
      gene: frame.pane-gene,
      chemical-getter: gene-stimulus-chemical3,
      chemical-setter: gene-stimulus-chemical3-setter,
      amount-getter: gene-stimulus-chemical3-amount,
      amount-setter: gene-stimulus-chemical3-amount-setter);

  pane chemical4-pane (frame)
    make(<chemical-and-amount-pane>,
      translator: c3-stimulus-chemical-translator,
      pivot: 124,
      gene: frame.pane-gene,
      chemical-getter: gene-stimulus-chemical4,
      chemical-setter: gene-stimulus-chemical4-setter,
      amount-getter: gene-stimulus-chemical4-amount,
      amount-setter: gene-stimulus-chemical4-amount-setter);

  pane stimulus-pane (frame)
     make(<description-and-amount-pane>,
      gene: frame.pane-gene,
      type-getter: gene-stimulus-type,
      type-setter: gene-stimulus-type-setter,
      amount-getter: gene-stimulus-significance,
      amount-setter: gene-stimulus-significance-setter,
      amount-description: "Significance",
      descriptions: if(frame.pane-gene.gene-genome-version == #"creatures2")
                      $c2-stimulus-descriptions 
                    else 
                      $c3-stimulus-descriptions
                    end if,
      converter: curry(convert-to-stimulus-description, frame.pane-gene.gene-genome-version));

pane sensory-pane (frame)
     make(<description-and-amount-pane>,
      gene: frame.pane-gene,
      type-getter: stimulus-gene-stimulus-sensory-neuron,
      type-setter: stimulus-gene-stimulus-sensory-neuron-setter,
      amount-getter: gene-stimulus-intensity,
      amount-setter: gene-stimulus-intensity-setter,
      amount-description: "Intensity",
      descriptions: if(frame.pane-gene.gene-genome-version == #"creatures2")
                      $c2-stimulus-general-sensory-descriptions 
                    else 
                      $c2-stimulus-general-sensory-descriptions
                    end if,
      converter: curry(stimulus-convert-to-general-sensory-description, frame.pane-gene.gene-genome-version));

  pane stimulus-lhs-pane (frame)
    vertically()
      make(<label>, label: "Stimulus");
      frame.stimulus-pane;
      make(<label>, label: "Sensory Neuron");
      frame.sensory-pane;
      make(<bitflag-pane>, 
        gene: frame.pane-gene,
        getter: gene-stimulus-modulate?,
        setter: gene-stimulus-modulate?-setter,
        description: "Modulate using sensory signal");
      make(<bitflag-pane>, 
        gene: frame.pane-gene,
        getter: gene-stimulus-add-offset?,
        setter: gene-stimulus-add-offset?-setter,
        description: "Add offset to neu (eg. word#)");
      make(<bitflag-pane>, 
        gene: frame.pane-gene,
        getter: gene-stimulus-sensed-asleep?,
        setter: gene-stimulus-sensed-asleep?-setter,
        description: "Detected even if asleep");
    end;

  pane stimulus-rhs-pane (frame)
    vertically()
      frame.chemical1-pane;
      frame.chemical2-pane;
      frame.chemical3-pane;
      frame.chemical4-pane;
    end;

  pane stimulus-gene-main-pane (frame)
    make(<tab-control>, pages: vector(make(<tab-control-page>, 
                                           label: "Stimulus",
                                           child: frame.stimulus-lhs-pane),
                                      make(<tab-control-page>, 
                                           label: "Chemical Adjustments",
                                           child: frame.stimulus-rhs-pane)
                                      ));

  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "Stimulus Data",
                        child: frame.stimulus-gene-main-pane);                   
    end;

end pane <c3-stimulus-gene-pane>;


define method make-pane-for-gene( gene :: <c2-stimulus-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<c2-stimulus-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;

define method make-pane-for-gene( gene :: <c3-stimulus-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<c3-stimulus-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;

