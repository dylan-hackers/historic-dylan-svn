Module:    gene-compare
Synopsis:  A program to compare Creature gene files.
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define open generic gene-equal( lhs :: <gene>, rhs :: <gene> ) => (result :: <boolean>);


define method gene-equal( lhs :: <gene>, rhs :: <gene> ) => (result :: <boolean>)
  lhs.object-class == rhs.object-class &
  lhs.gene-type == rhs.gene-type &
  lhs.gene-subtype == rhs.gene-subtype &
  lhs.gene-sequence-number == rhs.gene-sequence-number &
  lhs.gene-duplicate-number == rhs.gene-duplicate-number &
  lhs.gene-switch-on-stage == rhs.gene-switch-on-stage &
  lhs.gene-allow-duplicates? == rhs.gene-allow-duplicates? &
  lhs.gene-allow-mutations? == rhs.gene-allow-mutations? &
  lhs.gene-allow-deletions? == rhs.gene-allow-deletions? &
  lhs.gene-is-dormant? == rhs.gene-is-dormant? &
  lhs.gene-switch-on-gender == rhs.gene-switch-on-gender &
  lhs.gene-mutation-percentage == rhs.gene-mutation-percentage &
  lhs.gene-other-flags == rhs.gene-other-flags &
  lhs.gene-variant == rhs.gene-variant;
end method gene-equal;

define method gene-equal( lhs :: <c3-brain-lobe-gene>, rhs :: <c3-brain-lobe-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-brain-lobe-name = rhs.gene-brain-lobe-name &
  lhs.gene-brain-lobe-x-position == rhs.gene-brain-lobe-x-position &
  lhs.gene-brain-lobe-y-position == rhs.gene-brain-lobe-y-position &
  lhs.gene-brain-lobe-width == rhs.gene-brain-lobe-width &
  lhs.gene-brain-lobe-height == rhs.gene-brain-lobe-height &
  lhs.gene-brain-lobe-unknown = rhs.gene-brain-lobe-unknown;  
end method gene-equal;

define method gene-equal( lhs :: <brain-tract-gene>, rhs :: <brain-tract-gene> ) => (result :: <boolean>)
  next-method() &
    lhs.gene-brain-tract-unknown1 == rhs.gene-brain-tract-unknown1 &
    lhs.gene-brain-tract-source-lobe = rhs.gene-brain-tract-source-lobe &
    lhs.gene-brain-tract-source-lower == rhs.gene-brain-tract-source-lower &
    lhs.gene-brain-tract-source-upper == rhs.gene-brain-tract-source-upper &
    lhs.gene-brain-tract-source-amount == rhs.gene-brain-tract-source-amount &
    lhs.gene-brain-tract-destination-lobe = rhs.gene-brain-tract-destination-lobe &
    lhs.gene-brain-tract-destination-lower == rhs.gene-brain-tract-destination-lower &
    lhs.gene-brain-tract-destination-upper == rhs.gene-brain-tract-destination-upper &
    lhs.gene-brain-tract-destination-amount == rhs.gene-brain-tract-destination-amount &
    lhs.gene-brain-tract-unknown = rhs.gene-brain-tract-unknown;
end method gene-equal;

define method gene-equal( lhs :: <receptor-gene>, rhs :: <receptor-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-receptor-organ == rhs.gene-receptor-organ &
  lhs.gene-receptor-tissue == rhs.gene-receptor-tissue &
  lhs.gene-receptor-locus == rhs.gene-receptor-locus &
  lhs.gene-receptor-chemical == rhs.gene-receptor-chemical &
  lhs.gene-receptor-threshold == rhs.gene-receptor-threshold &
  lhs.gene-receptor-nominal == rhs.gene-receptor-nominal &
  lhs.gene-receptor-gain == rhs.gene-receptor-gain &
  lhs.gene-receptor-is-digital? == rhs.gene-receptor-is-digital? &
  lhs.gene-receptor-is-inverted? == rhs.gene-receptor-is-inverted? &
  lhs.gene-receptor-other-flags == rhs.gene-receptor-other-flags;
end method gene-equal;

define method gene-equal( lhs :: <emitter-gene>, rhs :: <emitter-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-emitter-organ == rhs.gene-emitter-organ &
  lhs.gene-emitter-tissue == rhs.gene-emitter-tissue &
  lhs.gene-emitter-locus == rhs.gene-emitter-locus &
  lhs.gene-emitter-chemical == rhs.gene-emitter-chemical &
  lhs.gene-emitter-threshold == rhs.gene-emitter-threshold &
  lhs.gene-emitter-sample-rate == rhs.gene-emitter-sample-rate &
  lhs.gene-emitter-gain == rhs.gene-emitter-gain &
  lhs.gene-emitter-is-digital? == rhs.gene-emitter-is-digital? &
  lhs.gene-emitter-clear-source? == rhs.gene-emitter-clear-source? &
  lhs.gene-emitter-is-inverted? == rhs.gene-emitter-is-inverted? &
  lhs.gene-emitter-other-flags == rhs.gene-emitter-other-flags;
end method gene-equal;

define method gene-equal( lhs :: <neuro-emitter-gene>, rhs :: <neuro-emitter-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-neuro-emitter-lobe1 == rhs.gene-neuro-emitter-lobe1 &
  lhs.gene-neuro-emitter-lobe1-cell == rhs.gene-neuro-emitter-lobe1-cell &
  lhs.gene-neuro-emitter-lobe2 == rhs.gene-neuro-emitter-lobe2 &
  lhs.gene-neuro-emitter-lobe2-cell == rhs.gene-neuro-emitter-lobe2-cell &
  lhs.gene-neuro-emitter-lobe3 == rhs.gene-neuro-emitter-lobe3 &
  lhs.gene-neuro-emitter-lobe3-cell == rhs.gene-neuro-emitter-lobe3-cell &
  lhs.gene-neuro-emitter-sample-rate == rhs.gene-neuro-emitter-sample-rate &
  lhs.gene-neuro-emitter-chemical1 == rhs.gene-neuro-emitter-chemical1 &
  lhs.gene-neuro-emitter-chemical1-amount == rhs.gene-neuro-emitter-chemical1-amount &
  lhs.gene-neuro-emitter-chemical2 == rhs.gene-neuro-emitter-chemical2 &
  lhs.gene-neuro-emitter-chemical2-amount == rhs.gene-neuro-emitter-chemical2-amount &
  lhs.gene-neuro-emitter-chemical3 == rhs.gene-neuro-emitter-chemical3 &
  lhs.gene-neuro-emitter-chemical3-amount == rhs.gene-neuro-emitter-chemical3-amount &
  lhs.gene-neuro-emitter-chemical4 == rhs.gene-neuro-emitter-chemical4 &
  lhs.gene-neuro-emitter-chemical4-amount == rhs.gene-neuro-emitter-chemical4-amount;
end method gene-equal;

define method gene-equal( lhs :: <reaction-gene>, rhs :: <reaction-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-reaction-lhs1-amount == rhs.gene-reaction-lhs1-amount &
  lhs.gene-reaction-lhs1 == rhs.gene-reaction-lhs1 &
  lhs.gene-reaction-lhs2-amount == rhs.gene-reaction-lhs2-amount &
  lhs.gene-reaction-lhs2 == rhs.gene-reaction-lhs2 &
  lhs.gene-reaction-rhs1-amount == rhs.gene-reaction-rhs1-amount &
  lhs.gene-reaction-rhs1 == rhs.gene-reaction-rhs1 &
  lhs.gene-reaction-rhs2-amount == rhs.gene-reaction-rhs2-amount &
  lhs.gene-reaction-rhs2 == rhs.gene-reaction-rhs2 &
  lhs.gene-reaction-rate == rhs.gene-reaction-rate;
end method gene-equal;

define method gene-equal( lhs :: <half-lives-gene>, rhs :: <half-lives-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-half-lives = rhs.gene-half-lives;
end method gene-equal;

define method gene-equal( lhs :: <initial-concentration-gene>, rhs :: <initial-concentration-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-initial-concentration-chemical == rhs.gene-initial-concentration-chemical &
  lhs.gene-initial-concentration-amount == rhs.gene-initial-concentration-amount;
end method gene-equal;

define method gene-equal( lhs :: <stimulus-gene>, rhs :: <stimulus-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-stimulus-type == rhs.gene-stimulus-type &
  lhs.gene-stimulus-significance == rhs.gene-stimulus-significance &
  lhs.gene-stimulus-sensory-neuron == rhs.gene-stimulus-sensory-neuron &
  lhs.gene-stimulus-intensity == rhs.gene-stimulus-intensity &
  lhs.gene-stimulus-modulate? == rhs.gene-stimulus-modulate? &
  lhs.gene-stimulus-add-offset? == rhs.gene-stimulus-add-offset? &
  lhs.gene-stimulus-sensed-asleep? == rhs.gene-stimulus-sensed-asleep? &
  lhs.gene-stimulus-other-feature == rhs.gene-stimulus-other-feature &
  lhs.gene-stimulus-chemical1 == rhs.gene-stimulus-chemical1 &
  lhs.gene-stimulus-chemical1-amount == rhs.gene-stimulus-chemical1-amount &
  lhs.gene-stimulus-chemical2 == rhs.gene-stimulus-chemical2 &
  lhs.gene-stimulus-chemical2-amount == rhs.gene-stimulus-chemical2-amount &
  lhs.gene-stimulus-chemical3 == rhs.gene-stimulus-chemical3 &
  lhs.gene-stimulus-chemical3-amount == rhs.gene-stimulus-chemical3-amount &
  lhs.gene-stimulus-chemical4 == rhs.gene-stimulus-chemical4 &
  lhs.gene-stimulus-chemical4-amount == rhs.gene-stimulus-chemical4-amount;
end method gene-equal;

define method gene-equal( lhs :: <genus-gene>, rhs :: <genus-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-genus-species == rhs.gene-genus-species;
end method gene-equal;

define method gene-equal( lhs :: <c2-genus-gene>, rhs :: <c2-genus-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-genus-mum = rhs.gene-genus-mum &
  lhs.gene-genus-dad = rhs.gene-genus-dad;
end method gene-equal;


define method gene-equal( lhs :: <c3-genus-gene>, rhs :: <c3-genus-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-genus-data = rhs.gene-genus-data;
end method gene-equal;

define method gene-equal( lhs :: <appearance-gene>, rhs :: <appearance-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-appearance-body-part == rhs.gene-appearance-body-part &
  lhs.gene-appearance-breed == rhs.gene-appearance-breed &
  lhs.gene-appearance-species == rhs.gene-appearance-species;
end method gene-equal;

define method gene-equal( lhs :: <pose-gene>, rhs :: <pose-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-pose == rhs.gene-pose &
  lhs.gene-pose-data = rhs.gene-pose-data;
end method gene-equal;

define method gene-equal( lhs :: <gait-gene>, rhs :: <gait-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-gait == rhs.gene-gait &
  lhs.gene-gait-data = rhs.gene-gait-data;
end method gene-equal;

define method gene-equal( lhs :: <instinct-gene>, rhs :: <instinct-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-instinct-lobe1 == rhs.gene-instinct-lobe1 &
  lhs.gene-instinct-lobe1-cell == rhs.gene-instinct-lobe1-cell &
  lhs.gene-instinct-lobe2 == rhs.gene-instinct-lobe2 &
  lhs.gene-instinct-lobe2-cell == rhs.gene-instinct-lobe2-cell &
  lhs.gene-instinct-lobe3 == rhs.gene-instinct-lobe3 &
  lhs.gene-instinct-lobe3-cell == rhs.gene-instinct-lobe3-cell &
  lhs.gene-instinct-decision == rhs.gene-instinct-decision &
  lhs.gene-instinct-chemical == rhs.gene-instinct-chemical &
  lhs.gene-instinct-chemical-amount == rhs.gene-instinct-chemical-amount;
end method gene-equal;

define method gene-equal( lhs :: <pigment-gene>, rhs :: <pigment-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-pigment-color == rhs.gene-pigment-color &
  lhs.gene-pigment-intensity == rhs.gene-pigment-intensity;
end method gene-equal;

define method gene-equal( lhs :: <pigment-bleed-gene>, rhs :: <pigment-bleed-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-pigment-bleed-rotation == rhs.gene-pigment-bleed-rotation &
  lhs.gene-pigment-bleed-swap == rhs.gene-pigment-bleed-swap;
end method gene-equal;

define method gene-equal( lhs :: <facial-expression-gene>, rhs :: <facial-expression-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-expression-number == rhs.gene-expression-number &
  lhs.gene-expression-weight == rhs.gene-expression-weight &
  lhs.gene-expression-drive1 == rhs.gene-expression-drive1 &
  lhs.gene-expression-drive1-amount == rhs.gene-expression-drive1-amount &
  lhs.gene-expression-drive2 == rhs.gene-expression-drive2 &
  lhs.gene-expression-drive2-amount == rhs.gene-expression-drive2-amount &
  lhs.gene-expression-drive3 == rhs.gene-expression-drive3 &
  lhs.gene-expression-drive3-amount == rhs.gene-expression-drive3-amount &
  lhs.gene-expression-drive4 == rhs.gene-expression-drive4 &
  lhs.gene-expression-drive4-amount == rhs.gene-expression-drive4-amount;
end method gene-equal;

define method gene-equal( lhs :: <organ-gene>, rhs :: <organ-gene> ) => (result :: <boolean>)
  next-method() &
  lhs.gene-organ-clock-rate == rhs.gene-organ-clock-rate &
  lhs.gene-organ-life-force-repair-rate == rhs.gene-organ-life-force-repair-rate &
  lhs.gene-organ-life-force-start == rhs.gene-organ-life-force-start &
  lhs.gene-organ-biotick-start == rhs.gene-organ-biotick-start &
  lhs.gene-organ-atp-damage-coefficient == rhs.gene-organ-atp-damage-coefficient;
end method gene-equal;
  

define method find-gene( looking-for :: <gene>, genes )
  local method gene-equal( gene :: <gene> )
    gene.gene-type == looking-for.gene-type &
    gene.gene-subtype == looking-for.gene-subtype &
    gene.gene-sequence-number == looking-for.gene-sequence-number &
    gene.gene-duplicate-number == looking-for.gene-duplicate-number &
    gene.gene-variant == looking-for.gene-variant;
  end method;

  find-element(genes, gene-equal);
end method find-gene;
   

define method compute-differences( lhs :: <genome>, rhs :: <genome> )
  let different = make(<stretchy-vector>);
  let new-in-lhs = make(<stretchy-vector>);
  let new-in-rhs = make(<stretchy-vector>);
  for(gene in lhs.genome-genes)
    let gene-in-rhs = find-gene(gene, rhs.genome-genes);
    if(gene-in-rhs)
      unless(gene-equal(gene, gene-in-rhs))
        different := add!(different, pair(gene, gene-in-rhs))
      end;
    else
      new-in-lhs := add!(new-in-lhs, gene);
    end;
  end for;

  for(gene in rhs.genome-genes)
    let gene-in-lhs = find-gene(gene, lhs.genome-genes);
    unless(gene-in-lhs)
      new-in-rhs := add!(new-in-rhs, gene);
    end;
  end for;
      
  values(different, new-in-lhs, new-in-rhs);    
end method;

