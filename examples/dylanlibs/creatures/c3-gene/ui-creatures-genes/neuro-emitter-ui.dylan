Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define method make-neuro-emitter-getter( original-getter )
  method ( gene )
    let value = original-getter(gene);
    if(zero?(value) | value = 255)
      value
    else
      value - 1;
    end;
  end method;
end method;

define method make-neuro-emitter-setter( original-setter )
  method( value, gene )  
    original-setter( if(zero?(value) | value = 255)
                       value
                     else
                       value + 1
                     end if, gene );
  end;
end method;



define macro make-neuro-emitter-lobe-cell-pane
  { make-neuro-emitter-lobe-cell-pane( ?frame:expression, ?lobe-number:name ) }
 => 
  { 
    make(<instinct-lobe-cell-pane>,
      gene: ?frame.pane-gene,
      lobe-converter: method(b) if(b >= 254) "<no lobe>" else get-lobe-description(?frame.pane-gene.gene-genome-version, b) end end,
      lobe-getter: make-neuro-emitter-getter("gene-neuro-emitter-" ## ?lobe-number),
      lobe-setter: make-neuro-emitter-setter("gene-neuro-emitter-" ## ?lobe-number ## "-setter"),
      cell-getter: "gene-neuro-emitter-" ## ?lobe-number ## "-cell",
      cell-setter: "gene-neuro-emitter-" ## ?lobe-number ## "-cell-setter")
  }
end macro;


define pane <neuro-emitter-gene-pane> (<gene-pane>)
  pane lobe1-pane (frame)
    make-neuro-emitter-lobe-cell-pane( frame, lobe1 );

  pane lobe2-pane (frame)
    make-neuro-emitter-lobe-cell-pane( frame, lobe2 );

  pane lobe3-pane (frame)
    make-neuro-emitter-lobe-cell-pane( frame, lobe3 );

  pane sample-rate-pane (pane)
      make(<amount-selection-pane>, 
        description: "Rate",
        gene: pane.pane-gene,
        amount-getter: gene-neuro-emitter-sample-rate,
        amount-setter: gene-neuro-emitter-sample-rate-setter);

  pane chemical1-pane (frame)
    make(<chemical-and-amount-pane>,
      gene: frame.pane-gene,
      chemical-getter: gene-neuro-emitter-chemical1,
      chemical-setter: gene-neuro-emitter-chemical1-setter,
      amount-getter: gene-neuro-emitter-chemical1-amount,
      amount-setter: gene-neuro-emitter-chemical1-amount-setter);

  pane chemical2-pane (frame)
    make(<chemical-and-amount-pane>,
      gene: frame.pane-gene,
      chemical-getter: gene-neuro-emitter-chemical2,
      chemical-setter: gene-neuro-emitter-chemical2-setter,
      amount-getter: gene-neuro-emitter-chemical2-amount,
      amount-setter: gene-neuro-emitter-chemical2-amount-setter);

  pane chemical3-pane (frame)
    make(<chemical-and-amount-pane>,
      gene: frame.pane-gene,
      chemical-getter: gene-neuro-emitter-chemical3,
      chemical-setter: gene-neuro-emitter-chemical3-setter,
      amount-getter: gene-neuro-emitter-chemical3-amount,
      amount-setter: gene-neuro-emitter-chemical3-amount-setter);

  pane chemical4-pane (frame)
    make(<chemical-and-amount-pane>,
      gene: frame.pane-gene,
      chemical-getter: gene-neuro-emitter-chemical4,
      chemical-setter: gene-neuro-emitter-chemical4-setter,
      amount-getter: gene-neuro-emitter-chemical4-amount,
      amount-setter: gene-neuro-emitter-chemical4-amount-setter);

  pane neuro-emitter-gene-main-pane (frame)
    vertically()
      make(<group-box>, label: "When this is true (lobe/cell)",
                        child: vertically()
                                 frame.lobe1-pane;
                                 frame.lobe2-pane;
                                 frame.lobe3-pane;
                               end);                   
      frame.sample-rate-pane;
      frame.chemical1-pane;
      frame.chemical2-pane;
      frame.chemical3-pane;
      frame.chemical4-pane;
    end;


  layout (frame)
    vertically()
      make(<gene-header-pane>, gene: frame.pane-gene);
      make(<group-box>, label: "NeuroEmitter Data",
                        child: frame.neuro-emitter-gene-main-pane);                   
    end;

end pane <neuro-emitter-gene-pane>;

define method make-pane-for-gene( gene :: <neuro-emitter-gene>, #key genome )
  => (pane :: <gene-pane>)
      make(<neuro-emitter-gene-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


