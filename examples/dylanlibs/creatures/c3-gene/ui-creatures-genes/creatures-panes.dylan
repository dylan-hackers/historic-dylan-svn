Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define constant $byte-input-size :: <integer> = 30;


define constant $c3-drive-choices = 
    concatenate(map(curry(get-lobe-cell-description, #"creatures3", 5), range(from: 0, to: 19)),
      map(curry(format-to-string, "Unknown value: %d"), range(from: 20, to: 255)));


define pane <bitflag-pane> (<gene-pane>)
  constant slot pane-bitflag-getter, required-init-keyword: getter:;
  constant slot pane-bitflag-setter, required-init-keyword: setter:;
  constant slot pane-description, required-init-keyword: description:;

  layout (pane)
    make(<check-button>,
      label: pane.pane-description,
      value: pane.pane-bitflag-getter(pane.pane-gene),
      value-changed-callback: method(g) pane.pane-bitflag-setter(g.gadget-value, pane.pane-gene) end);
end pane <bitflag-pane>;


define pane <amount-entry-pane> (<gene-pane>)
  constant slot pane-amount-getter, required-init-keyword: getter:;
  constant slot pane-amount-setter, required-init-keyword: setter:;

  pane amount-pane (pane)
    make(<text-field>, 
      min-width: $byte-input-size,
      max-width: $fill,
      value-type: <integer>,
      value: pane.pane-amount-getter(pane.pane-gene),
      value-changed-callback: 
        method(g)
          pane.pane-amount-setter(g.gadget-value, pane.pane-gene);
        end); 

  layout (pane) horizontally(max-width: 20) pane.amount-pane end;
end pane <amount-entry-pane>;

define macro make-amount-entry-pane
  { make-amount-entry-pane ( ?gene:expression, ?amount-slot:name ) }
 =>
  { make(<amount-entry-pane>, 
      gene: ?gene, 
      getter: ?amount-slot,
      setter: ?amount-slot ## "-setter")
  }
end macro;


define pane <string-entry-pane> (<gene-pane>)
  constant slot pane-string-getter, required-init-keyword: getter:;
  constant slot pane-string-setter, required-init-keyword: setter:;

  pane string-pane (pane)
    make(<text-field>, 
      text: pane.pane-string-getter(pane.pane-gene),
      value-changed-callback: method(g)
        pane.pane-string-setter(g.gadget-value, pane.pane-gene);
      end);

  layout (pane) pane.string-pane;
end pane <string-entry-pane>;

define pane <amount-selection-pane> (<gene-pane>)
  constant slot pane-description :: <string> = "Amount", init-keyword: description:;
  constant slot pane-amount-getter, required-init-keyword: amount-getter:;
  constant slot pane-amount-setter, required-init-keyword: amount-setter:;
  constant slot pane-pivot-value = 0, init-keyword: pivot:;
  constant slot pane-amount-min = 0, init-keyword: min:;
  constant slot pane-amount-max = 255, init-keyword: max:;

  pane amount-value-pane (pane)
    make(<text-field>, 
      value-type: <integer>,
      min-width: $byte-input-size,
      value: pane.pane-amount-getter(pane.pane-gene) - pane.pane-pivot-value,
      value-changed-callback: 
        method(g)
          let value = min(pane.pane-amount-max - pane.pane-pivot-value, g.gadget-value);
          pane.pane-amount-setter(value + pane.pane-pivot-value, pane.pane-gene);
          pane.amount-slider-pane.gadget-value := value;
        end);
        
  pane amount-slider-pane (pane)
    make(<scroll-bar>,
      orientation: #"horizontal",
      value-range: range(from: pane.pane-amount-min - pane.pane-pivot-value,
                         to: pane.pane-amount-max - pane.pane-pivot-value),
      value-changing-callback: method(g)
        pane.pane-amount-setter(g.gadget-value + pane.pane-pivot-value, pane.pane-gene);
        pane.amount-value-pane.gadget-value := pane.pane-amount-getter(pane.pane-gene) - pane.pane-pivot-value;
      end,
      value-changed-callback: method(g)
        pane.pane-amount-setter(g.gadget-value + pane.pane-pivot-value, pane.pane-gene);
        pane.amount-value-pane.gadget-value := pane.pane-amount-getter(pane.pane-gene) - pane.pane-pivot-value;
      end,
      slug-size: 1,
      value: pane.pane-amount-getter(pane.pane-gene) - pane.pane-pivot-value);

  layout (pane)
    make(<table-layout>,
      columns: 3,
      x-alignment: #[#"right", #"left", #"left"],
      y-alignment: #"center",
      x-ratios: #(4, 5, 1),
      children: vector(make(<label>, label: pane.pane-description),
                       pane.amount-slider-pane,
                       pane.amount-value-pane));
end pane <amount-selection-pane>;

define function within-range( minimum :: <integer>, maximum :: <integer>, value :: <integer> )
 => (result :: <integer>)
  max(minimum, min(maximum, value));
end function within-range;

define function within-byte-range( value :: <integer> )
 => (result :: <byte>)
  within-range(0, 255, value);
end function within-byte-range;

define method create-amount-selection-table( gene, descriptions, getters, setters )
  local method build-children( ) 
      let result = make(<stretchy-vector>);
      for( description in descriptions, getter in getters, setter in setters )
        let current-value :: <byte> = getter(gene);

        let value-pane = make(<text-field>, 
                              min-width: $byte-input-size,
                              value-type: <integer>,
                              value: current-value);

        let slider-pane = make(<scroll-bar>,
                               orientation: #"horizontal",
                               value-range: range(from: 0, to: 255),
                               slug-size: 1,
                               value: current-value);

        value-pane.gadget-value-changed-callback := method(g)
                                let value :: <byte> = within-byte-range(g.gadget-value);
                                g.gadget-value := value;
                                setter(value, gene);
                                slider-pane.gadget-value := value;
                              end;

        let value-change-method = 
          method(g)
            let value :: <byte> = g.gadget-value;
            setter(value, gene);
            value-pane.gadget-value := value;
          end;

        slider-pane.gadget-value-changing-callback := value-change-method;
        slider-pane.gadget-value-changed-callback := value-change-method;

        add!(result, make(<label>, label: description));
        add!(result, slider-pane);
        add!(result, value-pane);
      finally
        result;
      end for;
  end method build-children;

  make(<table-layout>,
       columns: 3,
       x-alignment: #[#"right", #"left", #"left"],
       y-alignment: #"center",
       x-ratios: #(4, 5, 1),
       children: build-children());
end method create-amount-selection-table;

define pane <amount-selection-table-pane> (<gene-pane>)
  slot pane-descriptions, init-keyword: descriptions:;
  constant slot pane-amount-getters, required-init-keyword: getters:;
  constant slot pane-amount-setters, required-init-keyword: setters:;

  layout (pane)
    create-amount-selection-table( pane.pane-gene, 
      pane.pane-descriptions,
      pane.pane-amount-getters,
      pane.pane-amount-setters );
end pane <amount-selection-table-pane>;

define method get-converter( pane )
    method(v)
      let value = pane.pane-translator(v);
      if(value > 255)
        format-to-string("Unknown value in genome: %d", v)
      else
        pane.pane-description-converter(value);
      end if;
    end method;    
end method get-converter;

define constant $default-valid-items = range(from: 0, to: 255);

define pane <description-selection-pane> (<gene-pane>)
  constant slot pane-type-getter, required-init-keyword: getter:;
  constant slot pane-type-setter, required-init-keyword: setter:;
  slot pane-description-converter, required-init-keyword: converter:;
  constant slot pane-value-changed-callback = #f, init-keyword: value-changed-callback:;
  constant slot pane-translator = identity, init-keyword: translator:;

  pane description-pane (pane)
    make(<option-box>, 
      items: $default-valid-items,
      value: pane.pane-type-getter(pane.pane-gene),
      label-key: get-converter(pane), 
      value-changed-callback: method(g) 
                                pane.pane-type-setter(
                                  g.gadget-value, 
                                  pane.pane-gene);
                                if(pane.pane-value-changed-callback) 
                                  pane.pane-value-changed-callback(pane);
                                end if;
                              end);

  layout (pane)
    pane.description-pane;
end pane <description-selection-pane>;

define method update-description-selection( pane :: <description-selection-pane>, converter )
    pane.pane-description-converter := converter;
    pane.description-pane.gadget-items := $default-valid-items;
    pane.description-pane.gadget-value := 0;
    update-gadget( pane.description-pane );
end method update-description-selection;

define pane <description-and-amount-pane> (<gene-pane>)
  constant slot pane-type-getter, required-init-keyword: type-getter:;
  constant slot pane-type-setter, required-init-keyword: type-setter:;
  constant slot pane-amount-getter, required-init-keyword: amount-getter:;
  constant slot pane-amount-setter, required-init-keyword: amount-setter:;
  constant slot pane-amount-description = "Amount", init-keyword: amount-description:;
  slot pane-descriptions, required-init-keyword: descriptions:;
  slot pane-description-converter, required-init-keyword: converter:;
  constant slot pane-translator = identity, init-keyword: translator:;
  constant slot pane-pivot-value = 0, init-keyword: pivot:;

  pane type-pane (pane)
    make(<description-selection-pane>,
      translator: pane.pane-translator,
      gene: pane.pane-gene,
      getter: pane.pane-type-getter,
      setter: pane.pane-type-setter,
      descriptions: pane.pane-descriptions,
      converter: pane.pane-description-converter);

  pane amount-pane (pane)
    make(<amount-selection-pane>, 
      pivot: pane.pane-pivot-value,
      gene: pane.pane-gene,
      description: pane.pane-amount-description,
      amount-getter: pane.pane-amount-getter,
      amount-setter: pane.pane-amount-setter);

  layout (pane)
    vertically()
      pane.type-pane;
      pane.amount-pane;
    end;

end pane <description-and-amount-pane>;

define pane <drive-pane> (<gene-pane>)
  constant slot pane-drive-getter, required-init-keyword: drive-getter:;
  constant slot pane-drive-setter, required-init-keyword: drive-setter:;
  constant slot pane-translator = identity, init-keyword: translator:;

  pane drive-pane (pane)
    make(<description-selection-pane>,
      translator: pane.pane-translator,
      gene: pane.pane-gene,
      getter: pane.pane-drive-getter,
      setter: pane.pane-drive-setter,
      descriptions: $c3-drive-descriptions,
      converter:  method(x) 
                   element($c3-drive-choices, x, 
                           default: format-to-string("Unknown value: %d", x))
                  end);

  layout (pane)
    pane.drive-pane;

end pane <drive-pane>;

define pane <drive-and-amount-pane> (<gene-pane>)
  constant slot pane-drive-getter, required-init-keyword: drive-getter:;
  constant slot pane-drive-setter, required-init-keyword: drive-setter:;
  constant slot pane-amount-getter, required-init-keyword: amount-getter:;
  constant slot pane-amount-setter, required-init-keyword: amount-setter:;
  constant slot pane-translator = identity, init-keyword: translator:;
  constant slot pane-pivot-value = 124, init-keyword: pivot:;

  layout (pane)
    make(<description-and-amount-pane>,    
      pivot: pane.pane-pivot-value,
      translator: pane.pane-translator,
      gene: pane.pane-gene,
      type-getter: pane.pane-drive-getter,
      type-setter: pane.pane-drive-setter,
      amount-getter: pane.pane-amount-getter,
      amount-setter: pane.pane-amount-setter,
      descriptions: $c3-drive-descriptions,
      converter:  method(x) 
                   element($c3-drive-choices, x, 
                           default: format-to-string("Unknown value: %d", x))
                  end);

end pane <drive-and-amount-pane>;


define pane <chemical-pane> (<gene-pane>)
  constant slot pane-chemical-getter, required-init-keyword: chemical-getter:;
  constant slot pane-chemical-setter, required-init-keyword: chemical-setter:;
  constant slot pane-translator = identity, init-keyword: translator:;

  pane chemical-pane (pane)
    make(<description-selection-pane>,
      translator: pane.pane-translator,
      gene: pane.pane-gene,
      getter: pane.pane-chemical-getter,
      setter: pane.pane-chemical-setter,
      converter: curry(convert-to-chemical-description, pane.pane-gene.gene-genome-version));

  layout (pane)
    pane.chemical-pane;

end pane <chemical-pane>;

define macro make-chemical-pane
  { make-chemical-pane ( ?gene:expression, ?chemical-slot:name ) }
 =>
  { make(<chemical-pane>, 
      gene: ?gene, 
      chemical-getter: ?chemical-slot,
      chemical-setter: ?chemical-slot ## "-setter")
  }
end macro;

define pane <chemical-and-amount-pane> (<gene-pane>)
  constant slot pane-chemical-getter, required-init-keyword: chemical-getter:;
  constant slot pane-chemical-setter, required-init-keyword: chemical-setter:;
  constant slot pane-amount-getter, required-init-keyword: amount-getter:;
  constant slot pane-amount-setter, required-init-keyword: amount-setter:;
  constant slot pane-translator = identity, init-keyword: translator:;
  constant slot pane-pivot-value = 0, init-keyword: pivot:;

  layout (pane)
    make(<description-and-amount-pane>,
      pivot: pane.pane-pivot-value,
      translator: pane.pane-translator,
      gene: pane.pane-gene,
      type-getter: pane.pane-chemical-getter,
      type-setter: pane.pane-chemical-setter,
      amount-getter: pane.pane-amount-getter,
      amount-setter: pane.pane-amount-setter,
      descriptions: $chemical-descriptions,
      converter: curry(convert-to-chemical-description, pane.pane-gene.gene-genome-version));

end pane <chemical-and-amount-pane>;

define function get-svrule-text(svrule :: <svrule>)
    let complete = #f;
    let text = "";
    for(opcode in svrule)
      if(~complete)
        text := concatenate(text, ":", as(<string>, opcode));
        if(opcode = #"<end>")
          complete := #t;
        end if;
      end if;
    finally
      text;
    end for;
end function get-svrule-text;

define pane <svrule-edit-pane> (<gene-pane>)
  constant slot pane-description = "State Rule...", init-keyword: description:;
  constant slot pane-svrule-getter, required-init-keyword: getter:;
//  slot pane-svrule-setter, required-init-keyword: setter:;

  pane svrule-text-pane (pane)
    make(<text-field>,
      read-only?: #t,
      text: get-svrule-text(pane.pane-svrule-getter(pane.pane-gene)));

  layout (pane)
    horizontally( max-width: $fill )
      make(<push-button>, 
        label: pane.pane-description,
        activate-callback: get-state-rule-button-method(pane));
      pane.svrule-text-pane;
    end;

end pane <svrule-edit-pane>;

define method make-opcode-editor(svrule)
    let result = make(<stretchy-vector>);
    for(index from 0 below svrule.size)
      add!(result, make(<option-box>,
                        items: key-sequence($svrule-opcode-values),
                        value: svrule[index],
                        label-key: curry(convert-to-description, <svrule-opcode>),
                        value-changed-callback: method(g) svrule[index] := g.gadget-value end));
    end for;
    result;
end method make-opcode-editor;
define frame <svrule-edit-dialog> (<dialog-frame>)
  constant slot dialog-svrule :: <svrule>, required-init-keyword: svrule:;

  layout (frame)
    make(<table-layout>, 
      rows: 3,
      columns: 4,
      children: make-opcode-editor(frame.dialog-svrule));

end frame <svrule-edit-dialog>;

define method get-state-rule-button-method(pane)
    method(gadget)
      let frame = sheet-frame(gadget);
    
      start-frame(make(<svrule-edit-dialog>, 
                       owner: frame, 
                       svrule: pane.pane-svrule-getter(pane.pane-gene)));
      pane.svrule-text-pane.gadget-value :=  get-svrule-text(pane.pane-svrule-getter(pane.pane-gene));
    end method;
end method get-state-rule-button-method;



