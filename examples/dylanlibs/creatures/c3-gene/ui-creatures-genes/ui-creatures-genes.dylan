Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define class <gene-pane> (<object>)
  constant slot pane-gene, required-init-keyword: gene:;
  constant slot pane-genome = #f, init-keyword: genome:;
end class <gene-pane>;

define generic make-pane-for-gene( gene :: <gene>, #key genome ) => (pane :: <gene-pane>);


define pane <gene-header-pane> (<gene-pane>)
  pane switch-on-stage-pane (pane)
    make(<option-box>, 
      items: map( convert-to-switch-on-stage, range(from: 0, to: 6)),
      value: pane.pane-gene.gene-switch-on-stage,
      label-key: curry(convert-to-description, <switch-on-stage>),
      value-changed-callback: method(g) pane.pane-gene.gene-switch-on-stage := g.gadget-value end);

  pane gender-pane (pane)
    make(<option-box>, 
      items: key-sequence($gender-descriptions),
      value: pane.pane-gene.gene-switch-on-gender,
      label-key: curry(convert-to-description, <gender>),
      value-changed-callback: method(g) pane.pane-gene.gene-switch-on-gender := g.gadget-value end);

  pane allow-duplicates-pane (pane)
    make(<bitflag-pane>, 
      gene: pane.pane-gene,
            description: "Dup",
            getter: gene-allow-duplicates?,
            setter: gene-allow-duplicates?-setter);

  pane allow-mutations-pane (pane)
    make(<bitflag-pane>,
      gene: pane.pane-gene,
      description: "Mut",
      getter: gene-allow-mutations?,
      setter: gene-allow-mutations?-setter);

  pane allow-deletions-pane (pane)
    make(<bitflag-pane>,
      gene: pane.pane-gene,
      description: "Cut",
      getter: gene-allow-deletions?,
      setter: gene-allow-deletions?-setter);

  pane is-dormant-pane (pane)
    make(<bitflag-pane>,
      gene: pane.pane-gene,
      description: "Dormant",
      getter: gene-is-dormant?,
      setter: gene-is-dormant?-setter);

  pane mutation-percentage-pane (pane)
    make(<text-field>,
      value-type: <integer>,
      text: format-to-string("%d", pane.pane-gene.gene-mutation-percentage),      
      value-changed-callback: method(g) pane.pane-gene.gene-mutation-percentage := g.gadget-value end);

  pane c2-main-pane (pane)
    vertically( spacing: 2, max-width: 200)
      horizontally(  )
        pane.switch-on-stage-pane;
        pane.gender-pane;
      end;
      horizontally()
        pane.allow-duplicates-pane;
        pane.allow-mutations-pane;
        pane.allow-deletions-pane;
        pane.is-dormant-pane;
      end;
      horizontally()
        make(<label>, label: "Mutation Chance:");
        pane.mutation-percentage-pane;      
      end;
    end;

  pane c3-main-pane (pane)
    vertically( spacing: 2, max-width: 200)
      horizontally(  )
        pane.switch-on-stage-pane;
        pane.gender-pane;
      end;
      horizontally()
        pane.allow-duplicates-pane;
        pane.allow-mutations-pane;
        pane.allow-deletions-pane;
        pane.is-dormant-pane;
      end;
      horizontally()
        make(<label>, label: "Mutation Chance:");
        pane.mutation-percentage-pane;      
      end;
      horizontally()
        make(<label>, label: "Variant:");
        make(<text-field>,
          value-type: <integer>,
          text: format-to-string("%d", pane.pane-gene.gene-variant),      
          value-changed-callback: method(g) pane.pane-gene.gene-variant := g.gadget-value end);
      end
    end;


  layout (pane)
    if(pane.pane-gene.gene-genome-version = #"creatures3")
      make(<group-box>, label: "Gene Header",
                          child: pane.c3-main-pane);                   
    else
      make(<group-box>, label: "Gene Header",
                          child: pane.c2-main-pane);                   
    end if;

end pane <gene-header-pane>;
define method make-pane-for-gene( gene :: <gene>, #key genome )
  => (pane :: <gene-pane>)
    make(<gene-header-pane>, gene: gene, genome: genome);
end method make-pane-for-gene;


