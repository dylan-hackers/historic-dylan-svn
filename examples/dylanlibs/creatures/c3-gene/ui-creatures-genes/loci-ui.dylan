Module:    ui-creatures-genes
Synopsis:  Frames for the standard Creatures genes
Author:    Chris Double
Copyright: (C) 1998, Chris Double.  All rights reserved.
License:   See License.txt

define method get-organ-description( gene, genome ) => (result :: <string>)
    let current-organ = #f;
    let description = #f;
    for( gene-item in genome )
      if(gene-item.item-gene.type-for-copy = <organ-gene>)
        current-organ := gene-item;
      end if;
      if(gene-item.item-gene = gene & current-organ)
        description := if( current-organ.item-gno)
                         current-organ.item-gno.gno-description;
                       else
                         current-organ.item-gene.gene-short-description;
                       end if;      
      end if;
    end for;
    description | "No Organ (Invalid genome?)";
end method get-organ-description;

define method internal-receptor-tissue-description( gene, genome, organ :: <byte>, tissue :: <byte> ) => (result :: <string>)
    if( organ = 2 )
      get-organ-description(gene, genome);
    else
      get-receptor-tissue-description( gene.gene-genome-version, organ, tissue );
    end if;
end method internal-receptor-tissue-description;

define method internal-emitter-tissue-description( gene, genome, organ :: <byte>, tissue :: <byte> ) => (result :: <string>)
    if( organ = 2 )
      get-organ-description(gene, genome);
    else
      get-emitter-tissue-description( gene.gene-genome-version, organ, tissue );
    end if;
end method internal-emitter-tissue-description;

define pane <receptor-loci-pane> (<gene-pane>)

  pane organ-pane (pane)
    make(<description-selection-pane>,
         value-changed-callback: method(p) 
           let organ = p.description-pane.gadget-value;
           update-description-selection( pane.tissue-pane, 
             curry(internal-receptor-tissue-description, pane.pane-gene, pane.pane-genome, 
                            organ));
           update-description-selection( pane.locus-pane, 
             curry(get-receptor-locus-description, 
                          pane.pane-gene.gene-genome-version,
                          pane.organ-pane.description-pane.gadget-value,
                          pane.tissue-pane.description-pane.gadget-value));
         end method,           
         gene: pane.pane-gene,
         getter: gene-receptor-organ,
         setter: gene-receptor-organ-setter,
         descriptions: map(curry(get-receptor-organ-description, pane.pane-gene.gene-genome-version), 
                           get-receptor-organ-range(pane.pane-gene.gene-genome-version)),
         converter: curry(get-receptor-organ-description, pane.pane-gene.gene-genome-version));

  pane tissue-pane (pane)
    make(<description-selection-pane>,
         gene: pane.pane-gene,
         value-changed-callback: method(p) 
           let tissue = p.description-pane.gadget-value;
           update-description-selection( pane.locus-pane, 
             curry(get-receptor-locus-description, 
                          pane.pane-gene.gene-genome-version,
                          pane.organ-pane.description-pane.gadget-value,
                          pane.tissue-pane.description-pane.gadget-value));
         end method,           
         getter: gene-receptor-tissue,
         setter: gene-receptor-tissue-setter,
         descriptions: 
           map(curry(get-receptor-tissue-description, 
                     pane.pane-gene.gene-genome-version,
                     pane.organ-pane.description-pane.gadget-value),
             get-receptor-tissue-range(pane.pane-gene.gene-genome-version, pane.organ-pane.description-pane.gadget-value)),
         converter: curry(internal-receptor-tissue-description, pane.pane-gene, pane.pane-genome, 
                            pane.organ-pane.description-pane.gadget-value));

  pane locus-pane (pane)
    make(<description-selection-pane>,
         gene: pane.pane-gene,
         getter: gene-receptor-locus,
         setter: gene-receptor-locus-setter,
         descriptions: 
           map(curry(get-receptor-locus-description, 
                     pane.pane-gene.gene-genome-version,
                     pane.organ-pane.description-pane.gadget-value,
                     pane.tissue-pane.description-pane.gadget-value),
             get-receptor-locus-range(pane.pane-gene.gene-genome-version, pane.organ-pane.description-pane.gadget-value,
                                                        pane.tissue-pane.description-pane.gadget-value)),
         converter: curry(get-receptor-locus-description, 
                          pane.pane-gene.gene-genome-version,
                          pane.organ-pane.description-pane.gadget-value,
                          pane.tissue-pane.description-pane.gadget-value));

  pane organ-tissue-locus-pane (pane)
    vertically()
      pane.organ-pane;
      pane.tissue-pane;
      pane.locus-pane;
    end;

  layout (pane)
    make(<group-box>, label: "Organ, Tissue, Locus",
                        child: pane.organ-tissue-locus-pane);                   

end pane <receptor-loci-pane>;

define pane <emitter-loci-pane> (<gene-pane>)

  pane organ-pane (pane)
    make(<description-selection-pane>,
         value-changed-callback: method(p) 
           let organ = p.description-pane.gadget-value;
           update-description-selection( pane.tissue-pane, 
             curry(internal-emitter-tissue-description, pane.pane-gene, pane.pane-genome, 
                            organ));
           update-description-selection( pane.locus-pane, 
             curry(get-emitter-locus-description, 
                          pane.pane-gene.gene-genome-version,
                          pane.organ-pane.description-pane.gadget-value,
                          pane.tissue-pane.description-pane.gadget-value));
         end method,           
         gene: pane.pane-gene,
         getter: gene-emitter-organ,
         setter: gene-emitter-organ-setter,
         descriptions: map(curry(get-emitter-organ-description, pane.pane-gene.gene-genome-version), 
                           get-emitter-organ-range(pane.pane-gene.gene-genome-version)),
         converter: curry(get-emitter-organ-description, pane.pane-gene.gene-genome-version));

  pane tissue-pane (pane)
    make(<description-selection-pane>,
         gene: pane.pane-gene,
         value-changed-callback: method(p) 
           let tissue = p.description-pane.gadget-value;
           update-description-selection( pane.locus-pane, 
             curry(get-emitter-locus-description, 
                          pane.pane-gene.gene-genome-version,
                          pane.organ-pane.description-pane.gadget-value,
                          pane.tissue-pane.description-pane.gadget-value));
         end method,           
         getter: gene-emitter-tissue,
         setter: gene-emitter-tissue-setter,
         descriptions: 
           map(curry(get-emitter-tissue-description, pane.pane-gene.gene-genome-version, pane.organ-pane.description-pane.gadget-value),
             get-emitter-tissue-range(pane.pane-gene.gene-genome-version, pane.organ-pane.description-pane.gadget-value)),
         converter: curry(internal-emitter-tissue-description, pane.pane-gene, pane.pane-genome, 
                            pane.organ-pane.description-pane.gadget-value));

  pane locus-pane (pane)
    make(<description-selection-pane>,
         gene: pane.pane-gene,
         getter: gene-emitter-locus,
         setter: gene-emitter-locus-setter,
         descriptions: 
           map(curry(get-emitter-locus-description, 
                     pane.pane-gene.gene-genome-version,
                     pane.organ-pane.description-pane.gadget-value,
                     pane.tissue-pane.description-pane.gadget-value),
             get-emitter-locus-range(pane.pane-gene.gene-genome-version, pane.organ-pane.description-pane.gadget-value,
                                                        pane.tissue-pane.description-pane.gadget-value)),
         converter: curry(get-emitter-locus-description, 
                          pane.pane-gene.gene-genome-version,
                          pane.organ-pane.description-pane.gadget-value,
                          pane.tissue-pane.description-pane.gadget-value));

  pane organ-tissue-locus-pane (pane)
    vertically()
      pane.organ-pane;
      pane.tissue-pane;
      pane.locus-pane;
    end;

  layout (pane)
    make(<group-box>, label: "Organ, Tissue, Locus",
                        child: pane.organ-tissue-locus-pane);                   

end pane <emitter-loci-pane>;

