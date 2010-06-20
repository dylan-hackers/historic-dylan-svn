module: dylan-topics
synopsis: Functions dealing with markup and definition lists.


define method defn-list (section :: <section>) => (defn-list :: false-or(<defn-list>))
   find-element(section.content, rcurry(instance?, <defn-list>))
end method;


/**
Synopsis:
This function replaces the default documentation of an init-arg, parameter, or
returned value with its individual documentation from author-supplied markup.

Elements of 'defn-list' and 'items' each have have a label and some content. For
'defn-list' elements, the label is the result of 'stringify-markup' applied to
the first column of a row and the content is the second column. For 'items'
elements, the label is the result of 'defn-list-item-label' applied to the
element and the content is generated from the element's 'markup-tokens'.

This function replaces 'defn-list' element content with corresponding 'items'
element content, if there is a corresponding 'items' element and that element
has content. Two elements correspond if their labels are equal.

Arguments:
defn-list   - An instance of <defn-list> containing generated documentation for
              init-args, parameters, or returned values.
items       - A sequence of <documentable-api-object>. The code representations
              of init-args, parameters, or returned values.
**/
define method replace-list-item-docs (defn-list :: <defn-list>, api-items :: <sequence>)
=> ()
   let defn-items = defn-list.items;
   for (item :: <documentable-api-object> in api-items)
      unless (item.markup-tokens.empty?)
         let item-label = item.defn-list-item-label;
         let item-content = content-from-markup(item.markup-tokens.first);
         for (row from 0 below dimension(defn-items, 0))
            let defn-label = defn-items[row, 0].stringify-markup;
            if (case-insensitive-equal?(defn-label, item-label))
               defn-items[row, 1] := item-content;
            end if
         end for
      end unless
   end for;
   defn-list
end method;


/// Synopsis: Get all markup tokens from a sequence of <documentable-api-object>.
define method all-markup-tokens (documentable-items :: <sequence>)
=> (tokens :: <sequence>)
   let token-sets = map(markup-tokens, documentable-items);
   apply(concatenate, #[], token-sets)
end method;


// TODO: Replace programmatic section construction with template system.


define method make-source-topics (defn :: <binding>) 
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let generated-topic = make(<unbound-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         existent-api: #t, qualified-name: defn.definition-qualified-name,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);
   values(vector(generated-topic), #[])
end method;


define method make-source-topics
   (defn :: type-union(<constant-binding>, <variable-binding>))
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let generated-topic = make(<variable-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         existent-api: #t, qualified-name: defn.definition-qualified-name,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);
   values(vector(generated-topic), #[])
end method;


define method make-source-topics (defn :: <macro-binding>) 
=> (topics :: <sequence>, catalog-topics :: <sequence>)
   let generated-topic = make(<macro-doc>, source-location: defn.source-location,
         id: defn.canonical-id, title: defn.canonical-title, generated: #t,
         existent-api: #t, qualified-name: defn.definition-qualified-name,
         title-id-source-location: $generated-source-location,
         qualified-name-source-location: $generated-source-location);
   values(vector(generated-topic), #[])
end method;

