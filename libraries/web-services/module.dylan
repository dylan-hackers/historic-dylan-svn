Module:    dylan-user
Synopsis:  Conversion of Dylan to XML and vice versa.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

define module web-services-imports
  use common-dylan,
    exclude: { format-to-string },
    export: all;
  use common-extensions,
    exclude: { format-to-string },
    export: all;
  use dylan-extensions,
    export: all,
    rename: { <namespace> => <dylan-namespace>,
              namespace-name => dylan-namespace-name };
  // Needed to dump functions...
  // use dfmc-common;
  use threads,
    export: all;
  use format,
    export: all;
  use format-out,
    export: all;
  use standard-io,
    export: all;
  use streams,
    export: all;
  use simple-random,
    export: all;
  use xml-parser,
//    rename: { name => xml/name, name-setter => xml/name-setter },
    export: all;
end module;

define module web-services-info
  use common-dylan;

  export $web-services-name, $web-services-major-version,
    $web-services-minor-version, web-services-full-name;
end module web-services-info;

define module xml-schema
  use web-services-imports;
  use namespaces;
  use xml-parser,
    export: { xml-name, xml-name-setter };

  export $xml-schema-namespace,
    $xml-schema-instance-namespace,
    $xsi-type-name,
    $xsi-nil-name,
    $xsi-schema-location-name,
    $xsi-no-namespace-schema-location-name;

  export <xml-name-mixin>, <maybe-xml-name-mixin>,
    <attribute-user-mixin>,
      attribute-uses, attribute-uses-setter,
      attribute-wildcard, attribute-wildcard-setter;

  export <xml-schema>,
      notation-declarations, notation-declarations-setter,
      type-definitions, type-definitions-setter,
      element-declarations, element-declarations-setter,
      attribute-declarations, attribute-declarations-setter, 
      attribute-group-definitions, attribute-group-definitions-setter,
      model-group-definitions, model-group-definitions-setter,
      local-name, target-namespace,
      document-valid?, xml-schema-valid?,
      post-schema-validation-infoset,
      schema-document-as-xml-schema, xml-schema-as-schema-document,
    <xml-schema-component>,
      annotation, annotation-setter,
      annotations, annotations-setter,
    <xml-schema-backpatch-component>,
      request-backpatch, do-backpatches,
    <xml-schema-definition>,
    <xml-schema-type-definition>, 
      type-definition-valuespace,
      final?, final?-setter,
    <derived-type-definition>,
      base-type-definition, base-type-definition-setter,
    <type-restriction>, <type-extension>,
    <simple-type-definition>,
      facets, facets-setter,
      variety, variety-setter,
    <ur-type-definition>,
    <simple-type-restriction>,
    // This doesn't make much sense, does it? <simple-type-extension>,
    <complex-type-definition>,
      content-type, content-type-setter,
      abstract?, abstract?-setter,
      prohibited-substitutions, prohibited-substitutions-setter,
    <complex-type-restriction>, <complex-type-extension>,
    $any-type, $any-simple-type,
    <xml-schema-declaration>, 
    <attribute-declaration>,
    <element-declaration>,
      type-definition, type-definition-setter,
      scope, scope-setter,
      value-constraint, value-constraint-setter,
      nillable?, nillable?-setter,
      substitution-group-affiliation, substitution-group-affiliation-setter,
      substitution-group-exclusions, substitution-group-exclusions-setter,
      disallowed-substitutions, disallowed-substitutions-setter,
      identity-constraint-definitions, identity-constraint-definitions-setter,
    <attribute-group-definition>,
    <identity-constraint-definition>,
      identity-constraint-category, identity-constraint-category-setter,
      selector, selector-setter,
      fields, fields-setter,
      referenced-key, referenced-key-setter,
    <model-group-definition>,
      model-group, model-group-setter,
    <notation-declaration>,
      system-identifier, system-identifier-setter,
      public-identifier, public-identifier-setter,
    <xml-schema-annotation>,
    <xml-schema-model-group>,
      particles, particles-setter,
      compositor, compositor-setter,
    <xml-schema-particle>,
      term, term-setter,
      content-type, content-type-setter,
      min-occurs, min-occurs-setter,
      max-occurs, max-occurs-setter,
    <xml-schema-wildcard>,
      namespace-constraint, namespace-constraint-setter,
      process-contents, process-contents-setter,
    <xml-schema-attribute-use>;

  export <xml-schema-object>,
    <xml-schema-annotation>,
    <xml-schema-attribute>,
    <xml-schema-element>,
    <xml-schema-type>,
    <simple-xml-schema-type>,
      simple-xml-schema-type-value, simple-xml-schema-type-value-setter,
      simple-xml-schema-type-definer,
    <complex-xml-schema-type>;

  export <xml-schema-integer>, <xml-schema-integer-definition>,
    <xml-schema-string>, <xml-schema-string-definition>;
end module xml-schema;

define module xml-converter
  use web-services-imports;
  use xml-schema;

  export <xml-converter-warning>,
    <xml-converter-error>,
    <xml-element-error>,
    <xml-dylan-object-error>,
    <no-conversion-for-xml-element-error>,
    <converted-object-not-simple-error>,
    xml-element,
    <no-conversion-for-dylan-object-error>,
    dylan-object;

  // The client interface.
  export <xml-converter>,
    convert-to-xml, convert-from-xml,
    converter-id,
    <unique-id>,
    serial-number, referenced-object,
    object-id, object-id-setter, 
    object-id!, 
    converted-class-name-and-library, 
    library-as-xml-namespace,
    class-name-as-xml-name,
    $dylan-default-namespace, $dylan-default-namespace-name,
    converted-slot-name,

    simple-element-value,

    // The implementor interface
    \with-current-xml-element,
    class-name-and-library-or-lose,
    converted-objects, converted-objects-setter,
    currently-converting, currently-converting-setter,
    found-cycle?,
    next-serial-number!,
    convert-object-to-xml,
    convert-slots-to-xml, convert-slot-to-xml,
    convert-unique-reference-to-xml,
    // has-unique-id-attribute?, 
    ensure-unique-id, assign-unique-id,
    unique-id-table, object-table,
    convert-unique-id-from-xml;

  export <slot-spec>,
    slot-spec-descriptor,
    slot-spec-name,
    slot-spec-bound?,
    slot-spec-value-element,
    slot-spec-element,
    <repeated-slot-spec>,
    repeated-slot-spec-descriptor,
    repeated-slot-spec-size-descriptor,
    repeated-slot-spec-size-init-keyword,
    repeated-slot-spec-size,
    repeated-slot-spec-name,
    repeated-slot-spec-elements;
end module xml-converter;

define module mailbox
  use web-services-imports;

  export <mailbox>,
    <mailbox-condition>, <mailbox-warning>, <mailbox-error>,
    <mailbox-delivery-timeout>, <mailbox-read-data-timeout>,
    deliver, read-data, message-exists?;
end module mailbox;

define module web-services
  use web-services-imports;
  use mailbox;
  use xml-schema;
  use xml-converter;

  export <context>,
    enclosing-context, service-providers,
    current-context, current-context-setter,
    context-encloses?, context-in-scope?,
    \with-current-context;

  export <service-provider>,
    <local-service-provider>,
    <logging-service-provider>,
    default-service-provider, default-service-provider-setter,
    service-provider-for, 
    service-provider-for-make, service-provider-for-make-setter,
    service-provider-for-call, service-provider-for-call-setter,
    \with-default-service-provider,
    \service-provider-operation-definer;

  export <factory-object>,
    make-factory-object;
end module web-services;

define module wsdl
  use web-services-imports;
  use mailbox;
  use web-services;
  use xml-schema;
end module wsdl;

define module uddi
  use web-services-imports;
  use mailbox;
  use web-services;
  use xml-schema;
  use wsdl;

  export <uddi-registry>,
    registry-nodes, registry-nodes-setter,
    registry-add-node!, registry-remove-node!;

  export <uddi-node>,
    uddi-node-registry, uddi-node-registry-setter;
end module uddi;

