Module:    xml-schema
Synopsis:  XML Schema Definition.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

define constant $xml-schema-namespace = 
    make(<xml-namespace>,
         url: "http://www.w3.org/2001/XMLSchema",
         short-name: "xs");

define constant $xml-schema-instance-namespace = 
    make(<xml-namespace>,
         url: "http://www.w3.org/2001/XMLSchema-instance",
         short-name: "xsi");

define constant $xsi-type-name
    = make(<xml-name>,
           namespace: $xml-schema-instance-namespace,
           local-name: "type");
define constant $xsi-nil-name
    = make(<xml-name>,
           namespace: $xml-schema-instance-namespace,
           local-name: "nil");
define constant $xsi-schema-location-name
    = make(<xml-name>,
           namespace: $xml-schema-instance-namespace,
           local-name: "schemaLocation");
define constant $xsi-no-namespace-schema-location-name
    = make(<xml-name>,
           namespace: $xml-schema-instance-namespace,
           local-name: "noNamespaceSchemaLocation");


// The definition of an XML Schema.
//
define class <xml-schema> (<object>)
  slot notation-declarations :: <collection> = #(),
    init-keyword: notation-declarations:;
  slot type-definitions :: <collection> = #(),
    init-keyword: type-definitions:;
  slot element-declarations :: <collection> = #(),
    init-keyword: element-declarations:;
  slot attribute-declarations :: <collection> = #(),
    init-keyword: attribute-declarations:;
  slot attribute-group-definitions :: <collection> = #(),
    init-keyword: attribute-group-definitions:;
  slot model-group-definitions :: <collection> = #(),
    init-keyword: model-group-definitions:;
end class <xml-schema>;

// Validates Document according to the Validation Rules (§C.1)
//
define generic document-valid?
    (document :: <xml>, xml-schema :: <xml-schema>)
 => (well? :: <boolean>);

// Checks XML-Schema for Schema Component Constraints (§C.4)
//
define generic xml-schema-valid?
    (xml-schema :: <xml-schema>)
 => (well? :: <boolean>);

// Returns the Post-Schema-Validation-Infoset (§C.2)
//
define generic post-schema-validation-infoset
    (document :: <xml>, xml-schema :: <xml-schema>)
 => (post-schema-validation-infoset :: <xml>);

// Returns the XML represenation of a Schema. (3.1.3)
//
define generic xml-schema-as-schema-document
    (xml-schema :: <xml-schema>)
 => (xml-representation :: <xml>);

// Parses an XML representation into a schema component or schema.
//
define generic schema-document-as-xml-schema
    (xml :: <xml>)
 => (component :: <xml-schema>);



// Mixins.
// ======

define abstract open class <xml-name-mixin> (<object>)
  slot xml-name :: <xml-name>,
    required-init-keyword: xml-name:;
end class <xml-name-mixin>;

define abstract open class <maybe-xml-name-mixin> (<object>)
  slot xml-name :: false-or(<xml-name>) = #f,
    init-keyword: xml-name:;
end class <maybe-xml-name-mixin>;

define method local-name
    (object, #key default :: false-or(<string>))
 => (local-name :: false-or(<string>));
  let qname = xml-name(object);
  if (qname)
    qname.name-local-name;
  else
    default;
  end if;
end method local-name;

define method local-name
    (object :: <xml-name>, #key default)
  name-local-name(object);
end method local-name;

define method target-namespace
    (object, #key default :: false-or(<xml-namespace>))
 => (default-namespace :: false-or(<xml-namespace>));
  let qname = xml-name(object);
  if (qname)
    qname.name-namespace;
  else
    default;
  end if;
end method target-namespace;

define abstract open class <attribute-user-mixin> (<object>)
  slot attribute-uses = #(),
    init-keyword: attribute-uses:;
  slot attribute-wildcard = #f,
    init-keyword: attribute-wildcard:;
end class <attribute-user-mixin>;


// Components.
// ==========

// Everything that may be part of a schema inherits from <xml-schema-component>.

define abstract class <xml-schema-component> (<object>)
  // The annotation that is allowed as the first child.
  slot annotation :: false-or(<xml-schema-annotation>) = #f,
    init-keyword: annotation:;
  // Additional annotations, e.g., those from attributes with
  // non-xs-namespace.
  slot annotations :: <collection> = #(),
    init-keyword: annotations:;
end class <xml-schema-component>;

// Support for Backpatching.
// ========================

// The spec explicitly allows forward references.  Thus we 
// need support for Backpatching.
// Backpatching support should look something like this.
// Slots should automatically call Do-Backpatches
// whenever we assign a non-backpatch component to a slot
// in which a backpatch is currently stored.  And some
// further magic.

define class <xml-schema-backpatch-component> (<xml-schema-component>)
  slot backpatchers = #(),
    init-keyword: backpatchers:;
end class <xml-schema-backpatch-component>;

define method request-backpatch
    (f :: <function>, backpatcher :: <xml-schema-backpatch-component>)
 => ();
  backpatchers(backpatcher) := pair(f, backpatchers(backpatcher));
end method request-backpatch;

define method do-backpatches
    (component :: <xml-schema-component>,
     new-component :: <xml-schema-component>)
 => ();
  // Do nothing.
end method do-backpatches;

define method do-backpatches
    (component :: <xml-schema-backpatch-component>,
     new-component :: <xml-schema-component>)
 => ();
  for (f in backpatchers(component))
    f(new-component);
  end for;
end method do-backpatches;


// Primary Components.
// ==================

define abstract class <xml-schema-definition> (<xml-schema-component>)
end class <xml-schema-definition>;

define abstract class <xml-schema-named-definition>
    (<xml-schema-definition>, <xml-name-mixin>)
end class <xml-schema-named-definition>;

define abstract class <xml-schema-type-definition>
    (<xml-schema-definition>, <maybe-xml-name-mixin>)
  slot final? = #f,
    init-keyword: final?:;
end class <xml-schema-type-definition>;

define open generic type-definition-valuespace
    (definition :: <xml-schema-type-definition>)
 => (dylan-type :: <type>);

define method type-definition-valuespace
    (definition :: <xml-schema-type-definition>)
 => (dylan-type :: <type>);
  <object>;
end method type-definition-valuespace;    

define abstract class <simple-type-definition> (<xml-schema-type-definition>)
  slot facets = #(),
    init-keyword: facets:;
  slot variety = #f,
    init-keyword: variety:;
end class <simple-type-definition>;

define abstract class <complex-type-definition>
    (<xml-schema-type-definition>, <attribute-user-mixin>)
  slot content-type = #f,
    init-keyword: content-type:;
  slot abstract? = #f,
    init-keyword: abstract?:;
  slot prohibited-substitutions = #(),
    init-keyword: prohibited-substitutions:;
end class <complex-type-definition>;

define class <ur-type-definition> (<complex-type-definition>)
  inherited slot xml-name = make(<xml-name>,
                                 namespace: $xml-schema-namespace,
                                 local-name: "anyType");
end class <ur-type-definition>;

define variable *the-ur-type-definition* :: false-or(<ur-type-definition>) = #f;

define method make
    (self == <ur-type-definition>, #key)
 => (the-ur-type-definition :: <ur-type-definition>);
  *the-ur-type-definition*
    | (*the-ur-type-definition* := next-method());
end method make;

// The base type could be more restricted for simple types.
// Alas not for the simple-ur-type, thus there's no need to do
// anything fancy right now.
//
define abstract class <derived-type-definition> (<xml-schema-type-definition>)
  slot base-type-definition :: <xml-schema-type-definition>,
    required-init-keyword: base-type-definition:;
end class <derived-type-definition>;

define abstract class <type-restriction> (<derived-type-definition>)
end class <type-restriction>;

define abstract class <type-extension> (<derived-type-definition>)
end class <type-extension>;

define class <simple-type-restriction>
    (<simple-type-definition>, <type-restriction>)
end class <simple-type-restriction>;

/*
define class <simple-type-extension>
    (<simple-type-definition>, <type-extension>)
end class <simple-type-extension>;
*/
define class <complex-type-restriction>
    (<complex-type-definition>, <type-restriction>)
end class <complex-type-restriction>;

define class <complex-type-extension>
    (<complex-type-definition>, <type-extension>)
end class <complex-type-extension>;

define constant $any-type :: <ur-type-definition>
  = make(<ur-type-definition>);

define constant $any-simple-type :: <simple-type-definition>
    = make(<simple-type-restriction>, 
           base-type-definition: $any-type,
           xml-name: make(<xml-name>,
                          namespace: $xml-schema-namespace,
                          local-name: "anySimpleType"));

define abstract class <xml-schema-declaration>
    (<xml-schema-component>, <xml-name-mixin>)
end class <xml-schema-declaration>;

define class <type-declaration> (<xml-schema-declaration>)
  slot type-definition = #f,
    init-keyword: type-definition:;
end class <type-declaration>;

define class <attribute-declaration> (<type-declaration>)
end class <attribute-declaration>;

define class <element-declaration> (<type-declaration>)
  slot scope = #f,
    init-keyword: scope:;
  slot value-constraint = #f,
    init-keyword: value-constraint:;
  slot nillable? :: <boolean> = #f,
    init-keyword: nillable?:;
  slot substitution-group-affiliation = #f,
    init-keyword: substitution-group-affiliation:;
  slot substitution-group-exclusions = #f,
    init-keyword: substitution-group-exclusions:;
  slot disallowed-substitutions = #f,
    init-keyword: disallowed-substitutions:;
  slot abstract? :: <boolean> = #f,
    init-keyword: abstract?:;
  slot identity-constraint-definitions = #(),
    init-keyword: identity-constraint-definitions:;
end class <element-declaration>;


// Secondary components.
// ====================
define class <attribute-group-definition>
    (<xml-schema-named-definition>, <attribute-user-mixin>)
end class <attribute-group-definition>;

define class <identity-constraint-definition> (<xml-schema-named-definition>)
  slot identity-constraint-category = #f,
    init-keyword: identity-constraint-category:;
  slot selector = #f,
    init-keyword: selector:;
  slot fields = #(),
    init-keyword: fields:;
  slot referenced-key = #f,
    init-keyword: referenced-key:;
end class <identity-constraint-definition>;

define class <model-group-definition> (<xml-schema-named-definition>)
  slot model-group = #f,
    init-keyword: model-group:;
end class <model-group-definition>;

define class <notation-declaration> (<xml-schema-named-definition>)
  slot system-identifier = #f,
    init-keyword: system-identifier:;
  slot public-identifier = #f,
    init-keyword: public-identifier:;
end class <notation-declaration>;


// Helper Components.
// =================

define class <xml-schema-annotation> (<xml-schema-component>)
end class <xml-schema-annotation>;

define class <xml-schema-model-group> (<xml-schema-component>)
  slot particles = #(),
    init-keyword: particles:;
  slot compositor = #f,
    init-keyword: compositor:;
end class <xml-schema-model-group>;

define class <xml-schema-particle> (<xml-schema-component>)
  slot term = #f,
    init-keyword: term:;
  slot content-type = #f,
    init-keyword: content-type:;
  slot min-occurs = 1,
    init-keyword: min-occurs:;
  slot max-occurs = 1,
    init-keyword: max-occurs:;
end class <xml-schema-particle>;

define class <xml-schema-wildcard> (<xml-schema-component>)
  slot namespace-constraint = #f,
    init-keyword: namespace-constraint:;
  slot process-contents = #f,
    init-keyword: process-contents:;
end class <xml-schema-wildcard>;

define class <xml-schema-attribute-use> (<xml-schema-component>)
end class <xml-schema-attribute-use>;


// Objects that represent an instance of an xml-schema.
//
define open abstract class <xml-schema-object> (<object>)
end class <xml-schema-object>;
define class <xml-schema-attribute> (<xml-schema-object>)
end class <xml-schema-attribute>;

define class <xml-schema-element> (<xml-schema-object>)
end class <xml-schema-element>;

define class <xml-schema-type> (<xml-schema-object>)
end class <xml-schema-type>;

define class <simple-xml-schema-type> (<xml-schema-type>)
  slot simple-xml-schema-type-value :: <string>,
    required-init-keyword: value:;
end class <simple-xml-schema-type>;

define open class <complex-xml-schema-type> (<xml-schema-type>)
end class <complex-xml-schema-type>;

define macro simple-xml-schema-type-definer 
  { define simple-xml-schema-type "<" ## ?type-name:name ## ">" (?:name) => ?dylan-type:expression
      ?:body
    end }
 =>
  { define class "<" ## ?type-name ## "-definition>" (<ur-type-definition>) end;
    define class "<" ## ?type-name ## ">"  (<simple-xml-schema-type>) end;
    define method convert-schema-to-dylan (?name :: "<" ## ?type-name ## ">")
     => (result :: ?dylan-type);
      ?body;
    end method convert-schema-to-dylan
  }
  { define simple-xml-schema-type "<" ## ?type-name:name ## ">" => ?dylan-type:expression
    end }
 =>
  { define class "<" ## ?type-name ## "-definition>" (<ur-type-definition>) end;
    define class "<" ## ?type-name ## ">"  (<simple-xml-schema-type>) end;
    define method convert-schema-to-dylan (name :: "<" ## ?type-name ## ">")
      as(?dylan-type, simple-xml-schema-type-value(name));
    end method convert-schema-to-dylan
  }
end macro simple-xml-schema-type-definer;

define simple-xml-schema-type <xml-schema-string> => <string> end;

define simple-xml-schema-type <xml-schema-integer> (xml-schema-integer) => <integer>
  string-to-integer(simple-xml-schema-type-value(xml-schema-integer));
end simple-xml-schema-type <xml-schema-integer>;

