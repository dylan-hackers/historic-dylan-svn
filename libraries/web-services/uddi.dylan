Module:    uddi
Synopsis:  Universal Description, Discovery and Integration (UDDI)
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

// This is an implementation of an interface to UDDI services.
// Maybe we should extend this to an interface that can interact
// with various Directories, like LDAP or X.500.  But for the 
// time being, KISS.

define open abstract class <uddi-registry> (<object>)
end class <uddi-registry>;

define open generic registry-nodes
    (registry :: <uddi-registry>) => (nodes :: <collection>);

define open generic registry-nodes-setter
    (nodes :: <collection>, registry :: <uddi-registry>)
 => (nodes :: <collection>);

define open generic registry-add-node!
    (registry :: <uddi-registry>, node :: <uddi-node>)
 => (registry :: <uddi-registry>);

define open generic registry-remove-node!
    (registry :: <uddi-registry>, node :: <uddi-node>)
 => (registry :: <uddi-registry>);


define method registry-add-node!
    (registry :: <uddi-registry>, node :: <uddi-node>)
 => (registry :: <uddi-registry>);
  registry-nodes(registry) := add!(registry-nodes(registry), node);
end method registry-add-node!;

define method registry-remove-node!
    (registry :: <uddi-registry>, node :: <uddi-node>)
 => (registry :: <uddi-registry>);
  registry-nodes(registry) := remove!(registry-nodes(registry), node);
end method registry-remove-node!;

define open abstract class <uddi-node> (<object>)
end class <uddi-node>;

define open generic uddi-node-registry
    (node :: <uddi-node>) => (registry :: <uddi-registry>);

define open generic uddi-node-registry-setter
    (registry :: <uddi-registry>, node :: <uddi-node>) => (registry :: <uddi-registry>);


