Module:    web-services
Synopsis:  Basic abstractions for the web service framework.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

// Some definitions to shorten the descriptions in this file:
// * An OPERATION is a function that is delegated to a service 
//   provider.
// * An OPERATION-NAME is a symbol that can be used to identify
//   an operation 
// * An OPERATION-TABLE is a table that is keyed by operation-names.

// The context in which we are operating.
// This is useful for service selection, etc.
//
define open class <context> (<object>)
  constant slot enclosing-context :: false-or(<context>) = #f,
    init-keyword: enclosing-context:;

  // The operation-table for this context.
  // We might want to merge this with the outer context, but I'm
  // not sure whether this is the right thing to do.
  constant slot service-providers :: <table> = make(<table>);
end class <context>;

define open generic enclosing-context
    (context :: <context>) => (enclosing-context :: false-or(<context>));

// Returns a table that maps operation-names to something.  What exactly
// "something" is, is defined by the definition of Service-Provider-For
// for the operation-name.
//
define open generic service-providers
    (context :: <context>) => (service-providers :: <table>);

// A table containing all operation-names provided by service
// providers, i.e.,  all symbols that may be used as second argument
// to Service-Provider-For.
//
define variable *default-service-provider-types* = #();

// A table mapping operation-names to the default provider
// for that operation.  If this table contains no entry for
// an operation-name the default service provider is used
// by Service-Provider-For.
//
define variable *default-service-providers* = make(<table>);

define method initialize (self :: <context>, #key)
  for (entry in *default-service-provider-types*)
    service-providers(self)[entry] := make(<table>);
  end for;
end method initialize;

// The current context.  Lazily initialized, because a user of the framework
// might define operations which need operation-tables.
//
define thread variable *current-context* :: false-or(<context>) = #f;

// The context in which we are currently operating.
//
define inline-only function current-context () => (current-context :: <context>)
  *current-context*
    | (*current-context* := make(<context>));
end function current-context;

define inline-only function current-context-setter
    (new-context :: <context>) => (new-context :: <context>);
  *current-context* := new-context;
end function current-context-setter;

define macro with-current-context
  { with-current-context (?context:expression)
      ?:body
    end }
  => { let cc = current-context();
       unless (enclosing-context(cc))
         enclosing-context(cc) := ?context;
       end unless;
       dynamic-bind (*current-context* = ?context)
         ?body
       end }
end macro with-current-context;

define open generic context-encloses?
    (context :: false-or(<context>), enclosed-context :: <context>)
 => (well? :: <boolean>);

define method context-encloses?
    (context == #f, enclosed-context :: <context>) => (no! :: singleton(#f));
  #f;
end method context-encloses?;

define method context-encloses?
    (context :: <context>, enclosed-context :: <context>) => (well? :: <boolean>);
  context == enclosed-context
   | context-encloses?(enclosing-context(context), enclosed-context);
end method context-encloses?;

define open generic context-in-scope? (context :: <context>) => (well? :: <boolean>);

define method context-in-scope?(context :: <context>) => (well? :: <boolean>);
  context-encloses?(current-context(), context);
end method context-in-scope?;

// A service provider is an abstraction that is used to control
// creation of (possibly remote) objects or calls to (possibly
// remote) functions.
//
define open abstract class <service-provider> (<object>)
end class <service-provider>;

// A service provider that delegates all operations defined for
// service providers to their local equivalent.
// E.g. make-factory-object simply invokes make if called with
// a <local-service-provider>.
//
define open class <local-service-provider> (<service-provider>)
end class <local-service-provider>;

// The "default default" service provider which is returned by service-provider-for
// if no other service provider is defined for an operation.
//
define variable *default-service-provider* :: false-or(<service-provider>) = #f;

define method default-service-provider
    () => (service-provider :: <service-provider>);
  *default-service-provider*
    | (*default-service-provider* := make(<local-service-provider>));
end method default-service-provider;

define method default-service-provider-setter
    (new-value :: <service-provider>) => (service-provider :: <service-provider>);
  *default-service-provider* := new-value;
end method default-service-provider-setter;

define macro with-default-service-provider
  { with-default-service-provider (?provider:expression) ?:body end }
  => { dynamic-bind (*default-service-provider* = ?provider) ?body end }
end macro with-default-service-provider;

// Returns the service provider that performs Task in Context.
//
define open generic service-provider-for
    (context :: <context>, task :: <symbol>, #rest arguments)
 => (service-provider :: <service-provider>);

define method add-service-provider-type
    (name :: <symbol>, #key default = #f)
  unless (member?(name, *default-service-provider-types*))
    *default-service-provider-types*
      := pair(name, *default-service-provider-types*);
  end unless;
  if (default)
    *default-service-providers*[name] := default;
  end if;
end method add-service-provider-type;

define macro service-provider-operation-definer
  { define service-provider-operation ?:name (?arglist:*) }
  => { define method service-provider-for
           (context :: <context>, task == ?#"name", #rest arguments)
        => (service-provider :: <service-provider>);
         apply("service-provider-for-" ## ?name, context, arguments);
       end method service-provider-for;
       define sealed domain service-provider-for(<context>, singleton(?#"name"));
       define method service-provider-for-setter
           (new-service-provider :: <service-provider>,
            context :: <context>, task == ?#"name", #rest arguments)
        => (service-provider :: <service-provider>);
         apply("service-provider-for-" ## ?name ## "-setter",
               new-service-provider, context, arguments);
       end method service-provider-for-setter;
       define sealed domain service-provider-for(<context>, singleton(?#"name"));
       
       define open generic "service-provider-for-" ## ?name
           (?arglist) => (service-provider :: <service-provider>);
       define open generic "service-provider-for-" ## ?name ## "-setter"
           (new-service-provicer :: <service-provider>, ?arglist)
        => (new-service-provider :: <service-provider>);
       add-service-provider-type(?#"name");
     }
end macro service-provider-operation-definer;

define service-provider-operation make
    (context :: <context>, class :: <class>, #rest arguments);

// Maybe implement nesting of service provider contexts in
// service-provider-for-foo?

define method service-provider-for-make-setter
    (new-service-provider :: <service-provider>,
     context :: <context>, 
     class :: <class>,
     #rest arguments)
 => (new-service-provider :: <service-provider>)
  context.service-providers[#"make"][class] := new-service-provider;
end method service-provider-for-make-setter;

define method service-provider-for-make
    (context :: <context>, class :: <class>, #rest arguments)
 => (service-provider :: <service-provider>);
  element(context.service-providers[#"make"], class, default: #f) 
    | default-service-provider();
end method service-provider-for-make;

define service-provider-operation call
    (context :: <context>, function :: <function>, #rest arguments);

define method service-provider-for-call-setter
    (new-service-provider :: <service-provider>,
     context :: <context>, 
     function :: <function>,
     #rest arguments)
 => (new-service-provider :: <service-provider>)
  context.service-providers[#"call"][function] := new-service-provider;
end method service-provider-for-call-setter;

define method service-provider-for-call
    (context :: <context>, function :: <function>, #rest arguments)
 => (service-provider :: <service-provider>);
  element(context.service-providers[#"call"], function, default: #f) 
    | default-service-provider();
end method service-provider-for-call;

define open abstract class <factory-object> (<object>)
end class <factory-object>;

define open generic make-factory-object
    (service-provider :: <service-provider>,
     class :: <class>,
     really-make-it :: <function>,
     #rest args) => (object :: <object>);

define method make
    (class :: subclass(<factory-object>), 
     #rest args, #key context = current-context())
 => (object :: <object>);
  apply(make-factory-object,
        service-provider-for(context, #"make", class), 
        class, 
        next-method,
        class, args);
end method make;

define method make-factory-object
    (service-provider :: <local-service-provider>,
     class :: <class>,
     really-make-it :: <function>,
     #rest args)
 => (factory-object :: <factory-object>);
  really-make-it();
end method make-factory-object;

define class <logging-service-provider> (<local-service-provider>)
end class <logging-service-provider>;

define method make-factory-object
    (service-provider :: <logging-service-provider>,
     class :: <class>,
     really-make-it :: <function>,
     #rest args)
 => (factory-object :: <factory-object>);
  let result = next-method();
  format-out("Creating: %=\n", convert-to-xml(make(<xml-converter>), result));
  result;
end method make-factory-object;

