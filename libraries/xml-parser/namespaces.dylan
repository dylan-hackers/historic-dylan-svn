Module:    namespaces
Synopsis:  Namespaces for XML
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

// An implementation of XML namespaces.
// This should actually be in the XML-Parser.

define class <xml-namespace-error> (<simple-error>)
end class;

define class <namespace-not-found> (<xml-namespace-error>)
  inherited slot condition-format-string 
    = "The namespace %S does not exist.";
end class <namespace-not-found>;


define class <xml-namespace> (<object>)
  constant slot namespace-names :: <table> = make(<string-table>);
  slot namespace-url :: false-or(<string>) = #f,
    init-keyword: url:;
  slot namespace-short-name :: false-or(<string>) = #f,
    init-keyword: short-name:;
end class <xml-namespace>;

define method print-object
    (namespace :: <xml-namespace>, stream :: <stream>) => ();
  format(stream, "{<xml-namespace>, url: %S, short-name: %S}",
         namespace.namespace-url, namespace.namespace-short-name);
end method print-object;

define method initialize (self :: <xml-namespace>, #rest args, #key) => ();
  // format-out("Initializing %=, %=\n", self, args);
  next-method();
  register-namespace(self);
end method initialize;

define variable *xml-namespaces* :: <table> = make(<string-table>);

define method make
    (self == <xml-namespace>, #key url, short-name)
 => (namespace :: <xml-namespace>);
  if (url)
    let old = element(*xml-namespaces*, url, default: #f);
    if (old)
      unless (namespace-short-name(old))
        // Maybe we should dynamically bind the short-name?
        namespace-short-name(old) := short-name;
      end unless;
      old;
    else
      next-method();
    end if;
  else
    next-method();
  end if;
end method make;

define variable *default-namespace* :: <xml-namespace> = make(<xml-namespace>);

define method default-namespace () => (namespace :: <xml-namespace>)
  *default-namespace*;
end method default-namespace;

define method default-namespace-setter
    (namespace :: <xml-namespace>) => (namespace :: <xml-namespace>)
  *default-namespace* := namespace;
end method default-namespace-setter;

define macro with-default-namespace
  { with-default-namespace (?namespace:expression)
      ?body:*
    end }
 => { dynamic-bind(*default-namespace* = ?namespace)
        ?body
      end }
end macro with-default-namespace;

define generic find-namespace
    (namespace-url :: <string>, #key default, create?)
 => (namespace :: false-or(<xml-namespace>));

define generic find-namespace-setter
    (namespace :: false-or(<xml-namespace>), namespace-url :: <string>)
 => (namespace :: false-or(<xml-namespace>));

define method find-namespace
    (namespace-url :: <string>, #key default = unsupplied(), create?)
 => (namespace :: false-or(<xml-namespace>));
  let namespace = element(*xml-namespaces*, namespace-url, default: #f);
  if (namespace)
    namespace;
  elseif (create?)
    *xml-namespaces*[namespace-url] := make(<xml-namespace>, url: namespace-url);
  elseif (default.supplied?)
    default;
  else
    error(make(<namespace-not-found>,
               format-arguments: list(namespace-url)));
  end if;
end method find-namespace;

define method find-namespace-setter
    (namespace :: false-or(<xml-namespace>), namespace-url :: <string>)
 => (namespace :: false-or(<xml-namespace>));
  *xml-namespaces*[namespace-url] := namespace;
end method find-namespace-setter;

define generic register-namespace
    (namespace :: <xml-namespace>, #key);

define method register-namespace (namespace :: <xml-namespace>, #key)
  let url = namespace.namespace-url;
  if (url)
    *xml-namespaces*[url] := namespace;
  end if;
end method register-namespace;

define constant $xml-namespace = make(<xml-namespace>,
                                      url: "http://www.w3.org/XML/1998/namespace",
                                      short-name: "xml");

define class <namespace-has-no-short-name> (<xml-namespace-error>)
  inherited slot condition-format-string = "The namespace %S has no short name";
end class <namespace-has-no-short-name>;


define class <xml-name-error> (<simple-error>)
end class <xml-name-error>;

// We use #f as Name-Namespace is a name is defined outside of any 
// namespace...
//
define class <xml-name> (<object>)
  slot name-namespace :: false-or(<xml-namespace>) = #f,
    init-keyword: namespace:;
  slot name-local-name :: <string>,
    required-init-keyword: local-name:;
end class <xml-name>;

define class <namespace-binding> (<object>)
  constant slot local-short-name :: <string>,
    required-init-keyword: local-short-name:;
  constant slot local-namespace :: <xml-namespace>,
  required-init-keyword: local-namespace:;
end class <namespace-binding>;

define method print-object
    (binding :: <namespace-binding>, stream :: <stream>) => ();
  format(stream, "{<namespace-binding> name: %S, namespace: %S}",
         local-short-name(binding), local-namespace(binding));
end method print-object;

define variable *local-namespaces* :: <list> = #();

define macro with-local-namespace 
  { with-local-namespace (?short-name:expression => ?url:expression)
      ?body:*
    end }
 => { dynamic-bind(*local-namespaces* = pair(make(<namespace-binding>,
                                                  local-short-name: ?short-name,
                                                  local-namespace: make(<xml-namespace>,
                                                                        url: ?url,
                                                                        short-name: ?short-name)),
                                             *local-namespaces*))
        ?body
      end }
end macro with-local-namespace;

define macro with-local-namespaces
  { with-local-namespaces ()
      ?body:*
    end }
 => { ?body }
  { with-local-namespaces (?short-name:expression => ?namespace:expression)
      ?body:*
    end }
 => { with-local-namespace (?short-name => ?namespace)
        ?body
      end }
  { with-local-namespaces (?short-name:expression => ?namespace:expression, ...)
      ?body:*
    end }
 => { with-local-namespace (?short-name => ?namespace)
        with-local-namespaces (...)
          ?body
        end
      end }
end macro with-local-namespaces;

define function find-local-namespace
    (short-name :: <string>,
     #key local-namespaces = *local-namespaces*,
          default = default-namespace())
 => (local-namespace :: false-or(<xml-namespace>));
  iterate loop (local-namespaces = local-namespaces)
    if (empty?(local-namespaces))
      default;
    else
      let binding = first(local-namespaces);
      check-type(binding, <namespace-binding>);
      if (binding.local-short-name = short-name)
        binding.local-namespace;
      else
        loop(tail(local-namespaces))
      end if;
    end if;
  end iterate;
end function find-local-namespace;

define method xml-name-as-string
    (type :: subclass(<string>), name :: <xml-name>, 
     #key fully-qualified? :: type-union(<boolean>, singleton(#"if-possible")) = #t)
 => (namestring :: <string>)
  if (fully-qualified?)
    let ns = name-namespace(name);
    let nn = if (ns)
               ns.namespace-short-name;
             else
               #f;
             end;
    if (ns & nn)
      as(type, concatenate(nn, ":", name.name-local-name));
    elseif (fully-qualified? = #"if-possible" | ~ ns)
      as(type, name.name-local-name);
    else
      error(make(<namespace-has-no-short-name>, format-arguments: ns));
    end if;
  else
    as(type, name.name-local-name);
  end if;
end method xml-name-as-string;

define class <xml-name-empty-error> (<xml-name-error>)
  inherited slot condition-format-string
    = "Trying to create an empty name.";
  inherited slot condition-format-arguments = #();
end class <xml-name-empty-error>;

define generic string-as-xml-name
    (string :: <string>, #key allow-empty-name?, local-namespaces)
 => (name :: <xml-name>);

define method string-as-xml-name
    (string :: <string>, 
     #key allow-empty-name?,
          local-namespaces = *local-namespaces*) => (name :: <xml-name>);
  let index = find-key(string, \=(_, ':'));
  let namespace = if (index)
                    let ns-name = copy-sequence(string, start: 0, end: index);
                    find-local-namespace(ns-name, local-namespaces: local-namespaces);
                  else
                    default-namespace();
                  end if;
  let local-name = if (index)
                     copy-sequence(string, start: index + 1)
                   else
                     string;
                   end if;
  if (local-name.empty? & ~allow-empty-name?)
    error(make(<xml-name-empty-error>));
  else
    make(<xml-name>, namespace: namespace, local-name: local-name);
  end if;
end method string-as-xml-name;

define method as
    (type :: subclass(<string>), name :: <xml-name>) => (namestring :: <string>);
  xml-name-as-string(type, name, fully-qualified?: if-possible:);
end method as;

// Tools for namespace conversion.

define method xml-namespace-prefix
    (string :: <string>) => (prefix :: false-or(<string>));

end method xml-namespace-prefix;

define constant $xmlns-size = 5;

define method xml-namespace-attribute?
    (attr :: <attribute>) => (well? :: <boolean>);
  let name = attr.name-with-proper-capitalization;
  let sz = name.size;
  local method check-xmlns-prefix ()
          iterate loop (i = 0)
            if (i >= $xmlns-size)
              #t;
            else
              // should this be case-insensitive-equal?
              name[i] = "xmlns"[i] & loop(i + 1);
            end if;
          end iterate;
        end method check-xmlns-prefix;
  if (sz > $xmlns-size)
    check-xmlns-prefix() & name[$xmlns-size] = ':';
  elseif (sz = $xmlns-size)
    check-xmlns-prefix();
  else
    #f;
  end if;
end method xml-namespace-attribute?;

// This method does not check that name actually starts with "xmlns"!
define method unprefixed-namespace-name
    (name :: <string>) => (name :: false-or(<string>));
  if (name.size > $xmlns-size)
    copy-sequence(name, start: $xmlns-size + 1);
  else
    #f;
  end if;
end method unprefixed-namespace-name;
    

define generic local-xml-namespaces
    (node :: <xml>)
 => (local-namespaces :: <list>, default-namespace :: <xml-namespace>);

// Only elements can introduce new namespaces.
//
define method local-xml-namespaces
    (node :: <xml>) 
 => (local-namespaces :: <list>, default-namespace :: <xml-namespace>);
  values(#(), default-namespace());
end method local-xml-namespaces;

define class <duplicate-definition-for-default-namespace>
    (<xml-namespace-error>)
  inherited slot condition-format-string
    = "Duplicate definition for default namespace in %S.";
end class <duplicate-definition-for-default-namespace>;

define method local-xml-namespaces
    (node :: <element>)
 => (local-namespaces :: <list>, default-namespace :: <xml-namespace>);
  let node-attributes = node.attributes;
  let namespace-attributes = choose(xml-namespace-attribute?, node-attributes);
  if (empty?(namespace-attributes))
    values(#(), default-namespace());
  else
    let result = #();
    let default = default-namespace();
    do(method (attr)
         // format-out("In local method %=\n", result);
         let name = unprefixed-namespace-name(attr.name-with-proper-capitalization);
         let ns = make(<xml-namespace>, 
                       url: attr.attribute-value,
                       short-name: name);
         if (name)
           // format-out("found namespace: %=", name);
           result := pair(make(<namespace-binding>,
                               local-short-name: name,
                               local-namespace: ns),
                          result);
         else
           if (default == ns)
             // format-out("No change.\n");
             // do nothing
           elseif (default = default-namespace())
             // format-out("found default.\n");
             default := ns;
           else
             error(make(<duplicate-definition-for-default-namespace>,
                        condition-format-arguments: list(node)));
           end if;
         end if;
       end method,
       namespace-attributes);
    values(result, default);
  end if;
end method local-xml-namespaces;

define macro with-local-xml-namespaces
  { with-local-xml-namespaces (?bindings:expression)
      ?body:*
    end }
 => { dynamic-bind (*local-namespaces* = concatenate(?bindings, *local-namespaces*))
        // format-out("locals: %=", *local-namespaces*);
        ?body
      end }
end macro with-local-xml-namespaces;

