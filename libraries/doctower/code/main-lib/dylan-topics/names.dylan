module: dylan-topics


//
// Presentation form of canonical-name
//


define function presentation-name
   (defn :: <definition>, #key method-params :: false-or(<sequence>) = #f)
=> (pres :: <string>)
   if (method-params)
      let types = param-types(method-params, defn.canonical-name.enclosing-name);
      let type-list = types.item-string-list | "";
      format-to-string("%s(%s)", defn.canonical-name.local-name, type-list)
            .standardize-presentation-name
   else
      defn.canonical-name.local-name.standardize-presentation-name
   end if
end function;


//
// Title form of canonical-name
//


define generic canonical-title (defn :: <definition>, #key method-params)
=> (title :: <title-seq>);

define method canonical-title (defn :: <library>, #key method-params)
=> (title :: <title-seq>)
   let api-name = make(<api/parm-name>, text: defn.presentation-name,
         source-location: defn.canonical-name.source-location);
   title-seq(api-name, " Library")
end method;

define method canonical-title (defn :: <module>, #key method-params)
=> (title :: <title-seq>)
   let api-name = make(<api/parm-name>, text: defn.presentation-name,
         source-location: defn.canonical-name.source-location);
   title-seq(api-name, " Module")
end method;

define method canonical-title (defn :: <binding>, #key method-params)
=> (title :: <title-seq>)
   let api-name = make(<api/parm-name>,
         text: presentation-name(defn, method-params: method-params),
         source-location: defn.canonical-name.source-location);
   title-seq(api-name)
end method;


//
// Fully qualified form of canonical-name
//


define function canonical-qualified-name
   (defn :: <definition>, #key method-params :: false-or(<sequence>) = #f) 
=> (fqn :: <string>)
   if (method-params)
      let types = param-types(method-params, defn.canonical-name.enclosing-name);
      let type-list = types.item-string-list | "";
      format-to-string("%s(%s)", defn.canonical-name, type-list)
            .standardize-qualified-name
   else
      format-to-string("%s", defn.canonical-name)
            .standardize-qualified-name
   end if
end function;


//
// ID form of canonical-name
//


define function canonical-id
   (defn :: <definition>, #key method-params :: false-or(<sequence>) = #f)
=> (id :: <string>)
   canonical-qualified-name(defn, method-params: method-params)
         .qualified-name-as-id
end function;


//
// Aliases
//


define method make-namespace-names (topic :: <api-doc>, defn :: <definition>)
=> ()
   let names-table = names-by-namespace(defn.aliases, defn.canonical-name);
   for (names keyed-by namespace in names-table)
      let namespace-string
            = format-to-string("%s", namespace).standardize-qualified-name;
      topic.names-in-namespace[namespace-string]
            := map(standardize-presentation-name, names)
   end for;
end method;

define method make-namespace-names (topic :: <api-doc>, meth :: <generic-method>)
=> ()
   let names-table = names-by-namespace
         (meth.generic-binding.aliases, meth.generic-binding.canonical-name);
   let method-params = meth.method-defn.param-list.req-params;
   for (names keyed-by namespace in names-table)
      let types = param-types(method-params, namespace);
      let type-list = types.item-string-list | "";
      names := replace-elements!(names, always(#t),
            method (generic-name :: <string>) => (method-name :: <string>)
               format-to-string("%s(%s)", generic-name, type-list)
                     .standardize-presentation-name
            end);
      let namespace-string
            = format-to-string("%s", namespace).standardize-qualified-name;
      topic.names-in-namespace[namespace-string] := names;
   end for;
end method;


define function names-by-namespace
   (source-names :: <sequence>, canonical-name :: <source-name>)
=> (namespace-table :: <table>)
   let namespace-table = make(<equal-table>);
   let exported-names = choose(exported-name?, source-names);
   let names-by-namespace = group-elements(exported-names,
         test: method (n1 :: <source-name>, n2 :: <source-name>)
               => (same-namespace? :: <boolean>)
                  n1.enclosing-name = n2.enclosing-name
               end);
   for (name-group in names-by-namespace)
      name-group := promote-canonical-local-name(name-group, canonical-name);
      let namespace = name-group.first.enclosing-name;
      namespace-table[namespace] := map(local-name, name-group);
   end for;
   namespace-table
end function;


//
// Parameter names
//


/// Synopsis: Gets text for method argument types using their names in a
/// namespace.
/// 
/// Arguments:
///    params    - A <sequence> of <req-param>.
///    namespace - A <source-name> indicating the desired namespace of argument
///                types.
/// Values:
///    types - A <sequence> of the <string> equivalents of 'params'.
define function param-types
   (params :: <sequence>, namespace :: <source-name>)
=> (types :: <sequence>)
   let type-frags = map(type, params);
   type-frags := replace-elements!(type-frags, false?, always($object-type));
   let frags-text = map(source-text, type-frags);
   let local-frags-text = map(rcurry(namespace-source-text, namespace), frags-text);
   map(source-text-as-string, local-frags-text)
end function;


//
// Source name and source text conversion
//


/// Synopsis: Converts locally defined <source-name>s in a <sequence> to their
/// <source-name>s in a particular namespace.
define function namespace-source-text
   (src-text :: <sequence>, namespace :: <source-name>)
=> (src-text :: <sequence>)
   local method new-name (name :: <source-name>) => (name :: <source-name>)
            let definition = element(*definitions*, name, default: #f);
            if (definition)
               let source-names = choose
                     (method (alias :: <source-name>) => (choose? :: <boolean>)
                         alias.enclosing-name = namespace
                      end, definition.aliases);
               source-names := promote-canonical-local-name
                     (source-names, definition.canonical-name);
               if (source-names.empty?)
                  name
               else
                  source-names.first
               end if
            else
               name
            end if
         end method;

   let new-text = src-text.shallow-copy;
   replace-elements!(new-text, rcurry(instance?, <source-name>), new-name)
end function;


/// Synopsis: Converts source text (a sequence of <source-name> and <character>)
/// into a string.
define function source-text-as-string (src-text :: <sequence>)
=> (str :: <string>)
   let str = make(<stretchy-vector>);
   for (elem in src-text)
      select (elem by instance?)
         <character> =>
            str := add!(str, elem);
         <source-name> =>
            str := concatenate!(str, elem.local-name);
      end select;
   end for;
   as(<string>, str)
end function;


/// Synopsis: Make the most important source name (i.e. the first one) the one
/// that is most like the provided canonical name.
define function promote-canonical-local-name
   (source-names :: <sequence>, canonical-name :: <source-name>)
=> (source-names :: <sequence>)
   let canonical-local = canonical-name.local-name;
   let promote-index = find-key(source-names,
         method (sn :: <source-name>) => (found? :: <boolean>)
            case-insensitive-equal?(sn.local-name, canonical-local)
         end);
   if (promote-index)
      concatenate(vector(source-names[promote-index]),
                  copy-sequence(source-names, end: promote-index),
                  copy-sequence(source-names, start: promote-index + 1))
   else
      source-names
   end if
end function;
