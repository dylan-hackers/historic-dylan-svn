module: dylan-topics


define function source-name-as-standardized-id (name :: <source-name>) => (id :: <string>)
   let name-string = format-to-string("%s", name);
   name-string.standardize-qualified-name.qualified-name-as-id
end function;


//
// Standard form of canonical-name
//


define generic definition-qualified-name (defn :: <definition>, #key) 
=> (id :: <string>);

define method definition-qualified-name (defn :: <definition>, #key) 
=> (id :: <string>)
   standardize-qualified-name(format-to-string("%s", defn.canonical-name))
end method;

define method definition-qualified-name
   (defn :: <generic-binding>, #key method-params :: false-or(<sequence>))
=> (id :: <string>)
   if (method-params)
      let types = canonical-param-types(method-params, #t);
      let type-list = types.item-string-list | "";
      standardize-qualified-name
            (format-to-string("%s(%s)", defn.canonical-name, type-list))
   else
      next-method()
   end if;
end method;


//
// ID form of canonical-name
//


define generic canonical-id (defn :: <definition>, #key) => (id :: <string>);

define method canonical-id (defn :: <definition>, #rest keys, #key, #all-keys)
=> (id :: <string>)
   let name = apply(definition-qualified-name, defn, keys);
   name.qualified-name-as-id
end method;


//
// Title form of canonical-name
//


define generic canonical-title (defn :: <definition>, #key) => (title :: <title-seq>);

define method canonical-title (defn :: <library>, #key) => (title :: <title-seq>)
   let name = defn.canonical-name.local-name;
   title-seq(format-to-string("%s Library", name.as-titlecase))
end method;

define method canonical-title (defn :: <module>, #key) => (title :: <title-seq>)
   let name = defn.canonical-name.local-name;
   title-seq(format-to-string("%s Module", name.as-titlecase))
end method;

define method canonical-title (defn :: <binding>, #key) => (title :: <title-seq>)
   title-seq(defn.canonical-name.local-name.as-titlecase)
end method;

define method canonical-title
   (defn :: <generic-binding>, #key method-params :: false-or(<sequence>))
=> (id :: <title-seq>)
   if (method-params)
      let types = canonical-param-types(method-params, #f);
      let type-list = types.item-string-list | "";
      let title = format-to-string("%s(%s)",
            defn.canonical-name.local-name, type-list);
      title-seq(standardize-title(title))
   else
      next-method()
   end if
end method;


//
// Helpers
//


/// Synopsis: Gets text for method argument types using their canonical names.
/// 
/// Arguments:
///    params   - A <sequence> of <req-param>.
///    scoped?  - A <boolean> indicating whether the names should include scope
///               information.
/// Values:
///    types - A <sequence> of the <string> equivalents of 'params'.
define method canonical-param-types (params :: <sequence>, scoped? :: <boolean>)
=> (types :: <sequence>)
   let type-frags = map(type, params);
   type-frags := replace-elements!(type-frags, false?, always($object-type));
   let local-types = map(source-text, type-frags);
   let canon-types = map(canonical-source-text, local-types);
   map(rcurry(source-text-as-string, scoped?), canon-types);
end method;


/// Synopsis: Converts locally defined <source-name>s in a <sequence> to their
/// canonical <source-name>s.
define method canonical-source-text (src-text :: <sequence>)
=> (src-text :: <sequence>)
   local method new-name (name :: <source-name>) => (name :: <source-name>)
            let definition = element(*definitions*, name, default: #f);
            if (definition) definition.canonical-name else name end
         end method;
   let new-text = src-text.shallow-copy;
   replace-elements!(new-text, rcurry(instance?, <source-name>), new-name)
end method;


/// Synopsis: Converts source text (a sequence of <source-name> and <character>)
/// into a string.
define method source-text-as-string (src-text :: <sequence>, scoped? :: <boolean>)
=> (str :: <string>)
   let str = make(<stretchy-vector>);
   for (elem in src-text)
      select (elem by instance?)
         <character> =>
            str := add!(str, elem);
         <source-name> =>
            str := concatenate!(str, if (scoped?) format-to-string("%s", elem)
                                     else elem.local-name end if);
      end select;
   end for;
   as(<string>, str)
end method;
