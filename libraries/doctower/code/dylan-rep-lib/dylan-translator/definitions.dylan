module: dylan-translator
synopsis: Common code dealing with definition and fragment representations.


/// Synopsis: Add a new definition to an existing collection, replacing or
/// merging if necessary.
///
/// --- Arguments: ---
/// context           - The <context>.
/// scope-definitions - A <skip-list> containing other definitions (keyed by
///                     local name) in the scope to which 'new-definition' is
///                     being added.
/// new-definition    - The <definition> to add to 'scope-definitions'.
/// name              - The <source-name> under which the definition is being
///                     added, in the same scope as 'scope-definitions'.
///
/// --- Values: ---
/// new-definition - The new, merged, or replaced definition in
///                  'scope-definitions'. May or may not be identical to the
///                  argument.
define method add-definition
   (context :: <context>, scope-definitions :: <skip-list>,
    new-definition :: <definition>, name :: <source-name>)
=> (new-definition :: <definition>)
   let existing-definition = element(scope-definitions, name.local-name, default: #f);
   let merged-definition = merge-definitions
         (context, existing-definition, new-definition);
   scope-definitions[name.local-name] := merged-definition;
   merged-definition
end method;


define method note-replacement
   (context :: <context>, replacement :: <definition>, original :: <definition>)
=> ()
   replace-elements!(context.replacements, curry(\==, original),
                     always(replacement));
   context.replacements[original] := replacement;
   context.changed? := #t;
end method;


/// Synopsis: Merge two definitions, creating a new definition if necessary.
define generic merge-definitions
   (context :: <context>, existing-definition :: false-or(<definition>),
    new-definition :: <definition>)
=> (merged-definition :: <definition>);


/// Synopsis: Trivial case.
define method merge-definitions
   (context :: <context>, existing == #f, new :: <definition>)
=> (merged :: <definition>);
   context.changed? := #t;
   new
end method;


/// Synopsis: Failsafe method.
define method merge-definitions
   (context :: <context>, def1 :: <definition>, def2 :: <definition>)
=> (merged :: <definition>)
   unless (def1 == def2)
      error("Merging different definitions %= and %=", def1, def2)
   end unless;
   def1
end method;


/// Synopsis: Return definition with better provenance.
///
/// The better definition is the one whose source location we want to keep.
/// It depends on the definition's provenance.
define generic better-definition (existing :: <definition>, new :: <definition>)
=> (better :: <definition>, worse :: <definition>);

define method better-definition (existing :: <definition>, new :: <definition>)
=> (better :: <definition>, worse :: <definition>)
   if (existing == new)
      values (existing, new)
   else
      let ranked-provenances = 
            #[#"predefined", #"create-clause", #"generic-definition", 
              #"definition", #"declaration", #"inference", #"expression"];
      let existing-prov-rank = position(ranked-provenances, existing.provenance);
      let new-prov-rank = position(ranked-provenances, new.provenance);
      if (existing-prov-rank <= new-prov-rank)
         values (existing, new)
      else
         values (new, existing)
      end if
   end if
end method;

define method better-definition (existing :: <module>, new :: <module>)
=> (better :: <module>, worse :: <module>)
   case
      existing == new =>
         values (existing, new);
      existing.canonical-name.enclosing-name = $common-dylan-library.canonical-name =>
         values (existing, new);
      new.canonical-name.enclosing-name = $common-dylan-library.canonical-name =>
         values (new, existing);
      otherwise =>
         next-method();
   end case
end method;

define method better-definition (existing :: <binding>, new :: <binding>)
=> (better :: <binding>, worse :: <binding>)
   case
      existing == new =>
         values (existing, new);
      existing.canonical-name.enclosing-name = $common-dylan-module.canonical-name =>
         values (existing, new);
      new.canonical-name.enclosing-name = $common-dylan-module.canonical-name =>
         values (new, existing);
      otherwise =>
         next-method();
   end case
end method;


define method merge-aliases 
   (context :: <context>, better :: <definition>, worse :: <definition>)
=> ()
   let preunion-size = better.aliases.size;
   better.aliases := union(better.aliases, worse.aliases, test: \=);
   if (better.aliases.size > preunion-size)
      context.changed? := #t;
   end if;
end method;


define method merge-markup
   (context :: <context>, better :: <definition>, worse :: <definition>)
=> ()
   better.markup-tokens := concatenate!(better.markup-tokens, worse.markup-tokens);
end method;
