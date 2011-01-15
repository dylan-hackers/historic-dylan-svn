module: topic-resolver


/**
Synopsis: Resolves xref, topic, and table-of-contents-related placeholders.

It discards catalog topics (such as "All Libraries" or "Modules in Io") that are
not actually referenced.

--- Conditions: ---
Signals an error if a link fails to resolve.
**/
define method resolve-target-placeholders
   (topics :: <sequence>, tables-of-content :: <sequence>,
    catalog-topics :: <sequence>, target-resolutions :: <table>,
    duplicate-title-targets :: <table>,
    duplicate-short-qualified-name-targets :: <table>)
=> (resolved-topics :: <sequence>, tables-of-content :: <sequence>)
   let unused-catalogs = as(<stretchy-vector>, catalog-topics.copy-sequence);
   
   // Assign topics in place of placeholders.
   for (topic in topics)
      let defined-parms = sections-by-parm-name(topic);
      visit-target-placeholders(topic, resolve-target-placeholder-in-topic,
            topic: topic, resolutions: target-resolutions,
            parms: defined-parms, dup-titles: duplicate-title-targets,
            dup-sqns: duplicate-short-qualified-name-targets,
            unused-catalogs: unused-catalogs)
   end for;

   // Assign topics in place of tables of content references.
   for (toc in tables-of-content)
      visit-target-placeholders(toc, resolve-target-placeholder-in-topic,
            topic: #f, resolutions: target-resolutions,
            parms: #f, dup-titles: duplicate-title-targets,
            dup-sqns: duplicate-short-qualified-name-targets,
            unused-catalogs: unused-catalogs)
   end for;
   
   // Remove unused catalog topics.
   let topics = reduce(remove!, topics, unused-catalogs);

   values(topics, tables-of-content);
end method;


define method resolve-target-placeholder-in-topic
   (object :: <object>,
    #key setter, visited, topic, resolutions, parms, dup-titles, dup-sqns,
         unused-catalogs)
=> (visit-slots? :: <boolean>)
   // Allow recursion in the general case.
   #t
end method;


define method resolve-target-placeholder-in-topic
   (xref :: <xref>,
    #key setter, visited, topic, resolutions, parms, dup-titles, dup-sqns,
         unused-catalogs)
=> (visit-slots? :: <boolean>)
   if (instance?(xref.target, <target-placeholder>))
      let placeholder :: <target-placeholder> = xref.target;
      let (resolution, replace-text-with-title?, parm-style?) =
            begin
               let local-res = resolve-parm-link(placeholder, topic, parms);
               if (local-res)
                  values(local-res, #f, #t)
               else
                  values(resolve-link(placeholder, topic, resolutions),
                         xref.target-from-text?, #f)
               end if
            end;

      if (resolution)
         if (instance?(xref, <vi-xref>))
            check-resolves-to-topic(placeholder, resolution)
         end if;
         xref.target := resolution;
         remove!(unused-catalogs, resolution);

         if (replace-text-with-title?)
            xref.text := make(<conref>, target: resolution, style: #"title",
                  source-location: placeholder.source-location)
         elseif (parm-style?)
            let content = if (instance?(xref.text, <title-seq>)) xref.text
                          else title-seq(xref.text) end;
            let parm-style = make(<api/parm-name>, text: content.stringify-title,
                  source-location: placeholder.source-location);
            xref.text := title-seq(parm-style)
         end if;
         #t

      elseif (xref.target-from-text?)
         // May not be intended as an actual link target. Give warning and
         // remove unlinkable xref.
         cant-resolve-xref-warning(placeholder, dup-titles, dup-sqns);
         setter(xref.text);
         #f

      else
         // Explicit link target. Give error.
         cant-resolve-error(placeholder, dup-titles, dup-sqns);
         #f
      end if
   end if
end method;


// TODO: Conrefs can target a section title. Make a new method to handle that,
// similar to the xref method above.


define method resolve-target-placeholder-in-topic
   (placeholder :: <target-placeholder>,
    #key setter, visited, topic, resolutions, parms, dup-titles, dup-sqns,
         unused-catalogs)
=> (visit-slots? :: <boolean>)
   let resolution = resolve-link(placeholder, topic, resolutions);
   if (resolution)
      check-resolves-to-topic(placeholder, resolution);
      setter(resolution);
      remove!(unused-catalogs, resolution);
      #f

   else
      cant-resolve-error(placeholder, dup-titles, dup-sqns);
      #t
   end if;
end method;


define method check-resolves-to-topic
   (placeholder :: <target-placeholder>, resolution :: <topic>)
=> ()
   #f
end method;


define method check-resolves-to-topic
   (placeholder :: <target-placeholder>, resolution :: <section>)
=> ()
   section-where-topic-required-in-link(location: placeholder.source-location,
         section-location: resolution.source-location)
end method;


define method cant-resolve-error
   (placeholder :: <target-placeholder>, dup-titles :: <table>, dup-sqns :: <table>)
=> ()
   signal-cant-resolve(placeholder, dup-titles, dup-sqns,
         ambiguous-title-in-link, target-not-found-in-link)
end method;


define method cant-resolve-xref-warning
   (placeholder :: <target-placeholder>, dup-titles :: <table>, dup-sqns :: <table>)
=> ()
   signal-cant-resolve(placeholder, dup-titles, dup-sqns,
         ambiguous-title-in-xref, unresolvable-target-in-xref)
end method;


define method signal-cant-resolve
   (placeholder :: <target-placeholder>, dup-titles :: <table>, dup-sqns :: <table>,
    signal-ambiguous :: <function>, signal-not-found :: <function>)
=> ()
   let possible-targets = element(dup-sqns, placeholder.target, default: #f)
         | element(dup-titles, placeholder.target, default: #f);
   if (possible-targets & ~possible-targets.empty?)
      let locs = map(source-location, possible-targets);
      signal-ambiguous(location: placeholder.source-location,
            target-text: placeholder.target,
            target-locations: locs.item-string-list);
   else
      signal-not-found(location: placeholder.source-location,
            target-text: placeholder.target);
   end if
end method;


/// Synopsis: Determines what topic a link refers to.
///
/// In general, a link is resolved to one of the following, in order:
///   1. Local argument/value name
///   2. ID
///   3. Fully qualified name in ID form (e.g. "::Dylan:Dylan:Make")
///   4. Fully qualified name in abbreviated ID form (e.g. ":::Make")
///   5. Unique title
///   6. API in current module/library
///   7. Unique API in other module/library
///
/// Case 1 is handled by 'resolve-parm-link' before this method is called. Cases
/// 2, 3, and 5 are precalculated by 'resolution-info'. Cases 4, 6, and 7 are
/// checked in this method if necessary. Technically, this method works out of 
/// order since it checks case 4 after 2-5, but there is no possibility of an
/// incorrect resolution because IDs are unique and titles cannot start with a
/// colon.
///
/// Arguments:
///   link               - The <target-placeholder> to resolve.
///   containing-topic   - The topic containing the link, used to look up the
///                        current module/library.
///   target-resolutions - A <table> mapping target strings to resolved targets.
///
/// Values:
///   resolution  -  #f if link could not be resolved, else the <topic> or
///                  <section> it resolved to.
///
define method resolve-link
   (link :: <target-placeholder>, containing-topic :: false-or(<topic>),
    target-resolutions :: <table>)
=> (resolution :: false-or(type-union(<topic>, <section>)))
   let link-text = link.target;
   let topic = element(target-resolutions, link-text, default: #f);

   // It is an API or a duplicate title or something unknown.
   if (~topic)
      // TODO: API canonicalization and lookup.
   end if;
   topic
end method;


define method resolve-parm-link
   (link :: <target-placeholder>, containing-topic :: false-or(<topic>),
    section-by-parm-name :: <table>)
=> (resolution :: false-or(<section>))
   let link-text = link.target.standardize-parm-target;
   element(section-by-parm-name, link-text, default: #f)
end method;


define method standardize-parm-target (target :: <string>) => (cleaned :: <string>)
   let cleaned = target.copy-sequence;
   cleaned := replace-elements!(cleaned, rcurry(\=, '\n'), always(' '));
   cleaned := regexp-replace(cleaned, " {2,}", " ");
   cleaned := regexp-replace(cleaned, "^#[^ ]+ ", "");
   cleaned := regexp-replace(cleaned, ":$", "");
   cleaned
end method;

