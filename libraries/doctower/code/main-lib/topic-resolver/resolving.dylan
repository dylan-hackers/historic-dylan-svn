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
    duplicate-title-targets :: <table>)
=> (resolved-topics :: <sequence>, tables-of-content :: <sequence>)
   let unused-catalogs = catalog-topics.copy-sequence;
   
   // Assign topics in place of placeholders.
   for (topic in topics)
      let defined-parms = sections-by-parm-name(topic);
      visit-target-placeholders(topic, resolve-target-placeholder-in-topic,
            topic: topic, resolutions: target-resolutions, parms: defined-parms,
            dup-titles: duplicate-title-targets, unused-catalogs: unused-catalogs)
   end for;

   // Assign topics in place of tables of content references.
   for (toc in tables-of-content)
      for (toc-ref :: false-or(<topic-ref>) in toc)
         if (toc-ref)
            resolve-target-placeholder-in-topic(toc-ref.target,
                  setter: rcurry(target-setter, toc-ref),
                  topic: #f, resolutions: target-resolutions, parms: #f,
                  dup-titles: duplicate-title-targets,
                  unused-catalogs: unused-catalogs);
         end if;
      end for;
   end for;
   
   // Remove unused catalog topics.
   let topics = reduce(remove!, topics, unused-catalogs);

   values(topics, tables-of-content);
end method;


define method resolve-target-placeholder-in-topic
   (object :: <object>,
    #key setter, topic, resolutions, parms, dup-titles, unused-catalogs)
=> (visit-slots? :: <boolean>)
   // Allow recursion in the general case.
   #t
end method;


define method resolve-target-placeholder-in-topic
   (topic :: <topic>,
    #key setter, topic: current-topic, resolutions, parms, dup-titles, unused-catalogs)
=> (visit-slots? :: <boolean>)
   // Only allow recursion into current topic.
   topic == current-topic
end method;


define method resolve-target-placeholder-in-topic
   (xref :: <xref>,
    #key setter, topic: current-topic, resolutions, parms, dup-titles, unused-catalogs)
=> (visit-slots? :: <boolean>)
   if (instance?(xref.target, <target-placeholder>))
      let placeholder :: <target-placeholder> = xref.target;
      let (resolution, replace-text-with-title?) =
            begin
               let local-res = resolve-parm-link(placeholder, current-topic, parms);
               if (local-res)
                  values(local-res, #f)
               else
                  values(resolve-link(placeholder, current-topic, resolutions),
                         xref.target-from-text?)
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
         end if;
         #t

      elseif (xref.target-from-text?)
         // May not be intended as an actual link target. Give warning and
         // remove unlinkable xref.
         cant-resolve-xref-warning(placeholder, dup-titles);
         setter(xref.text);
         #f

      else
         // Explicit link target. Give error.
         cant-resolve-error(placeholder, dup-titles);
         #f
      end if
   end if
end method;


define method resolve-target-placeholder-in-topic
   (placeholder :: <target-placeholder>,
    #key setter, topic: current-topic, resolutions, parms, dup-titles, unused-catalogs)
=> (visit-slots? :: <boolean>)
   let resolution = resolve-link(placeholder, current-topic, resolutions);
   if (resolution)
      check-resolves-to-topic(placeholder, resolution);
      setter(resolution);
      remove!(unused-catalogs, resolution);
      #f

   else
      cant-resolve-error(placeholder, dup-titles);
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
   (placeholder :: <target-placeholder>, dup-titles :: <table>)
=> ()
   signal-cant-resolve(placeholder, dup-titles,
         ambiguous-title-in-link, target-not-found-in-link)
end method;


define method cant-resolve-xref-warning
   (placeholder :: <target-placeholder>, dup-titles :: <table>)
=> ()
   signal-cant-resolve(placeholder, dup-titles,
         ambiguous-title-in-xref, unresolvable-target-in-xref)
end method;


define method signal-cant-resolve
   (placeholder :: <target-placeholder>, dup-titles :: <table>,
    signal-ambiguous :: <function>, signal-not-found :: <function>)
=> ()
   let targets-with-title = element(dup-titles, placeholder.target, default: #f);
   if (targets-with-title)
      let locs = map(source-location, targets-with-title);
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
///   3. Fully qualified name disguised as ID
///   4. Unique title
///   5. API in current module/library
///   6. Unique API in other module/library
///
/// Case 1 is handled by 'resolve-parm-link' before this method is called. Cases
/// 2-4 are precalculated by 'resolution-info'.
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
   let topic = element(target-resolutions, link.target, default: #f);
   if (~topic)
      // It is an API or a duplicate title or something unknown.
      // TODO: API canonicalization and lookup.
   end if;
   topic
end method;


define method resolve-parm-link
   (link :: <target-placeholder>, containing-topic :: false-or(<topic>),
    section-by-parm-name :: <table>)
=> (resolution :: false-or(<section>))
   let link-text = link.target.standardize-target;
   element(section-by-parm-name, link-text, default: #f)
end method;


define method standardize-target (target :: <string>) => (cleaned :: <string>)
   let cleaned = target.copy-sequence;
   cleaned := replace-elements!(cleaned, rcurry(\=, '\n'), always(' '));
   cleaned := regexp-replace(cleaned, " {2,}", " ");
   cleaned := regexp-replace(cleaned, "^#[^ ]+ ", "");
   cleaned := regexp-replace(cleaned, ":$", "");
   cleaned
end method;

