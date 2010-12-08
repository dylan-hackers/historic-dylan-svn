module: output


define constant $dita-templates = #[
   #"dita-map",
   #"dita-map-recursion",
   #"dita-topic",
   #"dita-concept",
   #"dita-reference",
   #"dita-section",
   #"dita-catalog",
   #"dita-unordered-list",
   #"dita-defn-list",
   #"dita-parm-list"
];


define method output-templates (output == #"dita") => (templates :: <sequence>)
   $dita-templates
end method;


define method output-file-info
   (output == #"dita", doc-tree :: <ordered-tree>)
=> (topic-files :: <table>, special-files :: <table>, file-info :: <sequence>)
   let topic-count = max(0, doc-tree.size - 1); // Ignore first element, it is #f.
   let topic-files = make(<table>, size: topic-count);
   let file-info = make(<stretchy-vector>);
   let special-files = make(<table>);  // None needed.

   let topic-seq = as(<vector>, doc-tree);
   let con-topics = choose(rcurry(instance?, <con-topic>), topic-seq);
   let ref-topics = choose(rcurry(instance?, <ref-topic>), topic-seq);
   let con-dir = as(<directory-locator>, "concepts");
   let ref-dir = as(<directory-locator>, "reference");

   // Topic files
   
   local method set-topic-info (topic-list, topic-dir) => ()
            let topic-digits = topic-list.size.digits;
            let topic-idx = 0;
            for (topic in topic-list)
               let prefix-part = format-to-string("%0*d", topic-digits, topic-idx + 1);
               let title-part = as-filename-part(topic.title.stringify-title);
               let base-name = format-to-string("%s_%s", prefix-part, title-part);
               let locator = make(<file-locator>,
                     directory: topic-dir, base: base-name, extension: "xml");
               let topic-file = make(<topic-output-file>, file: locator, topic: topic);
               topic-files[topic] := topic-file;
               file-info := add!(file-info, topic-file);
               topic-idx := topic-idx + 1;
            end for;
         end method;
   
   set-topic-info(con-topics, con-dir);
   set-topic-info(ref-topics, ref-dir);
   
   // Ditamap file

   let ditamap-locator = make(<file-locator>, base: "doctower", extension: "ditamap");
   let ditamap-file = make(<toc-output-file>, tree: doc-tree, file: ditamap-locator);
   file-info := add!(file-info, ditamap-file);
   
   values(topic-files, special-files, file-info)
end method;


define method target-link-info
   (output == #"dita", doc-tree :: <ordered-tree>, fallback-ids :: <table>,
    file-info :: <table>)
=> (target-info :: <table>)
   let target-info = make(<table>);
   for (topic in doc-tree)
      unless (~topic) // Root of doc-tree is #f
         visit-targets(topic, add-dita-link-info, target-info: target-info,
               current-topic: topic, fallback-ids: fallback-ids,
               output-file: file-info[topic])
      end unless
   end for;
   target-info
end method;


define method add-dita-link-info
   (object :: <object>,
    #key setter, visited, target-info, current-topic, fallback-ids, output-file)
=> (visit-slots? :: <boolean>)
   #t
end method;


define method add-dita-link-info
   (topic :: <topic>,
    #key setter, visited, target-info, current-topic, fallback-ids, output-file)
=> (visit-slots? :: <boolean>)
   let raw-topic-id = topic.id | fallback-ids[topic];
   let topic-id = raw-topic-id.sanitized-id;
   let raw-title-id = ":Title";
   let title-id = raw-title-id.sanitized-id;
   let raw-shortdesc-id = ":Synopsis";
   let shortdesc-id = raw-shortdesc-id.sanitized-id;
   let filename = output-file.locator.sanitized-url-path;
   let topic-href = format-to-string("%s#%s", filename, topic-id);
   let title-href = format-to-string("%s#%s/%s", filename, topic-id, title-id);
   let shortdesc-href = format-to-string("%s#%s/%s", filename, topic-id, shortdesc-id);
   target-info[topic] := make(<topic-target>,
         id: topic-id, href: topic-href, markup-id: raw-topic-id,
         title-id: title-id, title-href: title-href, title-markup-id: raw-title-id,
         shortdesc-id: shortdesc-id, shortdesc-href: shortdesc-href,
         shortdesc-markup-id: raw-shortdesc-id);
   #t
end method;


define method add-dita-link-info
   (sect :: <section>,
    #key setter, visited, target-info, current-topic, fallback-ids, output-file)
=> (visit-slots? :: <boolean>)
   let raw-topic-id = current-topic.id | fallback-ids[current-topic];
   let topic-id = raw-topic-id.sanitized-id;
   let raw-section-id = sect.id | fallback-ids[sect];
   let raw-title-id = format-to-string(":Title(%s)", raw-section-id);
   let section-id = raw-section-id.sanitized-id;
   let title-id = raw-title-id.sanitized-id;
   let filename = output-file.locator.sanitized-url-path;
   let section-href = format-to-string("%s#%s/%s", filename, topic-id, section-id);
   let title-href = format-to-string("%s#%s/%s", filename, topic-id, title-id);
   target-info[sect] := make(<section-target>,
         id: section-id, href: section-href, markup-id: raw-section-id,
         title-id: title-id, title-href: title-href, title-markup-id: raw-title-id);
   #t
end method;


define method add-dita-link-info
   (content :: type-union(<footnote>, <ph-marker>),
    #key setter, visited, target-info, current-topic, fallback-ids, output-file)
=> (visit-slots? :: <boolean>)
   let topic-id = (current-topic.id | fallback-ids[current-topic]).sanitized-id;
   let content-id = fallback-ids[content].sanitized-id;
   let filename = output-file.locator.sanitized-url-path;
   let content-href = format-to-string("%s#%s/%s", filename, topic-id, content-id);
   let info-class =
         select (content by instance?)
            <footnote> => <footnote-target>;
            <ph-marker> => <ph-marker-target>;
         end select;
   target-info[content] := make(info-class, id: content-id, href: content-href);
   #t
end method;


define method write-output-file
   (output == #"dita", file-info :: <toc-output-file>,
    link-map :: <table>, target-info :: <table>, special-file-info :: <table>)
=> ()
   let keys-with-root = file-info.tree.key-sequence;
   let all-sibling-sequences = map(inf-key-sequence, keys-with-root);
   let sibling-sequences = choose
         (method (seq) seq.size > 1 end, all-sibling-sequences);
   let full-sequence = copy-sequence(keys-with-root, start: 1);
   
   let var-table = table(<case-insensitive-string-table>,
         "package-title" => *package-title*,
         "root-topics" => file-info.tree.root-key.inf-key-sequence,
         "full-sequence" => full-sequence,
         "sibling-sequences" => sibling-sequences
         );

   let ops-table = table(<case-insensitive-string-table>,
         "href" =>
               method (key :: <ordered-tree-key>) => (href :: <string>)
                  let topic = file-info.tree[key];
                  target-info[topic].target-href
               end method,
         "child-recursion" =>
               identity /* this is replaced with local method below */
         );

   local method do-child-recursion (key :: <ordered-tree-key>) => (text :: <string>)
            let child-keys = key.inf-key-sequence;
            let vars = table(<case-insensitive-string-table>,
                  "children" => child-keys);
            text-from-template(#"dita-map-recursion",
                  variables: vars, operations: ops-table)
         end method;
   
   ops-table["child-recursion"] := do-child-recursion;
   output-from-template(#"dita-map", file-info,
         variables: var-table, operations: ops-table);
end method;


define method write-output-file
   (output == #"dita", file-info :: <topic-output-file>,
    link-map :: <table>, target-info :: <table>, special-file-info :: <table>)
=> ()
   let var-table = table(<case-insensitive-string-table>,
         "topic" => file-info.topic
         );
   
   let ops-table = table(<case-insensitive-string-table>,
         "id" =>
               method (topic :: <topic>) => (id :: <string>)
                  target-info[topic].target-id
               end method,
         "title-id" =>
               method (topic :: <topic>) => (id :: <string>)
                  target-info[topic].title-id
               end method,
         "shortdesc-id" =>
               method (topic :: <topic>) => (id :: <string>)
                  target-info[topic].shortdesc-id
               end method,
         "markup-id" =>
               method (topic :: <topic>) => (id :: <string>)
                  target-info[topic].markup-id
               end method,
         "title-markup-id" =>
               method (topic :: <topic>) => (id :: <string>)
                  target-info[topic].title-markup-id
               end method,
         "shortdesc-markup-id" =>
               method (topic :: <topic>) => (id :: <string>)
                  target-info[topic].shortdesc-markup-id
               end method,
         "formatted-title" =>
               method (topic :: <topic>) => (dita :: <string>)
                  dita-content(topic.title, target-info)
               end method,
         "formatted-shortdesc" =>
               method (topic :: <topic>) => (dita :: <string>)
                  if (topic.shortdesc)
                     // Skip the <p> tag; not allowed in <shortdesc>.
                     dita-content(topic.shortdesc.content, target-info)
                  else
                     ""
                  end if
               end method,
         "main-body" =>
               method (topic :: <topic>) => (dita :: <string>)
                  dita-main-body(topic, target-info)
               end method,
         "declarations-section" =>
               rcurry(dita-section, declarations-section, target-info),
         "syntax-section" =>
               rcurry(dita-section, syntax-section, target-info),
         "adjectives-section" =>
               rcurry(dita-section, adjectives-section, target-info),
         "conds-section" =>
               rcurry(dita-section, conds-section, target-info),
         "args-section" =>
               rcurry(dita-section, args-section, target-info),
         "vals-section" =>
               rcurry(dita-section, vals-section, target-info),
         "keywords-section" =>
               rcurry(dita-section, keywords-section, target-info),
         "value-section" =>
               rcurry(dita-section, value-section, target-info),
         "inheritables-section" =>
               rcurry(dita-section, inheritables-section, target-info),
         "supers-section" =>
               rcurry(dita-section, supers-section, target-info),
         "subs-section" =>
               rcurry(dita-section, subs-section, target-info),
         "funcs-on-section" =>
               rcurry(dita-section, funcs-on-section, target-info),
         "funcs-returning-section" =>
               rcurry(dita-section, funcs-returning-section, target-info),
         "modules-section" =>
               rcurry(dita-section, modules-section, target-info),
         "bindings-section" =>
               rcurry(dita-section, bindings-section, target-info),
         "related-links" =>
               method (topic :: <topic>) => (dita-links :: <sequence>)
                  map(rcurry(dita-content, target-info), topic.related-links)
               end method,
         "footnotes" => footnotes,
         "size" => size
         );

   let templ :: <symbol> =
         select (file-info.topic by instance?)
            <ref-topic> => #"dita-reference";
            <con-topic> => #"dita-concept";
            <topic> => #"dita-topic";
         end select;
   output-from-template(templ, file-info,
         variables: var-table, operations: ops-table);
end method;


define method dita-catalog-content (topic :: <catalog-topic>, target-info)
=> (dita :: <string>)
   let api-refs = map(rcurry(dita-content, target-info), topic.api-xrefs);
   if (api-refs.empty?)
      ""
   else
      let vars = table(<case-insensitive-string-table>, "items" => api-refs);
      text-from-template(#"dita-catalog", variables: vars)
   end if
end method;


define method dita-main-body (topic :: <topic>, target-info)
=> (dita :: <string>)
   dita-content(topic.content, target-info)
end method;


define method dita-main-body (topic :: <catalog-topic>, target-info)
=> (dita :: <string>)
   if (topic.topic-type = #"catalog")
      concatenate(next-method(), dita-catalog-content(topic, target-info))
   else
      next-method()
   end if
end method;


define method dita-section
   (topic :: <topic>, accessor :: <function>, target-info,
    #key prepend :: <string> = "")
=> (dita :: <string>)
   let dita = 
         if (applicable-method?(accessor, topic))
            let sect :: false-or(<section>) = topic.accessor;
            if (sect)
               let section-content =
                     concatenate(prepend, dita-content(sect.content, target-info));
               if (~section-content.empty?)
                  let vars = table(<case-insensitive-string-table>,
                        "id" =>
                              target-info[sect].target-id,
                        "title-id" =>
                              target-info[sect].title-id,
                        "markup-id" =>
                              target-info[sect].markup-id,
                        "title-markup-id" =>
                              target-info[sect].title-markup-id,
                        "formatted-title" =>
                              dita-content(sect.title, target-info),
                        "content" =>
                              section-content
                        );
                  text-from-template(#"dita-section", variables: vars);
               end if
            end if
         end if;
   dita | ""
end method;


define method dita-section
   (topic :: <library-doc>, accessor == modules-section, target-info, #key)
=> (dita :: <string>)
   next-method(topic, accessor, target-info,
         prepend: dita-catalog-content(topic, target-info))
end method;


define method dita-section
   (topic :: <module-doc>, accessor == bindings-section, target-info, #key)
=> (dita :: <string>)
   next-method(topic, accessor, target-info,
         prepend: dita-catalog-content(topic, target-info))
end method;


define method dita-content (obj :: <object>, target-info)
=> (dita :: <string>)
   /**/
   format-to-string("%=", obj).sanitized-xml;
end method;


define method dita-content (seq :: <sequence>, target-info)
=> (dita :: <string>)
   let dita-elems = map-as(<vector>, rcurry(dita-content, target-info), seq);
   reduce(concatenate, "", dita-elems)
end method;


define method dita-content (str :: <string>, target-info)
=> (dita :: <string>)
   sanitized-xml(str)
end method;


define method dita-content (char :: <character>, target-info)
=> (dita :: <string>)
   sanitized-xml(as(<string>, char))
end method;


define method dita-content (xref :: <xref>, target-info)
=> (dita :: <string>)
   // TODO: Xref output for footnotes and ph-markers.
   let title = dita-content(xref.text, target-info);
   let (href, scope) =
         select (xref.target by instance?)
            (<topic>, <section>) =>
               let base-href = target-info[xref.target].target-href;
               let href = concatenate("../", base-href).sanitized-xml;
               values(href, #f);
            <url> =>
               let href = xref.target.sanitized-url.sanitized-xml;
               values(href, "external");
            otherwise =>
               values(#f, #f);
         end select;
   let (format, type) =
         select (xref.target by instance?)
            <ref-topic> => values("dita", "reference");
            <con-topic> => values("dita", "concept");
            <topic>     => values("dita", "topic");
            <section>   => values(#f, "section");
            <url>       => values(#f, #f);
            otherwise   => values(#f, #f);
         end select;
   let scope-attr =
         if (scope) format-to-string("scope=\"%s\" ", scope) else "" end;
   let format-attr =
         if (format) format-to-string("format=\"%s\" ", format) else "" end;
   let type-attr =
         if (type) format-to-string("type=\"%s\" ", type) else "" end;
   format-to-string("<xref %s%s%shref=\"%s\"><ph>%s</ph></xref>",
         scope-attr, format-attr, type-attr, href, title);
end method;


define method dita-content (conref :: <conref>, target-info)
=> (dita :: <string>)
   let href-func = if (conref.style = #"title") title-href else shortdesc-href end;
   format-to-string("<ph conref=\"../%s\"/>",
         target-info[conref.target].href-func.sanitized-xml)
end method;


define method dita-content (link :: <topic-ref>, target-info)
=> (dita :: <string>)
   let (href, scope, format) =
         select (link.target by instance?)
            <topic> =>
               let base-href = target-info[link.target].target-href;
               let href = concatenate("../", base-href).sanitized-xml;
               values(href, #f, "dita");
            <url> =>
               let href = link.target.sanitized-url.sanitized-xml;
               values(href, "external", #f);
         end select;
   let type =
         select (link.target by instance?)
            <ref-topic> => "reference";
            <con-topic> => "concept";
            <topic>     => "topic";
            <url>       => #f;
         end select;
   let scope-attr =
         if (scope) format-to-string("scope=\"%s\" ", scope) else "" end;
   let format-attr =
         if (format) format-to-string("format=\"%s\" ", format) else "" end;
   let type-attr =
         if (type) format-to-string("type=\"%s\" ", type) else "" end;
   format-to-string("<link %s%s%shref=\"%s\"/>",
         scope-attr, format-attr, type-attr, href);
end method;


define method dita-content (ul :: <unordered-list>, target-info)
=> (dita :: <string>)
   let dita-items = map(rcurry(dita-content, target-info), ul.items);
   let vars = table(<case-insensitive-string-table>, "items" => dita-items);
   text-from-template(#"dita-unordered-list", variables: vars)
end method;


define method dita-content (defn-list :: <defn-list>, target-info)
=> (dita :: <string>)
   // BUGFIX: Map can't create an <array>. Bug #7473.
   let dita-items = make(<array>, dimensions: defn-list.items.dimensions);
   map-into(dita-items, rcurry(dita-content, target-info), defn-list.items);
   let templ = if (instance?(defn-list, <parm-list>))
                  #"dita-parm-list"
               else
                  #"dita-defn-list"
               end if;
   let vars = table(<case-insensitive-string-table>,
         "class" =>
               if (instance?(defn-list, <one-line-defn-list>))
                  "one-line"
               else
                  "many-line"
               end if,
         "items" =>
               range(from: 0, below: dimension(dita-items, 0))
         );
   let ops = table(<case-insensitive-string-table>,
         "term" =>
               method (i :: <integer>) => (dita :: <string>)
                  dita-items[i, 0]
               end,
         "defn" =>
               method (i :: <integer>) => (dita :: <string>)
                  dita-items[i, 1]
               end
         );
   text-from-template(templ, variables: vars, operations: ops)
end method;


define method dita-content (ent :: <entity>, target-info)
=> (dita :: <string>)
   format-to-string("&#%d;", ent.code)
end method;


define method dita-content (parm :: <api/parm-name>, target-info)
=> (dita :: <string>)
   // TODO: Can make this a DITA <parmname> tag when appropriate automatically?
   dita-entag("apiname", parm.text, target-info)
end method;


define method dita-content (term :: <term>, target-info)
=> (dita :: <string>)
   dita-entag("term", term.text, target-info)
end method;


define method dita-content (term :: <term-style>, target-info)
=> (dita :: <string>)
   dita-content(term.text, target-info)
end method;


define method dita-content (code :: <code-phrase>, target-info)
=> (dita :: <string>)
   dita-entag("codeph", code.text, target-info)
end method;


define method dita-content (cite :: <cite>, target-info)
=> (dita :: <string>)
   dita-entag("cite", cite.text, target-info)
end method;


define method dita-content (bold :: <bold>, target-info)
=> (dita :: <string>)
   dita-entag("b", bold.text, target-info)
end method;


define method dita-content (ital :: <italic>, target-info)
=> (dita :: <string>)
   dita-entag("i", ital.text, target-info)
end method;


define method dita-content (und :: <underline>, target-info)
=> (dita :: <string>)
   dita-entag("u", und.text, target-info)
end method;


define method dita-content (em :: <emphasis>, target-info)
=> (dita :: <string>)
   dita-entag("b", em.text, target-info)
end method;


define method dita-content (para :: <paragraph>, target-info)
=> (dita :: <string>)
   concatenate(dita-entag("p", para.content, target-info), "\n");
end method;


define method dita-entag (tag :: <string>, content, target-info)
=> (dita :: <string>)
   concatenate("<", tag, ">",
         dita-content(content, target-info),
         "</", tag, ">")
end method;
