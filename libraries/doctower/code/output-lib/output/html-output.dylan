module: output


define constant $html-templates = #[
   #"html-redirect",
   #"html-toc",
   #"html-toc-recursion",
   #"html-topic",
   #"html-section",
   #"html-unordered-list",
   #"html-defn-list"
];


define method output-templates (output == #"html") => (templates :: <sequence>)
   $html-templates
end method;


define method output-file-info
   (output == #"html", doc-tree :: <ordered-tree>)
=> (topic-files :: <table>, special-files :: <table>, file-info :: <sequence>)
   // Doc-tree includes #f as root, so effective size is one less and its first
   // element should be ignored.
   
   let file-info = make(<stretchy-vector>);
   let topic-count = max(0, doc-tree.size - 1);
   let topic-files = make(<table>, size: topic-count);
   let special-files = make(<table>, size: 4);
   let html-dir = as(<directory-locator>, "html");

   // Topic files
   
   let topic-digits = topic-count.digits;
   let topic-idx = 0;
   for (topic in doc-tree)
      unless (~topic)
         let prefix-part = format-to-string("%0*d", topic-digits, topic-idx + 1);
         let title-part = as-filename-part(topic.title.stringify-title);
         let base-name = format-to-string("%s_%s", prefix-part, title-part);
         let locator = make(<file-locator>,
               directory: html-dir, base: base-name, extension: "html");
         let topic-file = make(<topic-output-file>, file: locator, topic: topic);
         topic-files[topic] := topic-file;
         file-info := add!(file-info, topic-file);
         topic-idx := topic-idx + 1;
      end unless;
   end for;
   
   // TOC file

   let toc-locator = make(<file-locator>,
         directory: html-dir, base: "contents", extension: "html");
   let toc-file = make(<toc-output-file>, tree: doc-tree, file: toc-locator);
   special-files[#"toc"] := toc-file;
   file-info := add!(file-info, toc-file);

   // Index file

   let top-level-keys = doc-tree.root-key.inf-key-sequence;
   let index-dest =
         if (top-level-keys.size > 0)
            let home-topic = doc-tree[top-level-keys.first];
            topic-files[home-topic]
         else
            toc-file
         end if;
   let index-locator = make(<file-locator>, base: "index", extension: "html");
   let index-file = make(<redirect-output-file>, dest: index-dest,
         file: index-locator);
   special-files[#"home"] := index-dest;
   file-info := add!(file-info, index-file);
   
   // CSS file
   
   let css-locator = make(<file-locator>,
         directory: html-dir, base: "stylesheet", extension: "css");
   let css-origin = make(<file-locator>,
         directory: *template-directory*, base: "default-stylesheet", extension: "css");
   let css-file = make(<copied-output-file>, origin: css-origin, file: css-locator);
   special-files[#"css"] := css-file;
   file-info := add!(file-info, css-file);
   
   // General index file
   
   let gen-index-locator = make(<file-locator>,
         directory: html-dir, base: "general-index", extension: "html");
   let gen-index-file = make(<index-output-file>, file: gen-index-locator);
   special-files[#"index"] := gen-index-file;
   file-info := add!(file-info, gen-index-file);
   
   values(topic-files, special-files, file-info)
end method;


define method target-link-info
   (output == #"html", doc-tree :: <ordered-tree>,
    fallback-ids :: <table>, file-info :: <table>)
=> (target-info :: <table>)
   let target-info = make(<table>);
   for (topic in doc-tree)
      unless (~topic) // Root of doc-tree is #f
         let filename = as(<string>, file-info[topic].locator);
         visit-targets(topic, add-html-link-info, target-info: target-info,
               current-topic: topic, fallback-ids: fallback-ids, filename: filename)
      end unless
   end for;
   target-info
end method;


define method add-html-link-info
   (object :: <object>,
    #key setter, target-info, current-topic, fallback-ids, filename)
=> (visit-slots? :: <boolean>)
   #t
end method;


define method add-html-link-info
   (topic :: <topic>,
    #key setter, target-info, current-topic, fallback-ids, filename)
=> (visit-slots? :: <boolean>)
   let target-id = topic.id | fallback-ids[topic];
   let target-href = format-to-string("%s#%s", filename, target-id);
   let title-href = format-to-string("%s#:Title", filename);
   target-info[topic] := make(<topic-target>,
         id: target-id, href: target-href,
         title-id: ":Title", title-href: title-href);
   #t
end method;


define method add-html-link-info
   (sect :: <section>,
    #key setter, target-info, current-topic, fallback-ids, filename)
=> (visit-slots? :: <boolean>)
   let topic-id = current-topic.id | fallback-ids[current-topic];
   let section-id = sect.id | fallback-ids[sect];
   let target-id = format-to-string("%s/%s", topic-id, section-id);
   let title-id = format-to-string(":Title(%s)", target-id);
   let target-href = format-to-string("%s#%s", filename, target-id);
   let title-href = format-to-string("%s#%s", filename, target-id);
   target-info[sect] := make(<topic-target>,
         id: target-id, href: target-href,
         title-id: title-id, title-href: title-href);
   #t
end method;


define method add-html-link-info
   (content :: type-union(<footnote>, <ph-marker>),
    #key setter, target-info, current-topic, fallback-ids, filename)
=> (visit-slots? :: <boolean>)
   let topic-id = current-topic.id | fallback-ids[current-topic];
   let target-id = format-to-string("%s/%s", topic-id, fallback-ids[content]);
   let target-href = format-to-string("%s#%s", filename, target-id);
   let info-class =
         select (content by instance?)
            <footnote> => <footnote-target>;
            <ph-marker> => <ph-marker-target>;
         end select;
   target-info[content] := make(info-class, id: target-id, href: target-href);
   #t
end method;


define method write-output-file
   (output == #"html", file-info :: <redirect-output-file>,
    link-map :: <table>, target-info :: <table>, special-file-info :: <table>)
=> ()
   let var-table = table(<case-insensitive-string-table>,
         "destination" => file-info.destination.locator);
   output-from-template(#"html-redirect", file-info, variables: var-table);
end method;


define method write-output-file
   (output == #"html", file-info :: <toc-output-file>,
    link-map :: <table>, target-info :: <table>, special-file-info :: <table>)
=> ()
   let var-table = table(<case-insensitive-string-table>,
         "css-file" => special-file-info[#"css"].locator,
         "home-file" => special-file-info[#"home"].locator,
         "index-file" => special-file-info[#"index"].locator,
         "package-title" => *package-title*,
         "root-topics" => file-info.tree.root-key.inf-key-sequence
         );

   let ops-table = table(<case-insensitive-string-table>,
         "size" => size,
         "href" =>
               method (key :: <ordered-tree-key>) => (href :: <string>)
                  let topic = file-info.tree[key];
                  target-info[topic].target-href
               end method,
         "id" =>
               method (key :: <ordered-tree-key>) => (id :: <string>)
                  let topic = file-info.tree[key];
                  target-info[topic].target-id
               end method,
         "shortdesc" =>
               method (key :: <ordered-tree-key>) => (desc :: <string>)
                  let topic = file-info.tree[key];
                  if (topic.shortdesc)
                     topic.shortdesc.content.stringify-markup
                  else
                     ""
                  end if
               end method,
         "formatted-title" =>
               method (key :: <ordered-tree-key>) => (title :: <string>)
                  let topic = file-info.tree[key];
                  html-content(topic.title, target-info)
               end method,
         "child-recursion" =>
               identity /* this is replaced with local method below */
         );

   local method do-child-recursion (key :: <ordered-tree-key>) => (text :: <string>)
            let child-keys = key.inf-key-sequence;
            let vars = table(<case-insensitive-string-table>,
                  "children" => child-keys);
            text-from-template(#"html-toc-recursion",
                  variables: vars, operations: ops-table)
         end method;
   
   ops-table["child-recursion"] := do-child-recursion;
   output-from-template(#"html-toc", file-info,
         variables: var-table, operations: ops-table);
end method;


define method write-output-file
   (output == #"html", file-info :: <index-output-file>,
    link-map :: <table>, target-info :: <table>, special-file-info :: <table>)
=> ()
   // TODO: General index
end method;


define method write-output-file
   (output == #"html", file-info :: <topic-output-file>,
    link-map :: <table>, target-info :: <table>, special-file-info :: <table>)
=> ()
   let var-table = table(<case-insensitive-string-table>,
         "css-file" => special-file-info[#"css"].locator,
         "home-file" => special-file-info[#"home"].locator,
         "toc-file" => special-file-info[#"toc"].locator,
         "index-file" => special-file-info[#"index"].locator,
         "topic-file" => file-info.locator,
         "package-title" => *package-title*,
         "topic" => file-info.topic
         );
   
   let ops-table = table(<case-insensitive-string-table>,
         "href" =>
               method (topic :: type-union(<topic>, <url>)) => (href :: <string>)
                  if (instance?(topic, <topic>))
                     target-info[topic].target-href
                  else
                     topic
                  end if
               end method,
         "id" =>
               method (topic :: <topic>) => (id :: <string>)
                  target-info[topic].target-id
               end method,
         "shortdesc" =>
               method (topic :: type-union(<topic>, <url>)) => (desc :: <string>)
                  if (instance?(topic, <topic>) & topic.shortdesc)
                     topic.shortdesc.content.stringify-markup
                  else
                     ""
                  end if
               end method,
         "title" =>
               method (topic :: type-union(<topic>, <url>)) => (title :: <string>)
                  if (instance?(topic, <topic>))
                     topic.title.stringify-title
                  else
                     topic
                  end if
               end method,
         "prev-topic" =>
               method (topic :: <topic>) => (prev :: false-or(<topic>))
                  link-map[topic].prev-topic
               end method,
         "next-topic" =>
               method (topic :: <topic>) => (next :: false-or(<topic>))
                  link-map[topic].next-topic
               end method,
         "parent-topics" =>
               method (topic :: <topic>) => (parents :: <sequence>)
                  link-map[topic].parent-topics
               end method,
         "formatted-title" =>
               method (topic :: <topic>) => (html :: <string>)
                  html-content(topic.title, target-info)
               end method,
         "formatted-shortdesc" =>
               method (topic :: <topic>) => (html :: <string>)
                  html-content(topic.shortdesc | "", target-info)
               end method,
         "formatted-content" =>
               method (topic :: <topic>) => (html :: <string>)
                  html-content(topic.content, target-info)
               end method,
         "definitions-section" =>
               rcurry(html-section, definitions-section, target-info),
         "syntax-section" =>
               rcurry(html-section, syntax-section, target-info),
         "adjectives-section" =>
               rcurry(html-section, adjectives-section, target-info),
         "conds-section" =>
               rcurry(html-section, conds-section, target-info),
         "args-section" =>
               rcurry(html-section, args-section, target-info),
         "vals-section" =>
               rcurry(html-section, vals-section, target-info),
         "keywords-section" =>
               rcurry(html-section, keywords-section, target-info),
         "value-section" =>
               rcurry(html-section, value-section, target-info),
         "inheritables-section" =>
               rcurry(html-section, inheritables-section, target-info),
         "supers-section" =>
               rcurry(html-section, supers-section, target-info),
         "subs-section" =>
               rcurry(html-section, subs-section, target-info),
         "funcs-on-section" =>
               rcurry(html-section, funcs-on-section, target-info),
         "funcs-returning-section" =>
               rcurry(html-section, funcs-returning-section, target-info),
         "methods-section" =>
               rcurry(html-section, methods-section, target-info),
         "modules-section" =>
               rcurry(html-section, modules-section, target-info),
         "bindings-section" =>
               rcurry(html-section, bindings-section, target-info),
         "subtopics" =>
               method (topic :: <topic>) => (subtopics :: <sequence>)
                  link-map[topic].child-topics
               end method,
         "related-links" =>
               method (topic :: <topic>) => (links :: <sequence>)
                  map(target, topic.related-links)
               end method,
         "footnotes" => footnotes,
         "size" => size
         );

   output-from-template(#"html-topic", file-info,
         variables: var-table, operations: ops-table);
end method;


define method html-section
   (topic :: <topic>, accessor :: <function>, target-info)
=> (html :: <string>)
   if (applicable-method?(accessor, topic))
      let sect :: false-or(<section>) = topic.accessor;
      if (sect & ~sect.content.empty?)
         let vars = table(<case-insensitive-string-table>,
               "id" =>
                     target-info[sect].target-id,
               "formatted-title" =>
                     html-content(sect.title, target-info),
               "content" =>
                     html-content(sect.content, target-info)
               );
         text-from-template(#"html-section", variables: vars);
      else
         ""
      end if
   else
      ""
   end if
end method;


define method html-content (obj :: <object>, target-info)
=> (html :: <string>)
   /**/
   format-to-string("%=", obj).xml-sanitizer;
end method;


define method html-content (seq :: <sequence>, target-info)
=> (html :: <string>)
   let html-elems = map-as(<vector>, rcurry(html-content, target-info), seq);
   reduce(concatenate, "", html-elems)
end method;


define method html-content (str :: <string>, target-info)
=> (html :: <string>)
   xml-sanitizer(str)
end method;


define method html-content (char :: <character>, target-info)
=> (html :: <string>)
   xml-sanitizer(as(<string>, char))
end method;


define method html-content (xref :: <xref>, target-info)
=> (html :: <string>)
   let title = html-content(xref.text, target-info);
   select (xref.target by instance?)
      <url> =>
         let href = as(<string>, xref.target).xml-sanitizer;
         format-to-string("<a href=\"%s\">%s</a>", href, title);
      <topic> =>
         let href = target-info[xref.target].target-href.xml-sanitizer;
         let desc =
               if (xref.target.shortdesc)
                  xref.target.shortdesc.content.stringify-markup.xml-sanitizer
               else
                  ""
               end if;
         format-to-string("<a href=\"../%s\" title=\"%s\">%s</a>", href, desc, title);
      <section> =>
         let href = target-info[xref.target].target-href.xml-sanitizer;
         format-to-string("<a href=\"../%s\">%s</a>", href, title);
      otherwise =>
         // TODO: Xref output for footnotes and ph-markers.
         next-method();
   end select
end method;


define method html-content (conref :: <conref>, target-info)
=> (html :: <string>)
   if (conref.style = #"title")
      html-content(conref.target.title, target-info)
   else
      html-content(conref.target.shortdesc, target-info)
   end if;
end method;


define method html-content (ul :: <unordered-list>, target-info)
=> (html :: <string>)
   let html-items = map(rcurry(html-content, target-info), ul.items);
   let vars = table(<case-insensitive-string-table>, "items" => html-items);
   text-from-template(#"html-unordered-list", variables: vars)
end method;


define method html-content (defn-list :: <defn-list>, target-info)
=> (html :: <string>)
   // BUGFIX: Map can't create an <array>. Bug #7473.
   let html-items = make(<array>, dimensions: defn-list.items.dimensions);
   map-into(html-items, rcurry(html-content, target-info), defn-list.items);
   let vars = table(<case-insensitive-string-table>,
         "class" =>
               if (instance?(defn-list, <one-line-defn-list>))
                  "one-line"
               else
                  "many-line"
               end if,
         "items" =>
               range(from: 0, below: dimension(html-items, 0))
         );
   let ops = table(<case-insensitive-string-table>,
         "term" =>
               method (i :: <integer>) => (html :: <string>)
                  html-items[i, 0]
               end,
         "defn" =>
               method (i :: <integer>) => (html :: <string>)
                  html-items[i, 1]
               end
         );
   text-from-template(#"html-defn-list", variables: vars, operations: ops)
end method;


define method html-content (ent :: <entity>, target-info)
=> (html :: <string>)
   format-to-string("&#%d;", ent.code)
end method;


define method html-content (parm :: <api/parm-name>, target-info)
=> (html :: <string>)
   entag("var", parm.text, target-info)
end method;


define method html-content (term :: <term>, target-info)
=> (html :: <string>)
   entag("dfn", term.text, target-info)
end method;


define method html-content (term :: <term-style>, target-info)
=> (html :: <string>)
   enspan("term", term.text, target-info)
end method;


define method html-content (code :: <code-phrase>, target-info)
=> (html :: <string>)
   entag("code", code.text, target-info)
end method;


define method html-content (cite :: <cite>, target-info)
=> (html :: <string>)
   entag("cite", cite.text, target-info)
end method;


define method html-content (bold :: <bold>, target-info)
=> (html :: <string>)
   entag("b", bold.text, target-info)
end method;


define method html-content (ital :: <italic>, target-info)
=> (html :: <string>)
   entag("i", ital.text, target-info)
end method;


define method html-content (und :: <underline>, target-info)
=> (html :: <string>)
   entag("u", und.text, target-info)
end method;


define method html-content (em :: <emphasis>, target-info)
=> (html :: <string>)
   entag("em", em.text, target-info)
end method;


define method html-content (para :: <paragraph>, target-info)
=> (html :: <string>)
   entag("p", para.content, target-info)
end method;


define method enspan (span-class :: <string>, content, target-info)
=> (html :: <string>)
   concatenate("<span class=\"", span-class.xml-sanitizer, "\"",
         html-content(content, target-info),
         "</span>")
end method;


define method entag (tag :: <string>, content, target-info)
=> (html :: <string>)
   concatenate("<", tag, ">",
         html-content(content, target-info),
         "</", tag, ">")
end method;
