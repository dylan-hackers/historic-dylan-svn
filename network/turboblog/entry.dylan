module: turboblog
author: turbo24prg

define object-tests (entry) in turboblog end;

define class <blog-entry> (<entry>)
  slot entry-blog :: <blog>,
    init-keyword: entry-blog:;
  slot pingbacks :: <table> = make(<table>),
    init-keyword: pingbacks:;
  slot %pingbacks-count :: <integer> = 0;
end;

define action-tests
 (add-entry, edit-entry, remove-entry, list-entries)
in turboblog end;

define method pingbacks-count (entry :: <blog-entry>)
 => (res :: <integer>);
  entry.%pingbacks-count := entry.%pingbacks-count + 1;
  entry.%pingbacks-count;
end;

define class <entry-exists-error> (<error>)
  slot entry :: <blog-entry>,
    required-init-keyword: entry:;
  slot existing-entry :: <blog-entry>,
    required-init-keyword: existing-entry:;
end;

define method add-entry (blog :: <blog>, entry :: <blog-entry>)
 => (entry :: <blog-entry>);
  let existing-entry = find-entry(blog, entry.title, entry.published);
  if (existing-entry)
    signal(make(<entry-exists-error>, entry: entry, existing-entry: existing-entry));
  else
    entry.entry-blog := blog;
  
    blog.entries[id(entry.title, entry.published)] := entry;
    
    blog.updated := entry.published;
    
    for (category in entry.categories)
      blog.categories := add-new!(blog.categories, category);
    end for;

    entry.links := add!(entry.links, make(<blog-link>,
      href: build-uri(permanent-link(entry, escaped?: #t))));
    entry.links := add!(entry.links, make(<blog-link>,
      href: build-uri(permanent-link(entry, escaped?: #t)), rel: "edit"));

    let (year, month, day) = decode-date(entry.published);

    unless (element(blog.archive, year, default: #f))
      blog.archive[year] := 
        list(make(<table>), make(<vector>));
    end unless;
    unless (element(blog.archive[year][0], month, default: #f))
      blog.archive[year][0][month] := 
        list(make(<table>), make(<vector>));
    end unless;
    unless (element(blog.archive[year][0][month][0], day, default: #f))
      blog.archive[year][0][month][0][day] := make(<vector>);
    end unless;

    blog.archive[year][0][month][0][day] :=
      add!(blog.archive[year][0][month][0][day], entry);
    blog.archive[year][0][month][1] := 
      add!(blog.archive[year][0][month][1], entry);
    blog.archive[year][1] :=
      add!(blog.archive[year][1], entry);

    register-responder(entry);
  end if;
  entry;
end;

define method remove-entry (blog :: <blog>, entry :: <blog-entry>)
 => (entry :: <blog-entry>);
  let (year, month, day) = decode-date(entry.published);

  blog.archive[year][0][month][0][day] :=
    remove!(blog.archive[year][0][month][0][day], entry);
  blog.archive[year][0][month][1] :=
    remove!(blog.archive[year][0][month][1], entry);
  blog.archive[year][1] :=
    remove!(blog.archive[year][1], entry);

  remove-key!(blog.entries, id(entry.title, entry.published));
  remove-responder(entry);
  entry;
end;

define method find-entry (blog :: <blog>, title :: <string>, date :: <date>)
 => (entry :: false-or(<blog-entry>));
  element(blog.entries, id(title, date), default: #f)
end;

define method modify-entry (entry :: <blog-entry>, #key
 title: entry-title :: false-or(<string>), content: entry-content :: false-or(<content>),
 tags: entry-tags :: false-or(<vector>))
 => (entry :: <blog-entry>);

  if (entry-title)
    remove-key!(entry.entry-blog.entries, id(entry.title, entry.published));
    entry.title := entry-title;
    entry.entry-blog.entries[id(entry.title, entry.published)] := entry;
  end if;

  entry-content & (entry.content := entry-content);
  entry-tags & (entry.categories := entry-tags);
//  entry.identifier := permanent-link(entry);
  entry.updated := current-date();
  entry;
end;

define method add-author (entry :: <blog-entry>, participant :: <blog-participant>)
 => (participant :: <blog-participant>);
  entry.authors := add-new!(entry.authors, participant, test: method (first, second)
      first.participant-user.username = second.participant-user.username
    end);
  participant;
end;

define method register-responder (entry :: <blog-entry>)
  register-url(apply(join, "/", permanent-link(entry).uri-path),
    curry(entry-responder, entry), replace?: #t, prefix?: #t);
  register-url(concatenate(apply(join, "/", permanent-link(entry).uri-path), "/comments"),
    curry(comments-responder, entry), replace?: #t, prefix?: #t);
end;

define method remove-responder (entry :: <blog-entry>)
  remove-responder(apply(join, "/", permanent-link(entry).uri-path));
  remove-responder(concatenate(apply(join, "/", permanent-link(entry).uri-path), "/comments"));
end;

define variable *edit-entry-page* =
  make(<turboblog-page>, source: "edit-entry.dsp");

define variable *view-entry-page* =
  make(<themed-turboblog-page>,
       source: "themes/default/view-entry.dsp");

define variable *view-entries-page* =
  make(<themed-turboblog-page>,
       source: "themes/default/view-entries.dsp");

define variable *remove-entry-page* =
  make(<turboblog-page>, source: "remove-entry.dsp");


define named-method last-entry-tag? in turboblog
 (page :: <turboblog-page>)
  if (*entry* & *tag*)
    *tag* = last(*entry*.categories);
  end if;
end;

define named-method last-entry-author? in turboblog
 (page :: <turboblog-page>)
  if (*entry* & *participant*)
    *participant* = last(*entry*.authors);
  end if;
end;

define named-method entry-content-type? in turboblog
 (page :: <turboblog-page>)
  if (*entry* & *content-type*)
    *entry*.content.type = head(*content-type*);
  end if;
end;

define tag show-entry-title in turboblog
 (page :: <turboblog-page>)
 ()
  if (*entry*)
    output("%s", escape-xml(*entry*.title));
  elseif (*form* & element(*form*, "title", default: #f))
    output("%s", escape-xml(*form*["title"]));
  end if;
end;

define tag show-entry-body in turboblog (page :: <turboblog-page>)
 (output-format :: false-or(<string>), escape :: <boolean>)
  local method prepare (body :: <string>)
//      if (output-format) body := transform-content(output-format, body) end if;
      if (escape) body := escape-xml(body) end if;
      body;
    end;
  if (*entry* & *entry*.content)
    output("%s", prepare(*entry*.content.content));
  elseif (*form* & element(*form*, "body", default: #f))
    output("%s", prepare(*form*["body"]));
  end if;
end;

define tag show-entry-comment-count in turboblog (page :: <turboblog-page>)
 ()  
  if (*entry*)
    output("%s", size(*entry*.comments));
  end if;
end;

define tag show-entry-update in turboblog (page :: <turboblog-page>)
 (formatted :: <string>)
  if (*entry*)
    output("%s", format-date(formatted, *entry*.updated | *entry*.published));
  end if;
end;

define tag show-entry-published in turboblog (page :: <turboblog-page>)
 (formatted :: <string>)
  if (*entry*)
    output("%s", format-date(formatted, *entry*.published));
  end if;
end;

define tag show-entry-permanent-link in turboblog (page :: <turboblog-page>)
 ()
  if (*entry*)
    output("%s", permanent-link(*entry*));
  end if;
end;

define body tag list-entry-comments in turboblog (page :: <turboblog-page>, do-body :: <function>)
 (first-comment :: false-or(<string>), last-comment :: false-or(<string>))
  if (first-comment) first-comment := string-to-integer(first-comment) end if;
  if (last-comment) last-comment := string-to-integer(last-comment) end if;

  if (*entry*)
    for (index from (first-comment | 1) to (last-comment | size(*entry*.comments)),
          comment in sort-table(*entry*.comments, published))
      dynamic-bind(*comment* = comment)
        do-body();
      end;
    end for;
  end if;
end;

define body tag list-entry-pingbacks in turboblog (page :: <turboblog-page>, do-body :: <function>)
 (first-pingback :: false-or(<string>), last-pingback :: false-or(<string>))
  if (first-pingback) first-pingback := string-to-integer(first-pingback) end if;
  if (last-pingback) last-pingback := string-to-integer(last-pingback) end if;

  if (*entry*)
    for (index from (first-pingback | 1) to (last-pingback | size(*entry*.pingbacks)),
          pingback in sort-table(*entry*.pingbacks, published))
      dynamic-bind(*pingback* = pingback)
        do-body();
      end;
    end for;
  end if;
end;

define body tag list-entry-tags in turboblog (page :: <turboblog-page>, do-body :: <function>) 
 () 
  if (*entry*) 
    for (tag in *entry*.categories) 
      dynamic-bind(*tag* = tag) 
        do-body(); 
      end; 
    end for; 
  elseif (*form* & element(*form*, "tags", default: #f)) 
    output("%s", escape-xml(*form*["tags"])); 
  end if; 
end; 
 
define body tag list-entry-authors in turboblog (page :: <turboblog-page>, do-body :: <function>) 
 () 
  if (*entry*) 
    for (author in *entry*.authors) 
      dynamic-bind(*participant* = author) 
        do-body(); 
      end; 
    end for; 
  end if; 
end;

// TODO: respond-to(#"post" is stupid:
// handles post and put post request => should be renamed
define method respond-to (type == #"post", page :: <edit-entry-page>)
  let request = current-request();
  let response = current-response();

// action
  let edit? :: <boolean> = (*action* = #"edit-entry");
  let add? :: <boolean> = (*action* = #"add-entry");

  let atom? = (request-content-type(request) = #"application/atom+xml");
  let atom-entry = if (atom?)
      let document = parse-document(request-content(request));
      document & root(document);
    end if;

  local method return (entry :: <blog-entry>)
      if (atom?)
        if (add?)
          response.response-code := 201;
          response.response-message := "Created";
          add-header(response.response-headers, "Location", build-uri(permanent-link(entry)));
        end if;
        format(output-stream(response), "%s", add-atom-namespace(generate-atom(entry)));
      else
        redirect-to(entry);
      end if;
    end;

  let current-title = if (*entry*) *entry*.title end if;

  let title = #f;
  let body = #f;
  let tags = #f;
  let author = #f;
  let published = #f;
  let type = #f;

  if (atom-entry)
    title := begin
        let title = elements(atom-entry, #"title");
        ~empty?(title) & first(title).text;
      end;
    let content = elements(atom-entry, #"content");
    if (~empty?(content))
      type := attribute(first(content), "type");
      if (type)
        type := type.attribute-value;
        if (type = "xhtml")
          let divs = elements(first(content), #"xhtml:div");
          body := ~empty?(divs) & as(<string>, first(divs));
        else
          body := first(content).text;
        end if;
      end if;
    end if;
    author := begin 
        let author = elements(atom-entry, #"author");
        if (~empty?(author))
          let name = elements(first(author), #"name");
          ~empty?(name) & first(name).text;
        end if;
      end;
    published := begin
        let updated = elements(atom-entry, #"updated");
        ~empty?(updated) & first(updated).text;
      end;
  else
    title := get-query-value("title");
    body := get-query-value("body");
    tags := get-query-value("tags");
    type := get-query-value("type");
  end if;

// errors
  let errors = #();

// checks
  if (~ instance?(title, <string>) | title = "")
    errors := add!(errors, #"title");
  else
    if (title ~= current-title & find-entry(*blog*, title, current-date())) //??? 
      errors := add!(errors, #"entry-exists-already");
    end if;
    title := if (title ~= current-title) title end if;
  end if;

  if (~ (type = "xhtml" | type = "markup"))
    errors := add!(errors, #"type");
  end if;

  if (~ instance?(body, <string>) | body = "")
    errors := add!(errors, #"body");
  end if;

  if (atom? & ~authenticated-user().administrator? & author ~= authenticated-user().username)
    errors := add!(errors, #"author");
  end if;

  tags := if (instance?(tags, <string>))
           extract-tags(tags)  
  	else #[] end if;
 
  published := begin 
      let date = published & parse-date(published, "%Y-%m-%dT%H:%M:%S%:z");
      date | current-date();
    end;

  case
    ~empty?(errors) & atom? =>
      //TODO: use format-to-string and explain missing/wrong things better
      bad-request(message: format-to-string("errors: %=", errors));
    ~empty?(errors) =>
      dynamic-bind (*errors* = errors, *form* = request.request-query-values)
        next-method();
      end;
    edit? => begin
        modify-entry(*entry*, title: title,
          content: make(select (type by \=)
              "xhtml" => <xhtml-content>;
              "markup" => <markup-content>;
            end select, content: body),
          tags: tags);
        let participant = find-participant(*blog*, if (atom?) author
          else authenticated-user().username end if);
        add-author(*entry*, participant |
          make(<blog-participant>, participant-user: authenticated-user(),
            participant-role: #"administrator"));
        return(*entry*);
      end;
    add? => begin
        let entry = make(<blog-entry>, published: published, title: title,
          content: make(select (type by \=)
              "xhtml" => <xhtml-content>;
              "markup" => <markup-content>;
            end select, content: body),
          categories: tags);
        add-entry(*blog*, entry);
        let participant = find-participant(*blog*, if (atom?) author
          else authenticated-user().username end if);
        add-author(entry, participant |
          make(<blog-participant>, participant-user: authenticated-user(),
            participant-role: #"administrator"));
        do-pingbacks(entry);
        return(entry);
      end;
  end case;
end;

define method entry-responder (entry :: <blog-entry>)
  let request = current-request();
  authenticate();

  let blog = entry.entry-blog;
  let atom? :: <boolean> = (get-query-value("atom") = #t);
  let edit? :: <boolean> = (get-query-value("edit") = #t);
  let remove? :: <boolean> = (get-query-value("remove") = #t);

  let action :: <symbol> = case
      edit? | request.request-method = #"put" => #"edit-entry";
      remove? | request.request-method = #"delete" => #"remove-entry";
      atom? => #"view-atom-entry";
      otherwise => #"view-entry";
    end case;

  with-permission(action, blog: blog, entry: entry)
    dynamic-bind(*action* = action, *blog* = blog, *entry* = entry)
      select (request.request-method)
        #"get" => case
            atom? => output("%s", add-atom-namespace(generate-atom(entry)));
            edit? => respond-to(#"get", *edit-entry-page*);
            remove? => respond-to(#"get", *remove-entry-page*);
            otherwise => respond-to(#"get", *view-entry-page*);
          end case;
        #"post" => case
            remove? => begin
                remove-entry(blog, entry);
                redirect-to(blog);
              end;
            otherwise => respond-to(#"post", *edit-entry-page*);
          end case;
        #"put" => respond-to(#"post", *edit-entry-page*);
        #"delete" => remove-entry(blog, entry);
      end select;
    end;
  end;
end;
