module: turboblog
author: turbo24prg

define object-tests (blog) in turboblog end;

define class <blog> (<feed>)
  slot archive :: <table> = make(<table>),
    init-keyword: archive:;
  slot participants :: <string-table> = make(<string-table>),
    init-keyword: participants:;
  slot theme :: <string> = "default",
    init-keyword: theme:;
  slot list-directories? :: <boolean> = #f,
    init-keyword: list-directories?:;
  slot directories :: <table> = make(<table>),
    init-keyword: directories:;
  slot show-entry-authors? :: <boolean>,
    init-function: show-entry-authors?-default,
    init-keyword: show-entry-authors?:;
end;

define function show-entry-authors?-default () #t end;

define class <blog-feed> (<feed>)
  slot feed-blog :: <blog>,
    required-init-keyword: blog:;
end;

define class <blog-exists-error> (<error>)
  slot blog :: <blog>,
    required-init-keyword: blog:;
  slot existing-blog :: <blog>,
    required-init-keyword: existing-blog:;
end;

define action-tests
 (view-blog, add-blog, remove-blog, 
  configure-blog, list-blogs)
in turboblog end;

define method add-blog (blog-server :: <blog-server>, blog :: <blog>)
 => (blog :: <blog>);
  let existing-blog = find-blog(blog-server, blog.title);
  if (existing-blog)
    signal(make(<blog-exists-error>, blog: blog, existing-blog: existing-blog));
  else
    blog-server.blogs[blog.title] := blog;
    register-responder(blog);
    
    blog.directories[#"root"] := merge-locators(as(<file-system-locator>, 
      concatenate(blog.title, "/")), document-root(*virtual-host*));
    ensure-directories-exist(blog.directories[#"root"]);

    blog.directories[#"files"] := merge-locators(as(<file-system-locator>,
      concatenate("files/")),  blog.directories[#"root"]);
    ensure-directories-exist(blog.directories[#"files"]);
  end if;
  blog;
end;

define method remove-blog (blog-server :: <blog-server>, blog :: <blog>) 
 => (blog :: <blog>); 
  local method depth-test (first, second)
    size(first.locator-path) > size(second.locator-path);
  end;
  remove-key!(blog-server.blogs, blog.title);
  remove-responder(blog);
  let directories = map-as(<vector>, method (directory) directory end, blog.directories);
  for (directory in sort(directories, test: depth-test))
    do-directory(method (directory :: <pathname>, name :: <string>, type :: <file-type>)
        if (first(name) ~= '.' & type = #"file")
          delete-file(merge-locators(as(<file-system-locator>, name),
            as(<directory-locator>, directory)));
        end if;
      end, directory);
    delete-directory(directory);
  end for;
  blog;
end;

define method find-blog (blog-server :: <blog-server>, blog-name :: <string>)
 => (blog :: false-or(<blog>));
  element(blog-server.blogs, blog-name, default: #f)
end;

define method modify-blog (blog :: <blog>, #key 
 title: blog-title :: false-or(<string>), subtitle: blog-subtitle :: false-or(<string>),
 description: blog-description :: false-or(<string>),
 languages: blog-languages :: false-or(<list>),
 show-entry-authors?: blog-show-entry-authors? :: <boolean>,
 theme: blog-theme :: <string>)
 => (blog :: <blog>);
  if (blog-title)
    remove-blog(*blog-server*, blog);
    blog.title := blog-title;
    add-blog(*blog-server*, blog); 
  end if;
  blog-subtitle & (blog.subtitle := blog-subtitle);
  blog-description & (blog.description := blog-description);
  blog-languages & (blog.languages := blog-languages);
  blog.show-entry-authors? := blog-show-entry-authors?;
  blog-theme & (blog.theme := blog-theme);
  blog;
end;

define constant $blog-responder = 
  list(pair("archive", archive-responder), pair("links", links-responder), 
      pair("files", files-responder), pair("participants", participants-responder));

define method register-responder (blog :: <blog>)
  register-url(apply(join, "/", permanent-link(blog).uri-path),
    curry(blog-responder, blog), replace?: #t, prefix?: #t);
  for (responder-pair in $blog-responder)
    register-url(concatenate(apply(join, "/", permanent-link(blog).uri-path), "/", head(responder-pair)),
      curry(tail(responder-pair), blog), replace?: #t, prefix?: #t);
  end for;
  for (entry in blog.entries)
    register-responder(entry);
  end for;
end;

define method remove-responder (blog :: <blog>)
  remove-responder(apply(join, "/", permanent-link(blog).uri-path));
  for (responder-pair in $blog-responder)
    remove-responder(concatenate(apply(join, "/", permanent-link(blog).uri-path), "/", head(responder-pair)));
  end for;               
  for (entry in blog.entries)
    remove-responder(entry);
  end for;  
end;

define method archive-entries (blog :: <blog>, #key 
 year :: false-or(<integer>), month :: false-or(<integer>),
 day :: false-or(<integer>), tags :: false-or(<sequence>),
 search :: false-or(<string>))
 => (entries :: <sequence>);
  let archive-entries = case 
      (day & ~ month) |
      (month & ~ year) |
      (year & ~ element(blog.archive, year, default: #f)) |
      (month & ~ element(blog.archive[year][0], month, default: #f)) |
      (day & ~ element(blog.archive[year][0][month][0], day, default: #f)) =>
        #[];
      day =>
        blog.archive[year][0][month][0][day];
      month =>
        blog.archive[year][0][month][1];
      year =>
        blog.archive[year][1];
      (blog.entries.size > 0) =>
        map-as(<vector>, method (entry) entry end, blog.entries);  
      otherwise =>
        #[]
    end case;
  archive-entries := if (tags)
      filter-entries-by-tags(archive-entries, tags);
    else
      archive-entries
    end if;
  if (search)
    choose(method (entry)
        substring-position(entry.title, search) |
          substring-position(entry.content.content, search)
      end, archive-entries);
  else
    archive-entries;
  end if;
end;

define method filter-entries-by-tags (entries :: <sequence>, tags :: <sequence>)
 => (filtered-entries :: <sequence>);
  choose(method (entry)
      every?(method (tag)
          member?(tag, entry.categories, test: \=);
        end, tags);
    end, entries);
end;

define variable *configure-blog-page* =
  make(<turboblog-page>, source: "configure-blog.dsp");

define variable *view-blog-page* =
  make(<themed-turboblog-page>, source: "themes/default/index.dsp");

define variable *view-blogs-page* =
  make(<turboblog-page>, source: "view-blogs.dsp");

define variable *remove-blog-page* =
  make(<turboblog-page>, source: "remove-blog.dsp");


define named-method blog-show-entry-authors? in turboblog
 (page :: <turboblog-page>)
  if (*blog*)
    *blog*.show-entry-authors?
  else
    show-entry-authors?-default();
  end if;
end;

define tag show-blog-title in turboblog (page :: <turboblog-page>)
 ()
  if (*blog*)
    output("%s", escape-xml(*blog*.title));
  elseif (*form* & element(*form*, "title", default: #f))
    output("%s", escape-xml(*form*["title"]));
  end if;
end;

define tag show-blog-subtitle in turboblog (page :: <turboblog-page>)
 ()  
  if (*blog*)
    output("%s", escape-xml(*blog*.subtitle));
  elseif (*form* & element(*form*, "subtitle", default: #f))
    output("%s", escape-xml(*form*["subtitle"]));
  end if;
end;

define tag show-blog-description in turboblog (page :: <turboblog-page>)
 (output-format :: false-or(<string>), escape :: <boolean>)
  local method prepare (body :: <string>)
//      if (output-format) body := transform-content(output-format, body) end if;
      if (escape) body := escape-xml(body) end if;
      body;
    end;
  if (*blog*)
    output("%s", prepare(*blog*.description));
  elseif (*form* & element(*form*, "description", default: #f))
    output("%s", prepare(*form*["description"]));
  end if;
end;

define tag show-blog-feed-url in turboblog (page :: <turboblog-page>)
 (type :: <string>)
  if (*blog*)
    output("%s", escape-xml(feed-link(*blog*, type)));
  end if;
end;

define body tag list-blog-links in turboblog (page :: <turboblog-page>, do-body :: <function>)
 ()
  if (*blog*)
    for (link in *blog*.links)
      dynamic-bind(*link* = link)
        do-body();
      end;
    end for;
  end if;
end;

define body tag list-blog-participants in turboblog (page :: <turboblog-page>, do-body :: <function>)
 ()
  if (*blog*)
    for (participant in *blog*.participants)
      dynamic-bind(*participant* = participant)
        do-body();
      end;
    end for;
  end if;
end;

define tag show-blog-permanent-link in turboblog (page :: <turboblog-page>)
 ()
  if (*blog*)
    output("%s", permanent-link(*blog*));
  end if;
end;

define body tag list-blog-files in turboblog (page :: <turboblog-page>, do-body :: <function>)
 ()
  if (*blog*)
    let files = #[];
    do-directory(method (directory :: <pathname>, name :: <string>, type :: <file-type>)
        if (type = #"file" & first(name) ~= '.')
          files := add!(files,
            merge-locators(as(<file-system-locator>, name),
              as(<directory-locator>, directory)));
        end if;
      end, *blog*.directories[#"files"]);

    for (file in files)
      dynamic-bind(*file* = file)
        do-body();
      end;
    end for;
  end if;
end;

define body tag list-entries in turboblog (page :: <turboblog-page>, do-body :: <function>)
 (first-entry :: false-or(<string>), last-entry :: false-or(<string>), tagged :: false-or(<string>))
  first-entry := if (first-entry & is-numeric?(find-entry))
                    string-to-integer(first-entry)
                  end if;
  last-entry := if (last-entry & is-numeric?(last-entry))
                  string-to-integer(last-entry)
                end if;

  *entries* := *entries* | map-as(<vector>, method (entry) entry end, *blog*.entries);   
  if (tagged)
    *entries* := filter-entries-by-tags(*entries*, extract-tags(tagged));
  end if;
  for (index from (first-entry | 1) to (last-entry | size(*entries*)),
       entry in *entries*)
    dynamic-bind(*entry* = entry)
      do-body();
    end;
  end for;
end;

define body tag list-recent-entries in turboblog (page :: <turboblog-page>, do-body :: <function>)
 (first-entry :: false-or(<string>), last-entry :: false-or(<string>))
  first-entry := if (first-entry & is-numeric?(find-entry))
                    string-to-integer(first-entry)
                  end if;
  last-entry := if (last-entry & is-numeric?(last-entry))
                  string-to-integer(last-entry)
                end if;

  for (index from (first-entry | 1) to (last-entry | size(*blog*.entries)),
        entry in sort-table(*blog*.entries, published))
    dynamic-bind(*entry* = entry)
      do-body();
    end;
  end for;
end;

define body tag list-entries-monthly in turboblog (page :: <turboblog-page>, do-body :: <function>)
 ()
  local method bind-and-do-body (month)
    dynamic-bind(*month* = month)
      do-body();
    end;
  end;

  let month = #[];
  let entries = sort-table(*blog*.entries, published);
  for (entry in entries)
    let (this-year, this-month) = decode-date(entry.published);
    let (last-year, last-month) = if (size(month) > 0)
        decode-date(first(month).published);
      end if;
    if (month = #[] | (last-year & (last-year = this-year & last-month = this-month)))
      month := add!(month, entry);
      if (entry = last(entries))
        bind-and-do-body(month);
      end if;
    else
      bind-and-do-body(month);
      month := #[];
      month := add!(month, entry);
    end if;
  end for;
end;

define directory responder blogs-responder ("/blogs")
  let request = current-request();

  authenticate();

  let add? :: <boolean> = (get-query-value("add") = #t);

  let action :: <symbol> = case
      add? => #"add-blog";
      otherwise => #"list-blogs";
    end case;

  if (request.request-url-tail ~= "")
    respond-to(#"get", *non-existing-blog-page*);
  else
    with-permission(action)
      dynamic-bind(*action* = action)
        select (request.request-method)
          #"get" => case
              add? => respond-to(#"get", *configure-blog-page*);
              otherwise => respond-to(#"get", *view-blogs-page*);
            end case;
          #"post" => case
              add? => respond-to(#"post", *configure-blog-page*);
            end case;
        end select;
      end;
    end;
  end if;
end;

define method blog-responder (blog :: <blog>)
  let request = current-request();

  authenticate();

  local method deliver? (locator :: <locator>)
   => (result :: <boolean>);
    file-exists?(locator)
  end;

  let configure? :: <boolean> = (get-query-value("configure") = #t);
  let remove? :: <boolean> = (get-query-value("remove") = #t);
  let view? :: <boolean> = (request.request-url-tail = "");
  let feed :: false-or(<string>) = get-query-value("feed");

  let locator = document-location(request.request-url-tail, context:
    merge-locators(as(<directory-locator>, concatenate("themes/", blog.theme)),
      document-root(*virtual-host*)));

  let action :: <symbol> = case
      remove? => #"remove-blog";
      configure? => #"configure-blog";
      view? => #"view-blog";
      otherwise => #"view-file";
    end case;

  with-permission(action, blog: blog)
    dynamic-bind(*action* = action, *blog* = blog)
      select (request.request-method)
        #"get" => case
            configure? => respond-to(#"get", *configure-blog-page*);
            remove? => respond-to(#"get", *remove-blog-page*);
            feed => serve-feed(blog, as(<symbol>, feed));
            view? => respond-to(#"get", *view-blog-page*);
            deliver?(locator) =>
              static-file-responder(locator);
            otherwise => respond-to(#"get", *missing-page-error-page*);
          end case;
        #"post" => case
            configure? => respond-to(#"post", *configure-blog-page*);
            remove? => begin
                remove-blog(*blog-server*, blog);
                redirect-to("/blogs");
              end;
          end case;
      end select;
    end;
  end;
end;

define method respond-to (type == #"post", page :: <configure-blog-page>)
  let request = current-request();

  let current-title :: false-or(<string>) = *blog* & *blog*.title;
  let title :: false-or(<string>) = get-query-value("title");
  let subtitle :: false-or(<string>) = get-query-value("subtitle");
  let description :: false-or(<string>) = get-query-value("description");
  let theme :: false-or(<string>) = get-query-value("theme");
  let show-entry-authors :: <boolean> = (get-query-value("show-entry-authors") = "on");

  let errors :: <list> = #();

  if (~ instance?(title, <string>) | title = "")
    errors := add!(errors, #"title");
  else
    if (title ~= current-title & find-blog(*blog-server*, title))
      errors := add!(errors, #"blog-exists-already");
    end if;

    title := if (title ~= current-title) title end if;
  end if;

  if (~ instance?(theme, <string>) | theme = "")
    errors := add!(errors, #"theme");
  end if;

  subtitle := if (instance?(subtitle, <string>)) subtitle end if;
  description := if (instance?(description, <string>)) description end if;

  if (~ empty?(errors))
    dynamic-bind (*errors* = errors, *form* = request.request-query-values)
      next-method();
    end;
  else
    select (*action*) 
      #"configure-blog" => begin
          modify-blog(*blog*, title: title, subtitle: subtitle, theme: theme,
            description: description, show-entry-authors?: show-entry-authors);
          redirect-to(*blog*);
        end;
      #"add-blog" => begin
          let blog = make(<blog>, title: title, subtitle: subtitle, theme: theme,
              description: description, show-entry-authors?: show-entry-authors);
          add-blog(*blog-server*, blog);
          redirect-to(blog);
        end;
    end select;
  end if;
end;

define method archive-responder (blog :: <blog>)
  let request = current-request();

  authenticate();

  let (year, month, day, title) = apply(values,
    split(request.request-url-suffix, separator: "/"));
  let (year, month, day) = validate-dates(year, month, day); 

  let add? :: <boolean> = (get-query-value("add") = #t);
  let tagged :: false-or(<string>) = get-query-value("tagged");
  let search :: false-or(<string>) = get-query-value("search");
  let atom? :: <boolean> = (get-query-value("atom") = #t);

  let action :: <symbol> = case
      add? | request.request-method = #"post" => 
        #"add-entry";
      atom? => #"list-entries-as-atom";
      day => #"list-entries/day";
      month => #"list-entries/month";
      year => #"list-entries/year";
      otherwise => #"list-entries";
    end case;

  tagged := tagged & extract-tags(tagged);

  search := if (search & instance?(search, <string>))
      search;
    end if;

  let entries = sort(archive-entries(blog,
    year: year, month: month, day: day,
    tags: tagged, search: search), test: method (first, second)
        first.published > second.published
      end);

  with-permission(action, blog: blog)
    dynamic-bind(*action* = action, *blog* = blog, *entries* = entries)
      if (title)
        respond-to(#"get", *non-existing-entry-page*);
      else
        select (request.request-method)
          #"get" => case
              add? => respond-to(#"get", *edit-entry-page*);
              atom? => serve-feed (blog, #"atom", feed-entries: entries);
              otherwise => respond-to(#"get", *view-entries-page*);
            end case;
          #"post" => respond-to(#"post", *edit-entry-page*);
        end select;
      end if;
    end;
  end;
end;

