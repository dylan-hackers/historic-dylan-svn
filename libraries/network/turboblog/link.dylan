module: turboblog
author: turbo24prg

define object-tests (link) in turboblog end;

define class <blog-link> (<link>)
  slot link-blog :: <blog>,
    init-keyword: link-blog:;
end;

define action-tests
 (add-link, edit-link, remove-link, list-links)
in turboblog end;

define method add-link (blog :: <blog>, link :: <blog-link>)
 => (link :: <blog-link>);
  blog.links[link.title] := link;
  link.link-blog := blog;
  link;
end;

define method find-link (blog :: <blog>, title :: <string>)
 => (link :: false-or(<blog-link>));
  element(blog.links, title, default: #f)
end;

define method remove-link (blog :: <blog>, link :: <blog-link>)
 => (link :: <blog-link>);
  remove-key!(blog.links, link.title);
  link;
end;

define method modify-link (link :: <blog-link>, #key 
 title: link-title :: false-or(<string>),
 url: link-url :: false-or(<string>),
 description: link-description :: false-or(<string>),
 tags: link-tags :: false-or(<vector>))
 => (link :: <blog-link>);
 if (link-title)
   remove-key!(link.link-blog.links, link.title);
   link.title := link-title;
   link.link-blog.links[link.title] := link;
 end if;
 link-url & (link.href := link-url);
 link-description & (link.description := link-description);
 link-tags & (link.categories := link-tags);
 link;
end;

define variable *edit-link-page* =
  make(<turboblog-page>, source: "edit-link.dsp");

define variable *view-link-page* =
  make(<turboblog-page>, source: "view-link.dsp");

define variable *view-links-page* =
  make(<turboblog-page>, source: "view-links.dsp");

define variable *remove-link-page* =
  make(<turboblog-page>, source: "remove-link.dsp");


define tag show-link-title in turboblog
 (page :: <turboblog-page>)
 () 
  if (*link*)
    output("%s", escape-xml(*link*.title));
  elseif (*form* & element(*form*, "title", default: #f))
    output("%s", escape-xml(*form*["title"]));
  end if;
end;

define tag show-link-url in turboblog (page :: <turboblog-page>)
 ()
  if (*link*)
    output("%s", *link*.href);
  elseif (*form* & element(*form*, "url", default: #f))
    output("%s", *form*["url"]);
  end if;
end;

define tag show-link-description in turboblog (page :: <turboblog-page>)
 ()  
  if (*link*)
    output("%s", escape-xml(*link*.description));
  elseif (*form* & element(*form*, "description", default: #f))
    output("%s", escape-xml(*form*["description"]));
  end if; 
end;

define tag show-link-permanent-link in turboblog (page :: <turboblog-page>)
 ()
  if (*link*)
    output("%s", permanent-link(*link*));
  end if;
end;

define body tag list-link-tags in turboblog (page :: <turboblog-page>, do-body :: <function>)
 ()
  if (*link*)
    for (tag in *link*.categories)
      dynamic-bind(*tag* = tag)
        do-body();
      end;
    end for;
  elseif (*form* & element(*form*, "tags", default: #f))
    output("%s", escape-xml(*form*["tags"]));
  end if;
end;

define method respond-to (type == #"post", page :: <edit-link-page>)
  let request = current-request();

// action
  let edit? = (*action* = #"edit-link");
  let add? = (*action* = #"add-link");

// form
  let title = get-query-value("title");
  let url = get-query-value("url");
  let description = get-query-value("description");
  let tags = get-query-value("tags");

// errors
  let errors = #();

// checks
  if (~ instance?(title, <string>) | title = "")
    errors := add!(errors, #"title");
  end if;

  if (~ instance?(url, <string>) | url = "")
    errors := add!(errors, #"url");
  end if;

  tags := if (instance?(tags, <string>))
            remove-duplicates!(split(tags,
              separator: " "), test: \=);
          elseif (tags = #t) #[] end if;

  description := if (instance?(description, <string>))
      description
    end if;

  case
    ~ empty?(errors) =>
      dynamic-bind (*errors* = errors, *form* = request.request-query-values)
        next-method();
      end;
    edit? => begin
        modify-link(*link*, title: title, url: url,
          description: description, tags: tags);
        redirect-to(*link*);
      end;
    add? => begin
        let link = make(<blog-link>, title: title, href: url,
          description: description, categories: tags);
        add-link(*blog*, link);
        redirect-to(link);
      end;
  end case;
end;

define method links-responder (blog :: <blog>)
  let request = current-request();
  authenticate();

  let add? :: <boolean> = (get-query-value("add") = #t);
  let edit? :: <boolean> = (get-query-value("edit") = #t);
  let remove? :: <boolean> = (get-query-value("remove") = #t);
  let view? :: <boolean> = (request.request-url-tail ~= "");

  let action :: <symbol> = case
      add? => #"add-link";
      edit? => #"edit-link";
      remove? => #"remove-link";
      view? => #"view-link";
      otherwise => #"list-links";
    end case;

  let link = if (edit? | view? | remove?)
      find-link(blog, request.request-url-tail);
    end if;

  if ((edit? | view? | remove?) & ~ link)
// TODO:
//    respond-to(#"get", *non-existing-link-page*, request, response);
  else
    with-permission(action, blog: blog, link: link)
      dynamic-bind(*action* = action, *blog* = blog, *link* = link)
        select (request.request-method)
          #"get" => case
              add? | edit? => respond-to(#"get", *edit-link-page*);
              remove? => respond-to(#"get", *remove-link-page*);
              view? => respond-to(#"get", *view-link-page*);
              otherwise => respond-to(#"get", *view-links-page*);
            end case;
          #"post" => case
              add? | edit? => respond-to(#"post", *edit-link-page*);
              remove? => begin
                  remove-link(blog, link);
                  redirect-to(blog);
                end;
            end case;
        end select;
      end;
    end;
  end if;
end;
