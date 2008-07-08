module: turboblog
author: turbo24prg

define thread variable *comment* :: false-or(<blog-comment>) = #f;

define class <blog-comment> (<comment>)
  slot comment-number :: <integer>,
    init-keyword: comment-number:; 
  slot comment-entry :: <blog-entry>,
    init-keyword: comment-entry:;
  slot comment-user :: false-or(<blog-user>) = #f,
    init-keyword: comment-user:;
end;

define action-tests (edit-comment) in turboblog end;

define method add-comment (entry :: <blog-entry>, comment :: <blog-comment>)
 => (comment :: <blog-comment>);
  let new-number = entry.comments-count;
  comment.comment-number := new-number;
  entry.comments[new-number] := comment;
  comment.comment-entry := entry;
  comment;
end;

define method modify-comment (comment :: <blog-comment>, #key 
 name: comment-name :: false-or(<string>), content: comment-content :: false-or(<content>),
 website: comment-website :: false-or(<string>))
 => (comment :: <blog-comment>);  
  comment-name & (comment.name := comment-name);
  comment-content & (comment.content := comment-content);
  comment-website & (comment.website := comment-website);
  comment;
end;

define method remove-comment (entry :: <blog-entry>, comment :: <blog-comment>)
 => (comment :: <blog-comment>);  
  remove-key!(entry.comments, comment.comment-number);
  comment;
end;

define method find-comment (entry :: <blog-entry>, comment-number :: <integer>)
 => (comment :: false-or(<blog-comment>));
  element(entry.comments, comment-number, default: #f);
end;

define variable *edit-comment-page* =
  make(<themed-turboblog-page>,
       source: "themes/default/edit-comment.dsp");

define variable *view-comment-page* =
  make(<themed-turboblog-page>,
       source: "themes/default/view-comment.dsp");

define variable *view-comments-page* =
  make(<themed-turboblog-page>,
       source: "themes/default/view-comments.dsp");

define variable *remove-comment-page* =
  make(<themed-turboblog-page>,
       source: "themes/default/remove-comment.dsp");

define tag show-comment-comment-number in turboblog (page :: <turboblog-page>)
 ()
  if (*comment*)
    output("%s", *comment*.comment-number);
  end if;
end;

define tag show-comment-name in turboblog (page :: <turboblog-page>)
 ()
  if (*comment*)
    output("%s", escape-xml(*comment*.name));
  elseif (*form* & element(*form*, "name", default: #f))
    output("%s", escape-xml(*form*["name"]));
  end if;
end;

define tag show-comment-website in turboblog (page :: <turboblog-page>)
 ()
  if (*comment*)
    output("%s", *comment*.website);
  elseif (*form* & element(*form*, "website", default: #f))
    output("%s", escape-xml(*form*["website"]));
  end if;
end;

define tag show-comment-body in turboblog (page :: <turboblog-page>)
 ()
  if (*comment* & *comment*.content)
    output("%s", escape-xml(*comment*.content.content));
  elseif (*form* & element(*form*, "body", default: #f))
    output("%s", escape-xml(*form*["body"]));
  end if;
end;

define tag show-comment-published in turboblog (page :: <turboblog-page>)
 (formatted :: <string>)
  if (*comment*)
    output("%s", format-date(formatted, *comment*.published));    
  end if;
end;

define method respond-to (type == #"post", page :: <edit-comment-page>)

// action
  let edit? = (*action* = #"edit-comment");
  let add? = (*action* = #"add-comment");

// form
  let name = get-query-value("name");
  let body = get-query-value("body");
  let website = get-query-value("website");
  let on-error-page = get-query-value("on-error-page");

// errors
  let errors = #();

// checks
  if (~ instance?(name, <string>) | name = "")
    errors := add!(errors, #"name");
  end if;

  if (~ instance?(body, <string>) | body = "")
    errors := add!(errors, #"body");
  end if;

  website := if (instance?(website, <string>)) website end if;

  case
    ~ empty?(errors) =>
      dynamic-bind (*errors* = errors, *form* = current-request().request-query-values)
        let valid = instance?(on-error-page, <string>);
        case
          valid & on-error-page = "view-entry-page" =>
            respond-to(#"get", *view-entry-page*);
          otherwise =>
            next-method();
        end case;
      end;
    edit? => begin
        modify-comment(*comment*, name: name,
          content: make(<xhtml-content>, content: body),
          website: website);
        redirect-to(*comment*);
      end;
    add? => begin
        let comment = make(<blog-comment>, name: name,
          content: make(<xhtml-content>, content: body),
          website: website);
        add-comment(*entry*, comment);
        if (authenticated-user())
          comment.comment-user := authenticated-user();
        end if;
        redirect-to(comment);
      end;
  end case;
end;

define method comments-responder (entry :: <blog-entry>)
  let request = current-request();

  authenticate();

  let blog = entry.entry-blog;

  let add? :: <boolean> = (get-query-value("add") = #t);
  let edit? :: <boolean> = (get-query-value("edit") = #t);
  let remove? :: <boolean> = (get-query-value("remove") = #t);
  let view? :: <boolean> = (request.request-url-tail ~= "");

  let action :: <symbol> = case
      add? => #"add-comment";
      edit? => #"edit-comment";
      remove? => #"remove-comment";
      view? => #"view-comment";
      otherwise => #"list-comments";
    end case;

  let comment = if (edit? | view? | remove?)
      block  ()
        find-comment(entry, string-to-integer(request.request-url-tail));
      exception (<error>)
      end;
    end if;

  if ((edit? | view? | remove?) & ~ comment)
// TODO:
//    respond-to(#"get", *non-existing-comment-page*, request, response);
  else
    with-permission(action, entry: entry, comment: comment)
      dynamic-bind(*action* = action, *blog* = blog,
       *entry* = entry, *comment* = comment)
        if ((edit? | view? | remove?) & ~ comment)
          respond-to(#"get", *non-existing-comment-page*);
        else
          select (request.request-method)
            #"get" => case
                add? | edit? => respond-to(#"get", *edit-comment-page*);
                remove? => respond-to(#"get", *remove-comment-page*);
                view? => redirect-to(entry, comment: comment);  //respond-to(#"get", *view-comment-page*);
                otherwise => redirect-to(entry, comments?: #t); //respond-to(#"get", *view-comments-page*);
              end case;
            #"post" => case
                add? | edit? => respond-to(#"post", *edit-comment-page*);
                remove? => begin
                    remove-comment(entry, comment);
                    redirect-to(entry);
                  end;
              end case;
          end select;
        end if;
      end;
    end;
  end if;
end;
