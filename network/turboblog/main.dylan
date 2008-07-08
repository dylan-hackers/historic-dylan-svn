module: turboblog
author: turbo24prg

// generator
define constant $generator = make(<generator>,
  text: "turboblog", version: "seks",
  uri: "http://projects.turbolent.com/turboblog");

define object-tests
 (language, pingback, month, entries, 
  tag, author, theme, role, client, content-type)
in turboblog end;

// client's language
define thread variable *client-language* = #f;

define table *languages-in-english* = {
  #"english" => "English",
  #"german" => "German"
};

define table *languages-in-german* = {
  #"english" => "Englisch",
  #"german" => "Deutsch"
};

define table *supported-languages* = {
  #"english" => *languages-in-english*,
  #"german" => *languages-in-german*
};

define table *http-languages* :: <string-table> = {
  "en" => #"english",
  "en-gb" => #"english",
  "de-de" => #"german"
}; 

define table *supported-content-types* = {
  "xhtml" => "XHTML",
  "markup" => "Markup"
};

// tags
define tag show-language in turboblog (page :: <turboblog-page>)
 ()
  if (*language*)
    output("%s", escape-xml(tail(*language*)));
  end if;
end;

define tag show-language-name in turboblog (page :: <turboblog-page>)
 ()  
  if (*language*)
    output("%s", escape-xml(head(*language*)));
  end if;
end;

define tag show-content-type in turboblog (page :: <turboblog-page>)
 ()
  if (*content-type*)
    output("%s", escape-xml(tail(*content-type*)));
  end if;
end;

define tag show-content-type-name in turboblog (page :: <turboblog-page>)
 ()
  if (*content-type*)
    output("%s", escape-xml(head(*content-type*)));
  end if;
end;

define tag show-role in turboblog (page :: <turboblog-page>)
 ()
  if (*role*)
    output("%s", escape-xml(*role*));
  end if;
end;

define tag show-file-filename in turboblog (page :: <turboblog-page>)
 ()
  if (*file*)
    output("%s",
      concatenate(*file*.locator-base, if (*file*.locator-extension)
        concatenate(".", *file*.locator-extension) else "" end if));
  end if;
end;

define tag show-tag in turboblog (page :: <turboblog-page>)
 ()
  if (*tag*)
    output("%s", escape-xml(*tag*));
  end if;
end;

define tag show-author in turboblog (page :: <turboblog-page>)
 ()
  if (*author*)
    output("%s", escape-xml(*author*));
  end if;
end;

define tag show-month-date in turboblog (page :: <turboblog-page>)
 (formatted :: <string>)
  if (*month*)
    output("%s", format-date(formatted, first(*month*).published));
  end if;
end;              

define tag show-month-count in turboblog (page :: <turboblog-page>)
 ()
  if (*month*)
    output("%s", size(*month*));
  end if;
end;

define tag show-month-archive-url in turboblog (page :: <turboblog-page>)
 ()
  if (*month*)
    output("%s", archive-url(*month*));
  end if;
end;

define tag show-login-url in turboblog (page :: <turboblog-page>)
 (redirect :: type-union(<string>, <boolean>), current :: <boolean>)
  output("/?login%s",
    if (redirect)
      format-to-string("&amp;redirect=%s",
        encode-url(if (current) current-url() else redirect end if, reserved?: #t));
    else "" end);
end;

define tag show-logout-url in turboblog (page :: <turboblog-page>)
 (redirect :: type-union(<string>, <boolean>), current :: <boolean>)
  output("/?logout%s", 
    if (redirect)
      format-to-string("&amp;redirect=%s",
        encode-url(if (current) current-url() else redirect end if, reserved?: #t));
    else "" end);
end;

define tag show-current-url in turboblog (page :: <turboblog-page>)
 ()
  output("%s", current-url());
end;

define tag show-theme in turboblog (page :: <turboblog-page>)
 ()
  if (*theme*)
    output("%s", escape-xml(*theme*));
  end if;
end;

define tag show-search in turboblog (page :: <turboblog-page>)
 ()
  let search = get-query-value("search");
  if (search)
    output("%s", escape-xml(search));
  end if;
end;

define named-method authenticated? in turboblog (page :: <turboblog-page>)
  authenticated-user()
end;

define named-method editable? in turboblog (page :: <turboblog-page>,)
  #f;
end;

define named-method editable? in turboblog (page :: <view-entry-page>)
  #t;
end;

define named-method removable? in turboblog (page :: <turboblog-page>)
  #f;
end;

define named-method removable? in turboblog (page :: <view-entry-page>)
  #t;
end;

define named-method removable? in turboblog (page :: <edit-entry-page>)
  #t;
end;

// error tests
define error-tests
 (title, name, body, url, user, username, 
  password, filename, file, role, type,
  user-exists-already, user-doesnt-exist,
  blog-exists-already, entry-exists-already,
  link-exists-already, file-exists-already,
  participant-exists-already, unknown-role)
in turboblog end;

define named-method query-tagged? in turboblog
 (page :: <turboblog-page>)
  get-query-value("tagged");
end;

define named-method has-tags? in turboblog
 (page :: <turboblog-page>)
  *entry* & (size(*entry*.categories) > 0);
end;

define named-method has-comments? in turboblog
 (page :: <turboblog-page>)
  *entry* & (size(*entry*.comments) > 0);
end;

define named-method has-pingbacks? in turboblog
 (page :: <turboblog-page>)
  *entry* & (size(*entry*.pingbacks) > 0);
end;

define named-method has-website? in turboblog
 (page :: <turboblog-page>)
  *comment* & (*comment*.website ~= "");
end;

define named-method last-link-tag? in turboblog
 (page :: <turboblog-page>)
  if (*link* & *tag*)
    *tag* = last(*link*.categories);
  end if;
end;

define named-method blog-theme? in turboblog
 (page :: <turboblog-page>)
  *theme* & *blog* & *blog*.theme = *theme*;
end;

define named-method participant-role? in turboblog
 (page :: <turboblog-page>)
  *role* & *participant* & *participant*.participant-role = *role*;
end;

define body tag with-authenticated-user in turboblog
 (page :: <turboblog-page>, do-body :: <function>)
 ()
  dynamic-bind(*user* = authenticated-user()) 
    do-body();
  end;
end;

define body tag list-themes in turboblog
 (page :: <turboblog-page>, do-body :: <function>)
 ()
  let themes = #[];
  do-directory(method (directory :: <pathname>, name :: <string>, type :: <file-type>)
      if (type = #"directory" & first(name) ~= '.')
        themes := add!(themes, name);
      end if;
    end, concatenate(as(<string>, document-root(*virtual-host*)), "/themes/"));

  for (theme in themes)
    dynamic-bind(*theme* = theme)
      do-body();
    end;
  end for;
end;

define body tag list-languages in turboblog
 (page :: <turboblog-page>, do-body :: <function>)
 ()
  for (language keyed-by language-name in *supported-languages*[*client-language*])
    dynamic-bind(*language* = pair(language-name, language))
      do-body();
    end;
  end for;
end;

define body tag list-supported-content-types in turboblog
 (page :: <turboblog-page>, do-body :: <function>)
 ()
  for (content-type keyed-by content-type-name in *supported-content-types*)
    dynamic-bind(*content-type* = pair(content-type-name, content-type))      
      do-body();
    end;
  end for;
end;

define body tag list-possible-participants in turboblog
 (page :: <turboblog-page>, do-body :: <function>)
 ()
  for (user in choose(method (user)
    ~ user.administrator? & ~ find-participant(*blog*, user.username);
   end,  map-as(<vector>, method (element) element end, storage(<blog-user>))))
    dynamic-bind(*user* = user)
      do-body();
    end;
  end for;
end;

define body tag list-roles in turboblog
 (page :: <turboblog-page>, do-body :: <function>)
 ()
  for (role in $roles)
    dynamic-bind(*role* = role)
      do-body();
    end;
  end for;
end;

define body tag list-query-tags in turboblog
 (page :: <turboblog-page>, do-body :: <function>)
 ()
  for (tag in extract-tags(get-query-value("tagged")))
    dynamic-bind(*tag* = tag)
      do-body();
    end;
  end for;
end;

define responder root-responder ("/")
  let request = current-request();
  authenticate();

  let login? :: <boolean> = (get-query-value("login") = #t);
  let logout? :: <boolean> = (get-query-value("logout") = #t);
  let administrate? :: <boolean> = (get-query-value("administrate") = #t);
  let remove? :: <boolean> = (singleblog?() & (get-query-value("remove") = #t));
  let view? :: <boolean> = singleblog?();
  
  let action :: <symbol> = case
      login? => #"login";
      logout? => #"logout";
      administrate? => #"administrate";
      remove? => #"remove-blog";
      view? => #"view-blog";
      otherwise => #"list-blogs";
    end;

  let blog = if ((view? | remove?) & ~administrate?)
      singleblog();
    end if;

  with-permission(action, blog: blog)
    dynamic-bind(*action* = action, *blog* = blog)
      select (request.request-method)
        #"get" => case
            login? => login();
            logout? => logout();
            administrate? => respond-to(#"get", *administrate-blog-server-page*);
//          get-query-value("configure") =>
//            respond-to(#"get", *configure-blog-page*, request, response);
//            multiblog?() => redirect-to("/blogs");
            singleblog?() => redirect-to(singleblog());
            ~ blogs?() => respond-to(#"get", *no-blogs-page*);
//            otherwise => respond-to(#"get", *index-page*, request, response);
            otherwise => redirect-to("/blogs");
          end case;
        #"post" => case
            administrate? =>
              respond-to(#"post", *administrate-blog-server-page*);
          end case;
      end select;
    end;
  end;  
end;

define method setup (class == <blog-server>)
 => ();
  *blog-server* := first(storage(<blog-server>));
  for (blog in *blog-server*.blogs)
    register-responder(blog);
  end for;
end;

define function main () => ()
  let config-file = if (application-arguments().size > 0)
                      application-arguments()[0]
                    end;
  start-sockets();
   
  //XXX: fix!
  dumper(interval: 30);
  register-url("/administration-style.css", maybe-serve-static-file);

  let blog-server = make(<blog-server>, title: "T24", url: parse-uri("http://localhost/", as: <url>));
  *blog-server* := blog-server;
  save(blog-server);

  save(make(<blog-user>, username: "admin", 
    password: "admin", email: "", administrator?: #t));

  register-xml-rpc-method("pingback.ping", ping);

  start-server(config-file: config-file);
end;

begin
  main();
end;
