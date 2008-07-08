module: turboblog
author: turbo24prg

define variable *blog-server* :: false-or(<blog-server>) = #f;

define class <blog-server> (<object>)
  slot blog-server-title :: <string>,
    required-init-keyword: title:;
  slot blog-server-url :: <url>,
    required-init-keyword: url:;
  slot blogs :: <string-table> = make(<string-table>),
    init-keyword: blogs:;
  slot users :: <string-table> = make(<string-table>),
    init-keyword: users:;
  slot ignore-authorizations :: <list> = list(),
    init-keyword: ignore-authorizations:;
  slot ignore-logins :: <list> = list(),
    init-keyword: ignore-logins:;
end;

define action-tests (administrate) in turboblog end;

define function blogs? ()
 => (result :: <boolean>);
  size(*blog-server*.blogs) > 0
end;

define function multiblog? ()
 => (result :: <boolean>);
  size(*blog-server*.blogs) > 1
end;

define function singleblog? ()
 => (result :: <boolean>);
  size(*blog-server*.blogs) = 1
end;

// TODO: make configurable
define function singleblog ()
  first(map-as(<list>, identity, *blog-server*.blogs));
end;


define variable *administrate-blog-server-page* =
  make(<turboblog-page>, source: "administrate-blog-server.dsp");


define tag show-blog-server-url in turboblog (page :: <turboblog-page>)
 ()
  output("%s", permanent-link(*blog-server*));
end;

define tag show-blog-server-title in turboblog (page :: <turboblog-page>)
 ()
  output("%s", escape-xml(*blog-server*.blog-server-title));
end;

define body tag list-blogs in turboblog (page :: <turboblog-page>, do-body :: <function>)
 ()
  for (blog in *blog-server*.blogs)
    dynamic-bind(*blog* = blog)
      do-body();
    end;
  end for;
end;

define body tag list-users in turboblog (page :: <turboblog-page>, do-body :: <function>)
 ()
  for (user in storage(<blog-user>))
    dynamic-bind(*user* = user)
      do-body();
    end;
  end for;
end;

define method respond-to (type == #"post", page :: <administrate-blog-server-page>)

  let title :: false-or(<string>) = get-query-value("title");
  let errors :: <list> = #();

  if (~ instance?(title, <string>) | title = "")
    errors := add!(errors, #"title");
  end if;

  case
    ~ empty?(errors) =>
      dynamic-bind (*errors* = errors, *form* = current-request().request-query-values)
        next-method();
      end;
    otherwise => begin
        title & (*blog-server*.blog-server-title := title);
        redirect-to("/");
      end;
  end case;
end;
