module: web-framework
author: Hannes Mehnert <hannes@mehnert.org>

define variable *users* = make(<string-table>);

define web-class <user> (<object>)
  data username :: <string>;
  data password :: <string>;
  data email :: <string>;
  //slot object-table :: <table>, init-keyword: table:;
  data admin? :: <boolean>;
end;

define method as (class == <string>, user :: <user>)
 => (result :: <string>)
  concatenate(user.username, " ", user.email);
end;

define thread variable *user* = #f;

define method current-user () => (user :: false-or(<user>))
  *user*
end;

define inline-only method key (user :: <user>)
  user.username;
end;

define method check (user :: <user>, #key test-result = 0)
 => (res :: <boolean>)
  if (any?(method(x) x.username = user.username end, storage(<user>)))
    signal(make(<web-error>,
                error: "User with same name already exists!"))
  else
    #t;
  end;
end;

define method valid-user? (user-name :: <string>, pass :: <string>)
  let user = choose(method(x) x.username = user-name end, storage(<user>))[0];
  if (user & (user.password = pass))
    #t;
  else
    #f;
  end;
end;

define method login (request :: <request>)
  let user-name = get-query-value("username");
  let password = get-query-value("password");
  if (username & password)
    if (valid-user?(user-name, password))
      let session = ensure-session(request);
      *user* := choose(method(x) x.username = user-name end, storage(<user>))[0];
      set-attribute(session, #"username", user-name);
    end;
  end;
end;

define method logged-in (request :: <request>)
 => (username :: false-or(<string>))
  let session = get-session(request);
  if (session)
    let user-name = get-attribute(session, #"username");
    *user* := choose(method(x) x.username = user-name end, storage(<user>))[0];
    user-name;
  end;
end;

