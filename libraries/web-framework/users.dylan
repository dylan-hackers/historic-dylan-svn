module: users
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <user> (<object>)
  data username :: <string>;
  data password :: <string>;
  data email :: <string>;
  data access :: <list> = make(<list>);
end;

define method initialize (user :: <user>, #rest rest, #key, #all-keys)
  next-method();
  check(user);
  with-storage (table = <user>)
    table[user.username] := user;
  end;
end;

define inline-only method key (user :: <user>)
 => (res :: <string>)
  user.username;
end;

define method storage-type (type == <user>) => (res)
  <string-table>;
end;

define method as (class == <string>, user :: <user>)
 => (result :: <string>)
  concatenate(user.username, " ", user.email);
end;

define thread variable *user* = #f;

define method current-user () => (user :: false-or(<user>))
  *user*
end;

define method check (user :: <user>, #key test-result = 0)
 => (res :: <boolean>)
  if (element(storage(<user>), user.username, default: #f))
    signal(make(<web-error>,
                error: "User with same name already exists!"))
  else
    #t;
  end;
end;

define method valid-user? (user-name :: <string>, pass :: <string>)
  let user = element(storage(<user>), user-name, default: #f);
  if (user & (user.password = pass))
    #t;
  else
    #f;
  end;
end;

define method login (request :: <request>)
 => (username :: false-or(<string>))
  let user-name = get-query-value("username");
  let password = get-query-value("password");
  if (user-name & password)
    if (valid-user?(user-name, password))
      let session = ensure-session(request);
      *user* := storage(<user>)[user-name];
      set-attribute(session, #"username", user-name);
      user-name;
    end;
  end;
end;

define method logged-in? (request :: <request>)
 => (username :: false-or(<string>))
  let session = get-session(request);
  if (session)
    let user-name = get-attribute(session, #"username");
    *user* := storage(<user>)[user-name];
    user-name;
  end;
end;

