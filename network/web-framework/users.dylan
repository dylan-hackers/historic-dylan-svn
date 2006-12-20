module: users
author: Hannes Mehnert <hannes@mehnert.org>

define class <access-level> (<object>)
end;

define open web-class <user> (<object>)
  data username :: <string>;
  data password :: <string>;
  data email :: <string>;
  slot access :: <list> = make(<list>);
  has-a access-level;
end;
/*
define method initialize (user :: <user>, #rest rest, #key, #all-keys)
  next-method();
  check(user);
  save(user);
end;
*/
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

define method set-current-user (user :: <user>) => (user :: <user>)
  *user* := user;
end;
define method check (user :: <user>, #key test-result = 0)
 => (res :: <boolean>)
  if (element(storage(<user>), user.username, default: #f))
    signal(make(<web-error>,
                error: "User with same name already exists!"))
  else
    #t;
  end if;
end method check;

define method valid-user? (username :: <string>, pass :: <string>)
  let user = element(storage(<user>), username, default: #f);
  if (user & (user.password = pass))
    #t;
  else
    #f;
  end if;
end method valid-user?;

define method login (request :: <request>, username, password)
 => (username :: false-or(<string>))
  if (username & password & valid-user?(username, password))
    let session = ensure-session(request);
    *user* := storage(<user>)[username];
    set-attribute(session, #"username", username);
    username;
  end if;
end method login;

define method logged-in? (request :: <request>)
 => (username :: false-or(<string>))
  let session = get-session(request);
  if (session)
    let username = get-attribute(session, #"username");
    *user* := storage(<user>)[username];
    username;
  end;
end method logged-in?;

