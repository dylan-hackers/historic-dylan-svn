module: users
author: Hannes Mehnert <hannes@mehnert.org>

define thread variable *authenticated-user* = #f;
define variable *ignore-authorizations* = list();
define variable *ignore-logins* = list();

define open class <user> (<object>)
  slot username :: <string>,
    required-init-keyword: username:;
  slot password :: <string>,
    required-init-keyword: password:;
  slot email :: <string>,
    required-init-keyword: email:;
  slot additional-information :: <table> = make(<table>),
    init-keyword: additional-information:;
end;

define method initialize (user :: <user>, #rest rest, #key, #all-keys)
  next-method();
  let keyword = #f;
  for (element in rest)
    if (keyword) 
      unless (member?(keyword, #(#"username", #"password", #"email")))
        user.additional-information[keyword] := element;
      end;
      keyword := #f;
    else
      keyword := element;
    end if;
  end for;
  if (find-user(user.username))
    signal(make(<web-error>,
      error: "User with same name already exists!"))
  else
    save(user);
  end if;
end;
define method key (user :: <user>)
 => (res :: <string>);
  user.username;
end;

define method storage-type (type == <user>) => (res)
  <string-table>;
end;

define method as (class == <string>, user :: <user>)
 => (result :: <string>)
  user.username;
end;

define function authenticated-user ()
 => (user :: false-or(<user>));
  *authenticated-user*
end;

define function find-user (name :: <string>)
 => (user :: false-or(<user>))
  element(storage(<user>), name, default: #f);
end method check;

define method login ()
  let redirect-url = get-query-value("redirect");
  let authorization = header-value(#"authorization");
  let user = check-authorization();
  if (~authorization | ~user)
    require-authorization();
  elseif (user &
      member?(user, *ignore-authorizations*, test: \=) &
      member?(user, *ignore-logins*, test: \=))
    *ignore-authorizations* :=
      remove!(*ignore-authorizations*, user);
    require-authorization();
  elseif (user &
          ~member?(user, *ignore-authorizations*, test: \=) &
          member?(user, *ignore-logins*, test: \=))
    *ignore-logins* :=
      remove!(*ignore-logins*, user);
    redirect-url & redirect-to(redirect-url);
  else
    redirect-url & redirect-to(redirect-url);
  end if;
end;

define function logout ()
  let user = check-authorization();
  if (user)
    *ignore-authorizations* :=
      add!(*ignore-authorizations*, user);
    *ignore-logins* :=
      add!(*ignore-logins*, user);
  end if;
  let redirect-url = get-query-value("redirect");
  redirect-url & redirect-to(redirect-url);
end;

define function check-authorization ()
 => (user :: false-or(<user>));
  let authorization = header-value(#"authorization");
  if (authorization)
    let user = find-user(head(authorization));
    if (user & user.password = tail(authorization))
      user;
    end if;
  end if;
end;

define function authenticate ()
 => (user :: false-or(<user>));
  let authorization = header-value(#"authorization");
  if (authorization)
    let user = find-user(head(authorization));
    *authenticated-user* := if (user & user.password = tail(authorization) &
        ~member?(user, *ignore-authorizations*, test: \=) &
        ~member?(user, *ignore-logins*, test: \=)) //???
          user;
      end if;
  end if;
end;

define function require-authorization (#key realm :: <string> = "koala")
  let headers = current-response().response-headers;
  add-header(headers, "WWW-Authenticate", concatenate("Basic realm=\"", realm, "\""));
  unauthorized-error(headers: headers);
end;
