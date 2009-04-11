module: users
author: Hannes Mehnert <hannes@mehnert.org>

define thread variable *authenticated-user* = #f;

// The default "realm" value passed in the WWW-Authenticate header.
//
define variable *default-authentication-realm* :: <string> = "koala";

// Because clients (browsers) continue to send the Authentication header
// once an authentication has been accepted (at least until the browser
// is restarted, it seems) we need to keep track of the fact that a user
// has logged out by storing the auth values here.
//
define variable *ignore-authorizations* = list();
define variable *ignore-logins* = list();

define open class <user> (<object>)
  slot username :: <string>,
    required-init-keyword: username:;
  slot password :: <string>,
    required-init-keyword: password:;
  slot email :: <string>,
    required-init-keyword: email:;
  slot administrator? :: <boolean> = #f,
    init-keyword: administrator?:;
end class <user>;

define method initialize (user :: <user>, #key)
  next-method();
  if (find-user(user.username))
    signal(make(<web-error>,
                error: "User with same name already exists!"));
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
  authenticate();
  *authenticated-user*
end;

define function find-user
    (name :: <string>)
 => (user :: false-or(<user>))
  element(storage(<user>), name, default: #f);
end;

define method login (#key realm :: false-or(<string>))
  let redirect-url = get-query-value("redirect");
  let user = check-authorization();
  if (~user)
    require-authorization(realm: realm);
  elseif (member?(user, *ignore-authorizations*, test: \=) &
          member?(user, *ignore-logins*, test: \=))
    *ignore-authorizations* := remove!(*ignore-authorizations*, user);
    require-authorization(realm: realm);
  elseif (~member?(user, *ignore-authorizations*, test: \=) &
          member?(user, *ignore-logins*, test: \=))
    *ignore-logins* := remove!(*ignore-logins*, user);
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
  let authorization = get-header(current-request(), "Authorization", parsed: #t);
  if (authorization)
    let user = find-user(head(authorization));
    if (user & user.password = tail(authorization))
      user;
    end if;
  end if;
end;

define function authenticate ()
 => (user :: false-or(<user>));
  let authorization = get-header(current-request(), "Authorization", parsed: #t);
  if (authorization)
    let user = find-user(head(authorization));
    *authenticated-user*
      := if (user
               & user.password = tail(authorization)
               & ~member?(user, *ignore-authorizations*, test: \=)
               & ~member?(user, *ignore-logins*, test: \=)) //???
           user
         end;
  end
end function authenticate;

define function require-authorization (#key realm :: false-or(<string>))
  let realm = realm | *default-authentication-realm*;
  let headers = current-response().raw-headers;
  add-header(headers, "WWW-Authenticate", concatenate("Basic realm=\"", realm, "\""));
  unauthorized-error(headers: headers);
end;

define method \= (user1 :: <user>, user2 :: <user>)
 => (equal? :: <boolean>);
  user1.username = user2.username
end;

