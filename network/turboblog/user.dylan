module: turboblog
author: turbo24prg

define constant <blog-user> = <user>;

define method administrator? (user :: <blog-user>)
  element(user.additional-information, #"administrator?", default: #f);
end;
  
define method administrator?-setter (administrator? :: <boolean>, user :: <blog-user>)
  user.additional-information[#"administrator?"] := administrator?
end;

define object-tests (user) in turboblog end;

define action-tests
 (add-user, edit-user, remove-user, list-users)
in turboblog end;

define class <user-exists-error> (<error>)
  slot user :: <blog-user>,
    required-init-keyword: user:;
  slot existing-user :: <blog-user>,
    required-init-keyword: existing-user:;
end;

define method remove-user (blog-server :: <blog-server>, user :: <blog-user>)
 => (user :: <blog-user>);
  remove-key!(blog-server.users, user.username);
  user;
end;

define method modify-user (user :: <blog-user>, #key 
 username: user-username :: false-or(<string>), password: user-password :: false-or(<string>), 
 email: user-email :: false-or(<string>))
 => (user :: <blog-user>);
  if (user-username)
    remove-key!(*blog-server*.users, user.username);
    user.username := user-username;
    *blog-server*.users[user-username] := user;
  end if;
  user-password & (user.password := user-password);
  user-email & (user.email := user-email);
  user;
end;

define variable *edit-user-page* =
  make(<turboblog-page>, source: "edit-user.dsp");

define variable *view-user-page* =
  make(<turboblog-page>, source: "view-user.dsp");

define variable *view-users-page* =
  make(<turboblog-page>, source: "view-users.dsp");

define variable *remove-user-page* =
  make(<turboblog-page>, source: "remove-user.dsp");


define tag show-user-username in turboblog (page :: <turboblog-page>)
 ()
  if (*user*)
    output("%s", escape-xml(*user*.username));
  elseif (*form* & element(*form*, "username", default: #f))
    output("%s", escape-xml(*form*["username"]));
  end if;
end;

define tag show-user-email in turboblog (page :: <turboblog-page>)
 ()
  if (*user*)
    output("%s", escape-xml(*user*.email));
  elseif (*form* & element(*form*, "email", default: #f))
    output("%s", escape-xml(*form*["email"]));
  end if;
end;

define tag show-user-permanent-link in turboblog (page :: <turboblog-page>)
 ()
  if (*user*)
    output("%s", permanent-link(*user*));
  end if;
end;

define directory responder users-responder ("/users")
  let request = current-request();

  authenticate();

  let add? :: <boolean> = (get-query-value("add") = #t);
  let edit? :: <boolean> = (get-query-value("edit") = #t);
  let remove? :: <boolean> = (get-query-value("remove") = #t);
  let view? :: <boolean> = (request.request-url-tail ~= "");

  let action :: <symbol> = case
      add? => #"add-user";
      edit? => #"edit-user";
      remove? => #"remove-user";
      view? => #"view-user";
      otherwise => #"list-users";
    end case;

  let user = if (edit? | view? | remove?)
      find-user(request.request-url-tail);
    end if;

  if ((edit? | view? | remove?) & ~ user)
    respond-to(#"get", *non-existing-user-page*);
  else
    with-permission(action, user: user)
      dynamic-bind(*action* = action, *user* = user)
        select (request.request-method)
          #"get" => case
              add? | edit? => respond-to(#"get", *edit-user-page*);
              remove? => respond-to(#"get", *remove-user-page*);
              view? => respond-to(#"get", *view-user-page*);
              otherwise => respond-to(#"get", *view-users-page*);
            end case;
          #"post" => case
              add? | edit? => respond-to(#"post", *edit-user-page*);
              remove? => begin
                  remove-user(*blog-server*, user);
                  redirect-to("/users");
                end;
            end case;
        end select;
      end;
    end;
  end if;
end;

define method respond-to (type == #"post", page :: <edit-user-page>)
  let request = current-request();

// action
  let editing? :: <boolean> = (*action* = #"edit-user");
  let adding? :: <boolean> = (*action* = #"add-user");

// form
  let current-username = if (*user*) *user*.username end if;
  let username = get-query-value("username");
  let password = get-query-value("password");
  let email = get-query-value("email");

// errors
  let errors = #();

// checks
  if (~ instance?(username, <string>) | username = "")
    errors := add!(errors, #"username");
  else
    if (username ~= current-username & find-user(username))
      errors := add!(errors, #"user-exists-already");
    end if;
    username := if (username ~= current-username) username end if;
  end if;

  if (editing?)
    if (password = "")
      password := #f;
    end if;
  end if;

  if (adding? & (~ instance?(password, <string>) | password = ""))
    errors := add!(errors, #"password");
  end if;

  email := if (instance?(email, <string>)) email end if;

// security
  request.request-query-values["password"] := "";

// dispatch
  case
    ~ empty?(errors) =>
      dynamic-bind (*errors* = errors, *form* = request.request-query-values)
        next-method();
      end;
    editing? => begin
        modify-user(*user*, username: username, password: password, email: email);
        redirect-to(*user*);
      end;
    adding? => begin
        let user = make(<blog-user>, username: username, password: password, email: email);
        save(user);
        redirect-to(user);
      end;
  end case;
end;

