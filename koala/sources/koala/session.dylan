Module:    httpi
Author:    Carl Gay
Synopsis:  HTTP sessions
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Maps session-id to session object.
define variable *sessions* :: <table> = make(<table>);

// The number of seconds this cookie should be stored in the user agent, in seconds.
// #f means no max-age is transmitted, which means "until the user agent exits".
define variable *session-max-age* :: false-or(<integer>) = #f;

// API
// This doesn't affect any cookies set previously.
define method set-session-max-age
    (age :: false-or(<integer>))
  *session-max-age* := age
end;

define open primary class <session> (<attributes-mixin>)
  constant slot session-id :: <integer>, required-init-keyword: #"id";
  // ---TODO: 
  // cookie
  // 
end;

define method initialize
    (session :: <session>, #key id :: <integer>, #all-keys)
  next-method();
  if (element(*sessions*, id, default: #f))
    error("Attempt to create a session with a duplicate session id, %d", id);
  else
    *sessions*[id] := session;
  end;
end;

// ---TODO: Should probably vary the session id in a true random way.
//          And make it persistent across server reboots.

define function next-session-id
    () => (id :: <integer>)
  let id = random($maximum-integer);
  if (element(*sessions*, id, default: #f))
    next-session-id();
  end if;
  id;
end;

// API
// This is the only way for user code to get the session object.
define method get-session
    (request :: <request>) => (session :: false-or(<session>))
  request.request-session
  | (request.request-session := current-session(request))
end;

define method ensure-session
    (request :: <request>) => (session :: <session>)
  let session = get-session(request);
  unless (session)
    session := new-session(request);
  end unless;
  session;
end;

define method clear-session
    (request :: <request>) => ();
  let session = get-session(request);
  if (session)
    remove-key!(*sessions*, session.session-id);
    request.request-session := #f;
    add-cookie(*response*, "koala_session_id", -1,
               max-age: *session-max-age*,
               path: "/",
               // domain: ??? ---TODO
               comment: "This cookie assigns a unique number to your browser "
                 "so that we can remember who you are as you move from page "
                 "to page within our site.");
  end if;
end;

define method current-session
    (request :: <request>) => (session :: false-or(<session>))
  let cookies = request-header-value(request, #"cookie");
  when (cookies)
    block (return)
      for (cookie in cookies)
        when (cookie-name(cookie) = "koala_session_id")
          let session-id = string-to-integer(cookie-value(cookie));
          return(element(*sessions*, session-id, default: #f));
        end;
      end;
    end;
  end;
end;

define method new-session
    (request :: <request>) => (session :: <session>)
  let id = next-session-id();
  add-cookie(*response*, "koala_session_id", id,
             max-age: *session-max-age*,
             path: "/",
             // domain: ??? ---TODO
             comment: "This cookie assigns a unique number to your browser so "
               "that we can remember who you are as you move from page "
               "to page within our site.");
  let session = make(<session>, id: id);
  request.request-session := session
end;
