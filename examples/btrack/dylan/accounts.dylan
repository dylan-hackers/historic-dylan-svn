Module: btrack
Author: Carl Gay
Synopsis:  account-related pages and tags (including login/logout)

define page list-accounts-page (<btrack-page>)
    (url: "/list-accounts.dsp",
     source: document-location("dsp/list-accounts.dsp"))
end;

define page edit-account-page (<btrack-page>)
    (url: "/edit-account.dsp",
     source: document-location("dsp/edit-account.dsp"))
end;

// ---TODO: abstract this out to a mixin class, <require-login-mixin>
//
define method respond-to-get (page :: <edit-account-page>,
                              request :: <request>,
                              response :: <response>)
  let session = get-session(request);
  if (logged-in?(page, request))
    let account = load-account(get-query-value("id", as: <integer>));
    dynamic-bind (*record* = account)
      next-method();  // Process the DSP template for this page.
    end;
  else
    set-attribute(session, $target-page-key, *edit-account-page*);
    respond-to-get(*login-page*, request, response);
  end;
end;

define method respond-to-post (page :: <edit-account-page>,
                               request :: <request>,
                               response :: <response>)
  note-form-error("Saving an account is not implemented yet.");
  next-method();
end;

define named-method list-accounts in btrack
    (page)
  load-records(<account>, "select * from tbl_account")
end;

define tag show-email-address in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  format(output-stream(response), "%s", email-address(current-row() | *record*));
end;



////
//// Login/logout
////

// The basic login mechanism is this:  If a page is requested that requires
// login, the user is redirected to the login page and a bit is set somewhere
// (undetermined at the time of this writing) to say what page was being 
// requested.  The user enters login/password and the from is posted to the
// <login-page>.  If login is successful, the user is redirected to the original
// page they requested.

define constant $current-account-key = #"current-account";

define method current-account
    (session :: <session>) => (account :: false-or(<account>))
  get-attribute(session, $current-account-key)
end;

define method current-account
    (request :: <request>) => (account :: false-or(<account>))
  current-account(get-session(request))
end;

define method current-account
    (response :: <response>) => (account :: false-or(<account>))
  current-account(get-request(response))
end;

define named-method logged-in? in btrack
    (page, request) => (b :: <boolean>)
  current-account(request) & #t
end;

define page login-page (<btrack-page>)
    (url: "/login.dsp",
     source: document-location("dsp/login.dsp"))
end;

// This is called when the login page is submitted.
//
define method respond-to-post (page :: <login-page>,
                               request :: <request>,
                               response :: <response>)
  let username = get-form-value("username");
  let password = get-form-value("password");
  let username-supplied? = username & username ~= "";
  let password-supplied? = password & password ~= "";
  if (username-supplied? & password-supplied?)
    let account = load-account-named(username, password: password);
    if (account)
      let session = get-session(request);
      set-attribute(session, $current-account-key, account);
      dynamic-bind (*record* = account)
        respond-to-get(get-attribute(session, $target-page-key) | *home-page*,
                       request, response);
      end;
    else
      note-form-error("Login incorrect.  Please try again.");
      respond-to-get(*login-page*, request, response);
    end;
  else
    note-form-error("You must supply <b>both</b> a username and password.");
    // ---*** TODO: Calling respond-to-get probably isn't quite right.
    // If we're redirecting to another page should the query/form values
    // be cleared first?  Probably want to call process-page instead,
    // but with the existing request?
    respond-to-get(*login-page*, request, response);
  end;
end;

define page logout-page (<btrack-page>)
    (url: "/logout.dsp",
     source: document-location("dsp/logout.dsp"))
end;

define method respond-to-get (page :: <logout-page>,
                              request :: <request>,
                              response :: <response>)
  let session = get-session(request);
  remove-attribute(session, $current-account-key);
  note-form-error("You are logged out.");
  respond-to-get(*home-page*, request, response);
end;

define tag show-username in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  let account = current-account(get-request(response));
  let username = account & name(account);
  username & write(output-stream(response), username);
end;

define tag show-password in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  let account = current-account(get-request(response));
  let password = account & password(account);
  password & write(output-stream(response),
                   make(<string>,
                        size: size(password),
                        initial-element: "*"));
end;


