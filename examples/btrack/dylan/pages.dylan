Module: btrack
Author: Carl Gay
Synopsis:  Page and tag defs

begin
  // Start the Koala server.  Note that this sets configuration variables like
  // *document-root*, so it's called early.  Notably, document-location uses it.
  start-server();
end;


// When displaying a page that corresponds to a specific record, such as
// edit-account.dsp, this is bound to the record.
//
define thread variable *record* :: false-or(<database-record>) = #f;

// Key used to store the record that is currently being edited in the session.
//
define constant $edit-record-key = #"bt.edit-record";

define function get-edit-record
    (request :: <request>) => (record :: false-or(<database-record>))
  get-attribute(get-session(request), $edit-record-key)
end;

// Key used to store the target page into the session when temporarily redirecting
// to another page (e.g., to the login page).
//
define constant $target-page-key = #"bt.target-page";


define class <btrack-page> (<dylan-server-page>)
end;

define taglib btrack ()
end;

define page home-page (<btrack-page>)
    (url: "/home.dsp",
     source: document-location("dsp/home.dsp"))
end;

define page admin-page (<btrack-page>)
    (url: "/admin.dsp",
     source: document-location("dsp/admin.dsp"))
end;

define tag show-record-name in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  format(output-stream(response), "%s", name(current-row() | *record*));
end;

// This should always end in a slash.
//
define variable *image-directory* = "images/";

// Filename of the image displayed when no image name is specified.
// Relative to *image-directory*.
//
define variable *missing-image-name* = "missing-image.gif";

define tag show-image in btrack
    (page :: <btrack-page>, response :: <response>)
    (name)
  format(output-stream(response),
         "<img src='%s%s'>", *image-directory*, name | *missing-image-name*)
end;

define tag show-record-id in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  format(output-stream(response), "%d", record-id(current-row()));
end;  



// A simple error reporting mechanism.  Store errors in the page context
// so they can be displayed when the next page is generated.  The idea is
// that pages should use the <dsp:show-errors/> tag if they can be
// the target of a POST that might generate errors.

// ---TODO: Separate messages from errors and display them distinctly.

define method note-form-error
    (message :: <string>, #rest args)
  note-form-error(list(message, copy-sequence(args)));
end;

// This shows the use of <page-context> to store the form errors since they
// only need to be accessible during the processing of one page.
//
define method note-form-error
    (error :: <sequence>, #rest args)
  let context :: <page-context> = page-context();
  let errors = get-attribute(context, #"errors") | make(<stretchy-vector>);
  add!(errors, error);
  set-attribute(context, #"errors", errors);
end;

define tag show-messages in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  let errors = get-attribute(page-context(), #"errors");
  when (errors)
    let out = output-stream(response);
    format(out, "<FONT color='red'>Please fix the following errors:<P>\n<UL>\n");
    for (err in errors)
      // this is pretty consy
      format(out, "<LI>%s\n",
             apply(format-to-string, first(err), second(err)));
    end;
    format(out, "</UL></FONT>\n");
  end;
end;



// Called to validate each input field in a web form.  Methods should throw
// <invalid-form-field-exception> if the input is invalid.
//
define generic validate-form-field
    (page :: <btrack-page>, slot :: <slot-descriptor>, value :: <object>);


// ---TODO:
// Eventually these should be distinguished from other form errors so that
// they can be displayed differently.  e.g., by highlighting the cell containing
// the input field that got an error.
//
define method note-field-error
    (field-name :: <string>, msg :: <string>, #rest args)
  apply(note-form-error, concatenate(field-name, ": ", msg), args);

  // ---TODO: This shouldn't really signal an error since eventually it should
  //          validate all form fields and report all problems at once.
  signal(make(<invalid-form-field-exception>,
              format-string: "Invalid form field."));
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

define constant $current-account-key = #"bt.current-account";

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

define method force-login
    (target-page :: <btrack-page>, request :: <request>, response :: <response>)
  // Save the target page away so the login-page knows where to go when it's done.
  set-attribute(get-session(request), $target-page-key, target-page);
  respond-to-get(*login-page*, request, response);
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
      respond-to-get(get-attribute(session, $target-page-key) | *home-page*,
                     request,
                     response);
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

