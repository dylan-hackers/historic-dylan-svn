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

define tag show-name in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  format(output-stream(response), "%s", name(current-row() | *record*));
end;

define tag show-id in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  format(output-stream(response), "%s", record-id(current-row() | *record*));
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

define method note-form-message
    (message :: <string>, #rest args)
  apply(note-form-error, message, args)
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



// Called to validate each input field in an <edit-record-page> HTML form.
// Methods should throw <invalid-form-field-exception> if the input is invalid.
//
define generic validate-record-field (page :: <edit-record-page>,
                                      record :: <database-record>,
                                      slot :: <slot-descriptor>,
                                      value :: <object>);

define method validate-record-field (page :: <edit-record-page>,
                                     record :: <database-record>,
                                     slot :: <slot-descriptor>,
                                     input)
  // do nothing
end;




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

define method force-login (target-page :: <btrack-page>,
                           request :: <request>,
                           response :: <response>)
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

// Show the password for the current edit record only.
//
define tag show-password in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  let account = get-edit-record(get-request(response));
  let password = instance?(account, <account>) & password(account);
  password & write(output-stream(response), password);
end;

// ---TODO: Define a tag to replace the HTML <input> tag, that will automatically take
//          care of defaulting the value correctly if the form is redisplayed due to
//          error, and will display the input tag in a different background color.
//
define tag show-query-value
    (page :: <btrack-page>, response :: <response>)
    (name :: <string>)
  let qv = get-query-value(name);
  qv & write(output-stream(response), qv);
end;

////
//// Table row generators for list-*.dsp pages
////

//---TODO: It would be easy enough to have "define record" generate a mapping
//         from a pretty name like "account" to the record class <account>
//         and then repetitious code like below could be made generic instead.

define named-method version-generator in btrack
    (page :: <btrack-page>) => (rows :: <sequence>)
  load-all-records(<version>)
end;

define named-method product-generator in btrack
    (page :: <btrack-page>) => (rows :: <sequence>)
  load-all-records(<product>)
end;

define named-method module-generator in btrack
    (page :: <btrack-page>) => (rows :: <sequence>)
  load-all-records(<module>)
end;

define named-method browser-generator in btrack
    (page :: <btrack-page>) => (rows :: <sequence>)
  load-all-records(<browser>)
end;

define named-method platform-generator in btrack
    (page :: <btrack-page>) => (rows :: <sequence>)
  load-all-records(<platform>)
end;

define named-method operating-system-generator in btrack
    (page :: <btrack-page>) => (rows :: <sequence>)
  load-all-records(<operating-system>)
end;

define named-method bug-report-generator in btrack
    (page :: <btrack-page>) => (rows :: <sequence>)
  load-all-records(<bug-report>)
end;

define class <edit-record-page> (<btrack-page>)
end;

// This method is called under two different circumstances:
// (1) As the result of the user clicking a link of the form
//     <a href="edit-account.dsp&id=n">, in which case there is an 'id'
//     query parameter, or
// (2) As the result of a successful login.
// Therefore, if there is an 'id' query parameter it is used to find the
// record to edit.  If the user isn't logged in then that record is stored
// in the session and is redirected to the login page.  If there is no 'id'
// query parameter, the record to edit is already in the session.
//
// ---TODO: abstract this out to a mixin class, <require-login-mixin>
//
define method respond-to-get (page :: <edit-record-page>,
                              request :: <request>,
                              response :: <response>)
  let session = get-session(request);
  let record-id = get-query-value("id");
  let record-type = get-query-value("type");
  let record = select (record-id by \=)
                 "new" =>     // new record being created
                   let record-class = record-class-from-type-name(record-type);
                   make(record-class, id: next-record-id());
                 #f =>        // no record id means record is in session.
                   get-attribute(session, $edit-record-key);
                 otherwise =>
                   let record-class = record-class-from-type-name(record-type);
                   load-record(record-class, as(<integer>, record-id));
               end;
  record
    | error("No record was found for editing.  Please report this bug.");
  set-attribute(session, $edit-record-key, record);
  if (logged-in?(page, request))
    dynamic-bind (*record* = record)
      next-method();
    end;
  else
    force-login(page, request, response)
  end;
end;

define method respond-to-post (page :: <edit-record-page>,
                               request :: <request>,
                               response :: <response>)
  let record :: <database-record> = get-edit-record(request);
  let slots = slot-descriptors(object-class(record));
  let bindings = make(<string-table>); // maps form input name to parsed value
  let field-name = #f;
  let field-value = #f;
  block ()
    // Update the record with values from the page.
    for (slot in slots)
      field-name := slot-column-name(slot);
      field-value := get-form-value(field-name, as: slot-type(slot));
      if (field-value)
        validate-record-field(page, record, slot, field-value);
        let f = slot-setter(slot);
        // ---TODO: determine whether any slots changed.  If not, no need to save the record.
        f & f(field-value, record);
      else
        /* ---TODO
        if (slot-required?(slot))
          throw(<missing-required-field>, name: field-name)
        end;
        */
      end;
    end;
    save-record(record);
    // ---TODO: capitalize the pretty name.  add methods for capitalizing
    //          strings to the strings library.
    note-form-message("%s updated.", record-pretty-name(record));
    remove-attribute(get-session(request), $edit-record-key);     // clean up session
    // ---TODO: Figure out how to go to the 'origin' page.
    respond-to-get(*home-page*, request, response);
  exception (err :: <invalid-form-field-exception>)
    note-form-error("Error while processing the %= field.  %=",
                    field-name, err);
    next-method();
  end;
end;

define page list-browsers-page (<btrack-page>)
    (url: "/list-browsers.dsp",
     source: document-location("dsp/list-browsers.dsp"))
end;

define page edit-browser-page (<edit-record-page>)
    (url: "/edit-browser.dsp",
     source: document-location("dsp/edit-browser.dsp"))
end;

define page list-products-page (<btrack-page>)
    (url: "/list-products.dsp",
     source: document-location("dsp/list-products.dsp"))
end;

define page edit-product-page (<edit-record-page>)
    (url: "/edit-product.dsp",
     source: document-location("dsp/edit-product.dsp"))
end;

define page list-operating-systems-page (<btrack-page>)
    (url: "/list-operating-systems.dsp",
     source: document-location("dsp/list-operating-systems.dsp"))
end;

define page edit-operating-system-page (<edit-record-page>)
    (url: "/edit-operating-system.dsp",
     source: document-location("dsp/edit-operating-system.dsp"))
end;

define page list-platforms-page (<btrack-page>)
    (url: "/list-platforms.dsp",
     source: document-location("dsp/list-platforms.dsp"))
end;

define page edit-platform-page (<edit-record-page>)
    (url: "/edit-platform.dsp",
     source: document-location("dsp/edit-platform.dsp"))
end;

define page list-bugs-page (<btrack-page>)
    (url: "/list-bugs.dsp",
     source: document-location("dsp/list-bugs.dsp"))
end;

define page edit-bug-page (<edit-record-page>)
    (url: "/edit-bug.dsp",
     source: document-location("dsp/edit-bug.dsp"))
end;

define tag show-bug-number in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  let bug = current-row() | *record*;
  bug & write(output-stream(response), bug-number(bug));
end;


