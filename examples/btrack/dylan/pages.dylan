Module: btrack
Author: Carl Gay
Synopsis:  Page and tag defs

begin
  // Start the Koala server.  Note that this sets configuration variables like
  // *document-root*, so it's called early.  Notably, document-location uses it.
  start-server();
end;


define taglib btrack ()
end;

define class <btrack-page> (<dylan-server-page>)
end;

define class <require-login-mixin> (<object>)
end;

define class <btrack-record-page> (<require-login-mixin>, <edit-record-page>, <btrack-page>)
end;

define page home-page (<btrack-page>)
    (url: "/home.dsp",
     source: document-location("dsp/home.dsp"))
end;

define page admin-page (<btrack-page>)
    (url: "/admin.dsp",
     source: document-location("dsp/admin.dsp"))
end;

define page list-browsers-page (<btrack-page>)
    (url: "/list-browsers.dsp",
     source: document-location("dsp/list-browsers.dsp"))
end;

define page edit-browser-page (<btrack-record-page>)
    (url: "/edit-browser.dsp",
     source: document-location("dsp/edit-browser.dsp"))
end;

define page list-products-page (<btrack-page>)
    (url: "/list-products.dsp",
     source: document-location("dsp/list-products.dsp"))
end;

define page edit-product-page (<btrack-record-page>)
    (url: "/edit-product.dsp",
     source: document-location("dsp/edit-product.dsp"))
end;

define page edit-module-page (<btrack-record-page>)
    (url: "/edit-module.dsp",
     source: document-location("dsp/edit-module.dsp"))
end;

define page edit-version-page (<btrack-record-page>)
    (url: "/edit-version.dsp",
     source: document-location("dsp/edit-version.dsp"))
end;

define method display-hidden-fields
    (page :: type-union(<edit-module-page>, <edit-version-page>), stream :: <stream>)
  display-hidden-field(stream, "product_id", get-query-value("product_id"))
end;

define page list-operating-systems-page (<btrack-page>)
    (url: "/list-operating-systems.dsp",
     source: document-location("dsp/list-operating-systems.dsp"))
end;

define page edit-operating-system-page (<btrack-record-page>)
    (url: "/edit-operating-system.dsp",
     source: document-location("dsp/edit-operating-system.dsp"))
end;

define page list-platforms-page (<btrack-page>)
    (url: "/list-platforms.dsp",
     source: document-location("dsp/list-platforms.dsp"))
end;

define page edit-platform-page (<btrack-record-page>)
    (url: "/edit-platform.dsp",
     source: document-location("dsp/edit-platform.dsp"))
end;

define page list-bugs-page (<btrack-page>)
    (url: "/list-bugs.dsp",
     source: document-location("dsp/list-bugs.dsp"))
end;

define page edit-bug-page (<btrack-record-page>)
    (url: "/edit-bug.dsp",
     source: document-location("dsp/edit-bug.dsp"))
end;

define tag show-name in btrack
    (page :: <btrack-page>, response :: <response>)
    (key)
  let record = select (key by \=)
                 "row"     => current-row();
                 "record"  => *record*;
                 otherwise => current-row() | *record*;
               end;
  format(output-stream(response), "%s", name(record));
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

define tag show-owner in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  format(output-stream(response), "%s", name(owner(current-row() | *record*)));
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

// Key used to store the target page into the session when temporarily redirecting
// to another page (e.g., to the login page).
//
define constant $target-page-key = #"bt.target-page";


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
  debug-message("respond-to-post <login-page>");
  let username = get-form-value("username");
  let password = get-form-value("password");
  let username-supplied? = username & username ~= "";
  let password-supplied? = password & password ~= "";
  if (username-supplied? & password-supplied?)
    debug-message("respond-to-post: got uname and pwd");
    let account = load-account-named(username, password: password);
    if (account)
      let session = get-session(request);
      set-attribute(session, $current-account-key, account);
      debug-message("calling respond-to-get on %=",
                    get-attribute(session, $target-page-key) | *home-page*);
      respond-to-get(get-attribute(session, $target-page-key) | *home-page*,
                     request,
                     response);
    else
      note-form-error("Login incorrect.  Please try again.");
      debug-message("calling respond-to-get <login-page>");
      respond-to-get(*login-page*, request, response);
    end;
  else
    note-form-error("You must supply <b>both</b> a username and password.");
    // ---*** TODO: Calling respond-to-get probably isn't quite right.
    // If we're redirecting to another page should the query/form values
    // be cleared first?  Probably want to call process-page instead,
    // but with the existing request?
    debug-message("calling respond-to-get <login-page>");
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
  note-form-message("You have been logged out.");
  respond-to-get(*home-page*, request, response);
end;

define method respond-to-get-edit-record (page :: <require-login-mixin>,
                                          request :: <request>,
                                          response :: <response>,
                                          record :: <database-record>)
  iff(logged-in?(page, request),
      next-method(),
      force-login(page, request, response));
end;

define tag show-login-or-logout in btrack
    (page, response) ()
  let out = output-stream(response);
  format(out, iff(logged-in?(page, get-request(response)),
                  "<a href='logout.dsp'>Logout</a>",
                  "<a href='login.dsp'>Login</a>"));
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

////
//// Table row generators for list-*.dsp pages
////

//---TODO: Can probably make this code a lot less repetitious by passing the
//         record type in the dsp:table tag's attributes and retrieving it
//         here.

// Generate a sequence of all versions related to the current product.
//
define named-method gen-product-versions in btrack
    (page :: <edit-product-page>) => (rows :: <sequence>)
  load-records(<version>,
               sformat("select * from tbl_version where product_id = %d",
                       record-id(*record*)));
end;

// Generate a sequence of all modules related to the current product.
//
define named-method gen-product-modules in btrack
    (page :: <edit-product-page>) => (rows :: <sequence>)
  load-records(<module>,
               sformat("select * from tbl_module where product_id = %d",
                       record-id(*record*)));
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

define tag show-bug-number in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  let bug = current-row() | *record*;
  bug & format(output-stream(response), "%d", bug-number(bug));
end;

define tag show-synopsis in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  let bug = current-row() | *record*;
  bug & write(output-stream(response), synopsis(bug));
end;

define tag show-description in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  let record = current-row() | *record*;
  record & write(output-stream(response), description(record));
end;

define tag show-priority in btrack
    (page, response)
    ()
  format(output-stream(response), "%d", priority(current-row()));
end;
define tag show-severity in btrack
    (page, response)
    ()
  format(output-stream(response), "%d", severity(current-row()));
end;
define tag show-product in btrack
    (page, response)
    ()
  format(output-stream(response), "%s", name(product(current-row())));
end;
define tag show-module in btrack
    (page, response)
    ()
  format(output-stream(response), "%s", name(module(current-row())));
end;
define tag show-reported-by in btrack
    (page, response)
    ()
  format(output-stream(response), "%s", name(reported-by(current-row())));
end;


// ---TODO: Move option menus into DSP?

define macro option-menu-definer
  { define option-menu ?:name ?menu:expression; ?accessor:expression; end }
  => {  define tag ?name in btrack
            (page :: <btrack-page>, response :: <response>)
            ()
          let record = current-row() | *record*;
          let out = output-stream(response);
          when (record)
            display(out, ?menu, ?accessor(record));
          end;
        end;
      }
end;

//---TODO: I think in the vast majority of cases we'll need a value-generator and
//         then just a function to generate a name from the value.  Should update
//         the <option-menu> class for that.

define option-menu show-priority-options 
    make(<option-menu>,
         generator: method () range(from: 0, to: 5) end);
    priority;
end;
define option-menu show-severity-options 
    make(<option-menu>,
         generator: method () range(from: 0, to: 5) end);
    severity;
end;
define option-menu show-version-options
    btrack-option-menu(curry(load-all-records, <version>));
    version;
end;
define option-menu show-operating-system-options 
    btrack-option-menu(curry(load-all-records, <operating-system>));
    operating-system;
end;
define option-menu show-platform-options
    btrack-option-menu(curry(load-all-records, <platform>));
    platform;
end;
define option-menu show-browser-options 
    btrack-option-menu(curry(load-all-records, <browser>));
    browser;
end;
define option-menu show-product-options 
    btrack-option-menu(curry(load-all-records, <product>));
    product;
end;
define option-menu show-module-options
    btrack-option-menu(curry(load-all-records, <module>));
    module;
end;

define option-menu show-dev-assigned-options
    btrack-option-menu(curry(load-all-records, <account>));
    dev-assigned;
end;
define option-menu show-qa-assigned-options
    btrack-option-menu(curry(load-all-records, <account>));
    qa-assigned;
end;
define option-menu show-status-options
    make(<option-menu>,
         generator: method () range(from: 0, to: 5) end);
    status;
end;


define method load-account
    (id :: <integer>) => (account :: false-or(<account>))
  load-record(<account>, id)
end;

define method load-account
    (query :: <string>) => (account :: false-or(<account>))
  load-record(<account>, query)
end;

define method load-account-named
    (name :: <string>, #key password) => (account :: false-or(<account>))
  assert(size(name) > 0,
         "Attempt to load an account by name using the empty string as the name.");
  let query
    = iff(password,
          format-to-string("select * from tbl_account where name = '%s' and password = '%s'",
                           name, password),
          format-to-string("select * from tbl_account where name = '%s'",
                           name));
  load-record(<account>, query)
end;


define page list-accounts-page (<btrack-page>)
    (url: "/list-accounts.dsp",
     source: document-location("dsp/list-accounts.dsp"))
end;

define page edit-account-page (<btrack-record-page>)
    (url: "/edit-account.dsp",
     source: document-location("dsp/edit-account.dsp"))
end;

define method validate-record-field
    (page :: <edit-account-page>, account :: <account>, slot :: <slot-descriptor>, input)
  let column-name = slot-column-name(slot);
  select (column-name by \=)
    "password", "username" =>
      input.size < 4
        & note-field-error(column-name, "Must be at least 4 characters long.");
    otherwise => ;
  end;
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

define tag show-current-username in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  let account = current-account(response);
  let out = output-stream(response);
  iff(account,
      format(out, "Logged in as %s", name(account)),
      format(out, "(not logged in)"));
end;


