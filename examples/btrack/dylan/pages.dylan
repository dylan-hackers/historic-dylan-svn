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

// Key used to store *record* in the session.
//
define constant $record-key = #"record";

// Key used to store the target page into the session when temporarily redirecting
// to another page (e.g., to the login page).
//
define constant $target-page-key = #"target-page";


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


