Module:    httpi
Synopsis:  Some globals that don't belong anywhere else in particular.
           Most are configurable in the koala-config.xml file.
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Whether the server should run in debug mode or not.  If this is true then
// errors encountered while servicing HTTP requests will not be handled by the
// server itself.  Normally the server will handle them and return an "internal
// server error" response.  Setting this to true is the recommended way to
// debug your Dylan Server Pages.
define variable *debugging-server* :: <boolean> = #f;

// The top of the directory tree under which the server's configuration, error,
// and log files are kept.  Other pathnames are merged against this one, so if
// they're relative they will be relative to this.  The server-root pathname is
// relative to the koala executable, unless changed in the config file.
define variable *server-root* :: false-or(<directory-locator>) = #f;

define function ensure-server-root ()
  when (~*server-root*)
    let exe-dir = locator-directory(as(<file-locator>, application-filename()));
    *server-root* := parent-directory(exe-dir);
  end;
end;

define function init-server-root (#key location)
  ensure-server-root();
  when (location)
    *server-root* := merge-locators(as(<directory-locator>, location),
                                    *server-root*);
  end;
end;


// TODO: The follow 3 should probably be per vhost.

define variable *mime-type-map* :: <table> = make(<table>);

// This is the "master switch" for auto-registration of URLs.  If #f then URLs will
// never be automatically registered based on their file types.  It defaults to #f
// to be safe.
// @see *auto-register-map*
define variable *auto-register-pages?* :: <boolean> = #f;

// Maps from file extensions (e.g., "dsp") to functions that will register a URL
// responder for a URL.  If a URL matching the file extension is requested, and
// the URL isn't registered yet, then the function for the URL's file type extension
// will be called to register the URL and then the URL will be processed normally.
// This mechanism is used, for example, to automatically export .dsp URLs as Dylan
// Server Pages so that it's not necessary to have a "define page" form for every
// page in a DSP application.
define variable *auto-register-map* :: <string-table>
  = make(<string-table>);

// This is #t when the server is listening for requests and #f otherwise.
define variable *server-running?* :: <boolean> = #f;

// Since logging is done on a per-vhost basis, this hack is needed
// to make logging work before vhosts are initialized.
define variable *temp-log-target*
  = make(<stream-log-target>, stream: *standard-output*);
