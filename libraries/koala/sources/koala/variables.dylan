Module:    httpi
Synopsis:  Some globals that don't belong anywhere else in particular.
           Most are configurable in the koala-config.xml file.
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// The port on which to listen for HTTP requests.
define variable *server-port* :: <integer> = 80;

// Whether to show directory contents when a user requests a directory URL.
define variable *allow-directory-listings* = #t;

// Whether the server should run in debug mode or not.  If this is true then errors
// encountered while servicing HTTP requests will not be handled by the server itself.
// Normally the server will handle them and return an "internal server error" response.
// Setting this to true is the recommended way to debug your Dylan Server Pages.
define variable *debugging-server* :: <boolean> = #f;

// Whether or not to include a Server: header in all responses.  Most people won't
// care either way, but some might want to hide the server type so as to prevent
// cracking or to hide the fact that they're not using one of the Chosen Few accepted
// technologies.  Wimps.  ;-)
define variable *generate-server-header* :: <boolean> = #t;

// The top of the directory tree under which the server's
// configuration, error, and log files are kept.  Other pathnames
// are merged against this one, so if they're relative they will
// be relative to this.  The server-root pathname is relative to
// the koala executable.
define variable *server-root* :: false-or(<directory-locator>) = #f;

// The root of the web document hierarchy.  By default, this will be
// *server-root*/www.
define variable *document-root* :: false-or(<directory-locator>) = #f;

// The set of file names that are searched for when a directory URL is requested.
// They are searched in order, and the first is chosen.
define variable *default-document-names* :: <sequence>
  = #["index.html", "index.htm"];

// The value sent in the "Content-type" header for static file responses if no other
// value is set.  See *mime-type-map*.
define variable *default-static-content-type* :: <string> = "application/octet-stream";

// The value sent in the "Content-type" header for dynamic responses if no other value is
// set.
define variable *default-dynamic-content-type* :: <string> = "text/html";

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

define variable *mime-type-map* :: <table> = make(<table>);

