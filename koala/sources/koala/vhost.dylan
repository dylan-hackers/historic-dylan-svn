Module:    httpi
Synopsis:  Virtual hosts
Author:    Carl Gay
Copyright: Copyright (c) 2004 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Most slots are set when the config file is processed.  A valiant attempt
// should be made to use good defaults, in case the config file doesn't specify
// a value.

define class <virtual-host> (<object>)
  constant slot name :: <string>,
    required-init-keyword: name:;

  //slot port :: <integer> = 80;

  // The root of the web document hierarchy.  By default, this will be
  // *server-root*/<vhost-name>/www.
  slot document-root :: <directory-locator> = default-document-root();

  // TODO: x-platform: this should be a case-sensitive string table for 
  //       unix variants and insensitive for Windows.
  constant slot directory-specs :: <table> = make($filename-table-class);

  // Whether to allow directory listings.
  // May be overridden for specific directories.
  // Default is to be secure.
  slot allow-directory-listings? :: <boolean> = #f;

  // Whether the server should run in debug mode or not.  If this is true then
  // errors encountered while handling HTTP requests will not be handled by the
  // server itself.  Normally the server will handle them and return an
  // "internal server error" response.  Setting this to true is the recommended
  // way to debug your Dylan Server Pages.  See the comments on this in the
  // default config file.
  slot debugging-server? :: <boolean> = #t;

  // Whether or not to include a Server: header in all responses.  Most people
  // won't care either way, but some might want to hide the server type so as
  // to prevent cracking or to hide the fact that they're not using one of the
  // Chosen Few accepted technologies.  Wimps.  ;-)
  slot generate-server-header? :: <boolean> = #t;

  // The set of file names that are searched for when a directory URL is
  // requested.  They are searched in order, and the first match is chosen.
  slot default-document-names :: <sequence>
    = list("index.html", "index.htm");

  // The value sent in the "Content-Type" header for static file responses if
  // no other value is set.  See *mime-type-map*.
  slot default-static-content-type :: <string> = "application/octet-stream";

  // The value sent in the "Content-Type" header for dynamic responses if no
  // other value is set.
  slot default-dynamic-content-type :: <string> = "text/html";

  // This is the "master switch" for auto-registration of URLs.  If #f then
  // URLs will never be automatically registered based on their file types.  It
  // defaults to #f to be safe.  See auto-register-map
  slot auto-register-pages? :: <boolean> = #f;

  // Maps from file extensions (e.g., "dsp") to functions that will register a
  // URL responder for a URL.  If a URL matching the file extension is
  // requested, and the URL isn't registered yet, then the function for the
  // URL's file type extension will be called to register the URL and then the
  // URL will be processed normally.  This mechanism is used, for example, to
  // automatically export .dsp URLs as Dylan Server Pages so that it's not
  // necessary to have a "define page" form for every page in a DSP
  // application.
  constant slot auto-register-map :: <table> = make($filename-table-class);

end;


define method initialize (vhost :: <virtual-host>, #key, #all-keys)
  directory-specs(vhost)[as(<string>, document-root(vhost))] = ...;
end;
