Module:    internals
Synopsis:  Some globals that don't belong anywhere else in particular.
           Most are configurable in the config.xml file.
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Whether to show directory contents when a user enter the URI of a directory.
// For now it's boolean, but could be a list of directories to allow/disallow.
define variable *allow-directory-listings* = #t;

// Whether the server should run in debug mode or not.  If this is true then errors
// encountered while servicing HTTP requests will not be handled by the server itself.
// Normally the server will handle them and return an "internal server error" response.
// Setting this to true is the recommended way to debug your Dylan Server Pages.
define variable *debugging-server* :: <boolean> = #t;

// Whether or not to include a Server: header in all responses.  Most people won't
// care either way, but some might want to hide the server type so as to prevent
// cracking or to hide the fact that they're not using one of the Chosen Few accepted
// technologies.  Wimps.  ;-)
define variable *generate-server-header* :: <boolean> = #t;

// The root of the web document hierarchy.  By default, if the server is running in
// .../foo/bin/server.exe this will be set to .../foo/www/ and any files in that
// directory will be publically accessible.
define variable *document-root* :: false-or(<directory-locator>) = #f;

// The set of file names that are searched for when a directory URI is requested.
// They are searched in order, and the first is chosen.
define variable *default-document-names* :: <sequence>
  = #["index.html", "index.htm"];

// The value sent in the "Content-type" header for static file responses if no other
// value is set.  See *mime-type-map*.
define variable *default-static-content-type* :: <string> = "application/octet-stream";

// The value sent in the "Content-type" header for dynamic responses if no other value is
// set.
define variable *default-dynamic-content-type* :: <string> = "text/html";

define table *mime-type-map* = {
  #"au"    => "audio/basic",
  #"snd"   => "audio/basic",
  #"mid"   => "audio/midi",
  #"midi"  => "audio/midi",
  #"kar"   => "audio/midi",
  #"mpga"  => "audio/mpeg",
  #"mp2"   => "audio/mpeg",
  #"doc"   => "application/msword",
  #"bin"   => "application/octet-stream",
  #"exe"   => "application/octet-stream",
  #"class" => "application/octet-stream",
  #"ps"    => "application/postscript",
  #"ai"    => "application/postscript",
  #"eps"   => "application/postscript",
  #"ppt"   => "application/powerpoint",
  #"zip"   => "application/zip",
  #"pdf"   => "application/pdf",
  #"au"    => "audio/basic",
  #"snd"   => "audio/basic",
  #"mid"   => "audio/midi",
  #"midi"  => "audio/midi",
  #"kar"   => "audio/midi",
  #"mpga"  => "audio/mpeg",
  #"mp2"   => "audio/mpeg",
  #"gif"   => "image/gif",
  #"jpe"   => "image/jpeg",
  #"jpeg"  => "image/jpeg",
  #"jpg"   => "image/jpeg",
  #"png"   => "image/png",
  #"bat"   => "text/plain",
  #"ini"   => "text/plain",
  #"bat"   => "text/plain",
  #"bat"   => "text/plain",
  #"bat"   => "text/plain",
  #"txt"   => "text/plain",
  #"text"  => "text/plain",
  #"htm"   => "text/html",
  #"html"  => "text/html",
  #"xml"   => "text/xml",
  #"mpe"   => "video/mpeg",
  #"mpeg"  => "video/mpeg",
  #"mpg"   => "video/mpeg",
  #"qt"    => "video/quicktime",
  #"mov"   => "video/quicktime",
  #"avi"   => "video/x-msvideo",
  #"asf"   => "video/x-msvideo"  // a guess
};

// If this is set to true and certain URLs are requested, a page will be
// automatically registered for the URL.  See *auto-register-file-types*.
define variable *auto-register-pages?* :: <boolean> = #t;

// Maps from file extensions (e.g., "dsp") to functions that will register a URL
// responder for a URL.  If a URL matching the file extension is requested, and
// the URL isn't registered yet, then the function for the URL's file type extension
// will be called to register the URL and then the URL will be processed normally.
// This mechanism is used, for example, to automatically export .dsp URLs as Dylan
// Server Pages so that it's not necessary to have a "define page" form for every
// page in a DSP application.
define variable *auto-register-map* :: <string-table>
  = make(<string-table>);


