Module:    httpi
Synopsis:  Virtual hosts
Author:    Carl Gay
Copyright: Copyright (c) 2004 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define class <directory-spec> (<object>)
  constant slot dirspec-name :: <string>,
    required-init-keyword: name:;

  // TODO:
  // If this regular expression is the first to match the request URL
  // then this directory spec will be used.
  slot regular-expression :: <string>;        // will be <regular-expression>
  
  // Whether to allow directory listings.
  // May be overridden for specific directories.
  // Default is to be secure.
  slot allow-directory-listing? :: <boolean> = #f,
    init-keyword: allow-directory-listing?:;

  // Whether to allow serving documents that are outside of the document
  // root and are accessed via a symlink from within the document root.
  // Default is to be secure.
  slot follow-symlinks? :: <boolean> = #f,
    init-keyword: follow-symlinks?:;

  // TODO:
  //slot allow-cgi?, etc ...
  
end;


define method initialize
    (spec :: <directory-spec>, #key name, #all-keys)
  // TODO:
  // This is temp code, depending on the fact that we only create specs ending
  // in '*' right now.
  let pos = char-position('*', name, 0, size(name));
  regular-expression(spec) := iff(pos, substring(name, 0, pos), name);
end;


define method dirspec-matches?
    (spec :: <directory-spec>, url :: <string>)
  // TODO: regular expressions.  For now if it's an initial substring match that'll do.
  looking-at?(regular-expression(spec), url, 0, size(url))
end;


// Most slots are set when the config file is processed.  A valiant attempt
// should be made to use good defaults, in case the config file doesn't specify
// a value.

define class <virtual-host> (<object>)
  constant slot vhost-name :: <string>,
    required-init-keyword: name:;

  // The root of the web document hierarchy.  By default, this will be
  // *server-root*/www/<vhost-name>/.  If name is the empty string then
  // just *server-root*/www/.
  slot document-root :: <directory-locator>;

  // TODO: no need for this here.  Even though ports can be specified inside
  //       the virtual host definition in the config file, we just need a 
  //       list of virtual hosts per port.  Start up one listener per port
  //       and serve requests only for the vhosts that are registered on that
  //       port.
  slot vhost-port :: <integer> = 80;

  // List of <directory-spec> objects that determine how documents in
  // different directories are treated.  These are searched in order,
  // and the first one to match the requested URL is used.  Items are
  // pushed onto the beginning of this list as the config file is read,
  // so if two specs match the request URL then later specs will take
  // precedence.  I think this will match the natural usage, where people
  // will put more general specs first in the file and more specific ones
  // later, but we might want to revisit this decision.
  slot directory-specs :: <list>
    = list();

  // Each vhost gets an implicitly defined spec for the vhost root directory.
  // It must, of course, match all documents under the vhost document root.
  // It should always be the last element in directory-specs(vhost).
  // See initialize(<virtual-host>).
  constant slot root-directory-spec :: <directory-spec>
    = make(<directory-spec>,
           name: "/*");

  // Whether or not to include a Server: header in all responses.  Most people
  // won't care either way, but some might want to hide the server type so as
  // to prevent cracking or to hide the fact that they're not using one of the
  // Chosen Few accepted technologies.  Wimps.  ;-)
  slot generate-server-header? :: <boolean> = #t;

  // TODO: this should be per-dirspec.  no reason some subtree on the same
  //       vhost shouldn't have a different set of default docs.
  // The set of file names that are searched for when a directory URL is
  // requested.  They are searched in order, and the first match is chosen.
  slot default-documents :: <list>
    = list(as(<file-locator>, "index.html"),
           as(<file-locator>, "index.htm"));

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
  // TODO: x-platform: this should be a case-sensitive string table for 
  //       unix variants and insensitive for Windows.
  constant slot auto-register-map :: <table> = make(<string-table>);

end;


define method initialize
    (vhost :: <virtual-host>, #key name, #all-keys)
  next-method();
  // This may be overridden by a <document-root> spec in the config file.
  document-root(vhost) := subdirectory-locator(*server-root*, name);
  // Add a spec that matches all urls.
  add-directory-spec(vhost, root-directory-spec(vhost));
end;

define method add-directory-spec
    (vhost :: <virtual-host>, spec :: <directory-spec>)
  directory-specs(vhost)
    := pair(spec, remove!(directory-specs(vhost), spec,
                          test: method (s1, s2)
                                  dirspec-name(s1) = dirspec-name(s2)
                                end));
end;

// The vhost used if the request host doesn't match any other virtual host.
//
define constant $default-virtual-host :: <virtual-host>
  = make(<virtual-host>, name: "default");

define table $virtual-hosts :: <string-table>
  = {
      // These may be overwritten by config file entries.
      // The local host's real IP address will be added as a host name alias for
      // the default virtual host as well.
      "localhost" => $default-virtual-host,
      "127.0.0.1" => $default-virtual-host
      };

define method add-virtual-host
    (name :: <string>, vhost :: <virtual-host>)
  $virtual-hosts[name] := vhost;
end;

define method virtual-host
    (name :: <string>) => (vhost :: false-or(<virtual-host>))
  element($virtual-hosts, name, default: #f)
end;

define method virtual-host
    (request :: <request>) => (vhost :: false-or(<virtual-host>))
  let host-spec = request-host(request);
  if (host-spec)
    local method die ()
            bad-request(message: format-to-string("Unknown host: %s", host-spec));
          end;
    let colon = char-position(':', host-spec, 0, size(host-spec));
    let host = iff(colon, substring(host-spec, 0, colon), host-spec);
    let port = colon &
                 block ()
                   string-to-integer(host-spec, start: colon + 1)
                 exception (ex :: <error>)
                   log-debug("error parsing port in host spec");
                   die();
                 end;
    let vhost = virtual-host(host);
    log-debug("host = %=, port = %=, vhost = %=, vport = %=",
              host, port, vhost, vhost & vhost-port(vhost));
    log-debug("local host name = %=",
              host-name($local-host));
    iff(vhost & (~port | port == vhost-port(vhost)),
        vhost,
        die())
  else
    $default-virtual-host
  end
end;

define method virtual-host
    (port :: <integer>) => (vhost :: false-or(<virtual-host>))
  block (return)
    for (vhost :: <virtual-host> keyed-by name in $virtual-hosts)
      if (vhost-port(vhost) == port)
        return(vhost)
      end
    end
  end
end;

define method directory-spec-matching
    (vhost :: <virtual-host>, url :: <string>)
  iterate loop (specs :: <list> = directory-specs(vhost))
    if (empty?(specs))
      // The last spec is guaranteed to match all documents under the document root,
      // so if we get here it's an error.
      // TODO: improve the error message here, at least in the case where debugging
      //       is enabled.
      internal-server-error();
    else
      let spec :: <directory-spec> = head(specs);
      iff(dirspec-matches?(spec, url),
          spec,
          loop(tail(specs)));
    end;
  end;
end;
  

