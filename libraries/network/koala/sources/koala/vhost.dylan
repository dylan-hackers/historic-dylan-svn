Module:    httpi
Synopsis:  Virtual hosts
Author:    Carl Gay
Copyright: Copyright (c) 2004 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Some methods to make logging slightly more convenient by not having
// to always pass log-target(*virtual-host*).
define method log-copious (format-string, #rest format-args)
  apply(%log-copious, *standard-output-log-target* | debug-log-target(*virtual-host*),
        format-string, format-args);
end;
define method log-verbose (format-string, #rest format-args)
  apply(%log-verbose, *standard-output-log-target* | debug-log-target(*virtual-host*),
        format-string, format-args);
end;
define method log-debug (format-string, #rest format-args)
  apply(%log-debug, *standard-output-log-target* | debug-log-target(*virtual-host*),
        format-string, format-args);
end;
define method log-info (format-string, #rest format-args)
  apply(%log-info, *standard-output-log-target* | debug-log-target(*virtual-host*),
        format-string, format-args);
end;
define method log-warning (format-string, #rest format-args)
  apply(%log-warning, *standard-output-log-target* | error-log-target(*virtual-host*),
        format-string, format-args);
end;
define method log-error (format-string, #rest format-args)
  apply(%log-error, *standard-output-log-target* | error-log-target(*virtual-host*),
        format-string, format-args);
end;


define class <directory-spec> (<object>)
  constant slot dirspec-pattern :: <string>,
    required-init-keyword: pattern:;

  constant slot directory-parent :: false-or(<directory-spec>),
    required-init-keyword: parent:;

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
    (spec :: <directory-spec>, #key pattern, #all-keys)
  // TODO:
  // This is temp code, depending on the fact that we only create specs ending
  // in '*' right now.
  let pos = char-position('*', pattern, 0, size(pattern));
  regular-expression(spec) := iff(pos, substring(pattern, 0, pos), pattern);
end;


define method dirspec-matches?
    (spec :: <directory-spec>, url :: <string>)
  // TODO: regular expressions.  For now if it's an initial substring match that'll do.
  looking-at?(regular-expression(spec), url, 0, size(url))
end;


// Most slots are set when the config file is processed.  A valiant attempt
// should be made to use good defaults, in case the config file doesn't specify
// a value.  Init args passed to make(<http-server-configuration>) are passed
// through to this class when making the default-virtual-host, for user convenience.

define class <virtual-host> (<object>)
  constant slot vhost-name :: <string>,
    required-init-keyword: name:;

  // The root of the web document hierarchy.  The config file loader may set this
  // to {server-root}/www/<vhost-name>/.  The default value is set in initialize.
  slot document-root :: <directory-locator>,
    init-keyword: document-root:;

  // TODO: no need for this here.  Even though ports can be specified inside
  //       the virtual host definition in the config file, we just need a 
  //       list of virtual hosts per port.  Start up one listener per port
  //       and serve requests only for the vhosts that are registered on that
  //       port.
  slot vhost-port :: <integer>,
    init-value: 80,
    init-keyword: port:;

  // List of <directory-spec> objects that determine how documents in
  // different directories are treated.  These are searched in order,
  // and the first one to match the requested URL is used.  Items are
  // pushed onto the beginning of this list as the config file is read,
  // so if two specs match the request URL then later specs will take
  // precedence.  I think this will match the natural usage, where people
  // will put more general specs first in the file and more specific ones
  // later, but we might want to revisit this decision.
  slot directory-specs :: <list>,
    init-value: list(),
    init-keyword: directory-specs:;

  // Each vhost gets an implicitly defined spec for the vhost root directory.
  // It must, of course, match all documents under the vhost document root.
  // It should always be the last element in directory-specs(vhost).
  // See initialize(<virtual-host>).
  constant slot root-directory-spec :: <directory-spec>,
    init-value: make(<directory-spec>,
                     parent: #f,
                     pattern: "/*"),
    init-keyword: root-directory-spec:;

  // Whether or not to include a Server: header in all responses.  Most people
  // won't care either way, but some might want to hide the server type so as
  // to prevent cracking or to hide the fact that they're not using one of the
  // Chosen Few accepted technologies.  Wimps.  ;-)
  slot generate-server-header? :: <boolean>,
    init-value: #t,
    init-keyword: generate-server-header?:;

  // TODO: this should be per-dirspec.  no reason some subtree on the same
  //       vhost shouldn't have a different set of default docs.  CFT.
  // The set of file names that are searched for when a directory URL is
  // requested.  They are searched in order, and the first match is chosen.
  slot default-documents :: <list>,
    init-value: list(as(<file-locator>, "index.html"),
                     as(<file-locator>, "index.htm")),
    init-keyword: default-documents:;

  // The value sent in the "Content-Type" header for static file responses if
  // no other value is set.  See *default-mime-type-map*.
  slot default-static-content-type :: <string>,
    init-value: "application/octet-stream",
    init-keyword: default-static-content-type:;

  // The value sent in the "Content-Type" header for dynamic responses if no
  // other value is set.
  slot default-dynamic-content-type :: <string>,
    init-value: "text/html; charset=utf-8",
    init-keyword: default-dynamic-content-type:;

  // Log targets.  If these are #f then the default virtual host's
  // log target is used.  They are never #f for the default-virtual-host.
  slot activity-log-target :: false-or(<log-target>) = #f,
    init-keyword: #"activity-log";
  slot error-log-target :: false-or(<log-target>) = #f,
    init-keyword: #"error-log";
  slot debug-log-target :: false-or(<log-target>) = #f,
    init-keyword: #"debug-log";

end class <virtual-host>;


define method initialize
    (vhost :: <virtual-host>,
     #key server-configuration :: false-or(<http-server-configuration>))
  next-method();
  if (~slot-initialized?(vhost, document-root))
    // Set the document root here because we need access to the vhost name.
    // This value is only used if no config file is loaded and no initial value
    // is passed to make since the config file loader always sets the document
    // root based on the config's server root.
    // todo -- Should have different default for Windows and unix and I'm not
    //         even sure this value is reasonable for unix.
    vhost.document-root := make(<directory-locator>,
                                path: vector("var", "www", vhost.vhost-name));
  end if;
  // Add a spec that matches all urls.
  add-directory-spec(vhost, vhost.root-directory-spec);
  // If server-configuration was supplied this isn't the default virtual host.
  if (server-configuration)
    // This may be overridden by a <document-root> spec in the config file.
    vhost.document-root
      := subdirectory-locator(server-configuration.server-root, vhost.vhost-name);
    vhost.activity-log-target
      := vhost.activity-log-target
         | server-configuration.default-virtual-host.activity-log-target;
    vhost.error-log-target
      := vhost.error-log-target
         | server-configuration.default-virtual-host.error-log-target;
    vhost.debug-log-target
      := vhost.debug-log-target
         | server-configuration.default-virtual-host.debug-log-target;
  end if;
end;

define method print-object
    (vhost :: <virtual-host>, stream :: <stream>)
 => ()
  format(stream, "{<virtual-host>: name: %=}", vhost.vhost-name);
end;

define method add-directory-spec
    (vhost :: <virtual-host>, spec :: <directory-spec>)
  directory-specs(vhost)
    := pair(spec, remove!(directory-specs(vhost), spec,
                          test: method (s1, s2)
                                  dirspec-pattern(s1) = dirspec-pattern(s2)
                                end));
end;

//// VIRTUAL HOST ACCESS

// The initial value here is never used; it's just there so this doesn't
// have to be typed as false-or(<virtual-host>).
//
define thread variable *virtual-host* :: false-or(<virtual-host>) = #f;

define method virtual-host
    (config :: <http-server-configuration>, name :: <string>)
 => (vhost :: false-or(<virtual-host>))
  element(config.virtual-hosts, name, default: #f)
end;

define method virtual-host
    (config :: <http-server-configuration>, request :: <request>)
 => (vhost :: false-or(<virtual-host>))
  let host-spec = request-host(request);
  local method die ()
          bad-request(message: format-to-string("Unknown virtual host: %s",
                                                host-spec));
        end;
  if (host-spec)
    let colon = char-position(':', host-spec, 0, size(host-spec));
    let host = iff(colon, substring(host-spec, 0, colon), host-spec);
    let port = colon &
                 block ()
                   string-to-integer(host-spec, start: colon + 1)
                 exception (ex :: <error>)
                   log-debug("error parsing port in host spec");
                   die();
                 end;
    let vhost = (virtual-host(config, host) | config.default-virtual-host);
    // TODO: If this is an HTTPS request and no port is specified, make sure
    //       vhost-port(vhost) == 443
    if (vhost & ((~port & vhost-port(vhost) == 80)
                   | port == vhost-port(vhost)))
      vhost
    else
      die()
    end
  else
    config.default-virtual-host | die()
  end
end;

define method virtual-host
    (config :: <http-server-configuration>, port :: <integer>)
 => (vhost :: false-or(<virtual-host>))
  block (return)
    for (vhost :: <virtual-host> keyed-by name in config.virtual-hosts)
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

