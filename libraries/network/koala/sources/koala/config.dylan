Module:    httpi
Synopsis:  Parse config file and create server configuration object.
Author:    Carl Gay
Copyright: Copyright (c) 2001-2004 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define constant $config-directory-name :: <string> = "conf";
define constant $log-directory-name :: <string> = "logs";
define constant $document-directory-name :: <string> = "www";

// This class holds all information that is configurable in a given Koala
// <http-server> instance.
//
define class <http-server-configuration> (<object>)

  // Whether the server should run in debug mode or not.  If this is
  // true then errors encountered while servicing HTTP requests will not
  // be handled by the server itself.  Normally the server will handle
  // them and return an "internal server error" response.  A good way to
  // debug Dylan Server Pages.  Can be enabled via the --debug
  // command-line option.
  slot debugging-enabled? :: <boolean> = #f,
    init-keyword: #"debug?";

  // Map from URL string to a response function.  The leading slash is removed
  // from URLs because it's easier to use merge-locators that way.
  // TODO: this should be per vhost
  //       then 'define page' needs to specify vhost until dynamic
  //       library loading works.  (ick.)  once dynamic library loading
  //       works we use <module foo> inside <virtual-host> in the config
  //       and bind *virtual-host* while the library is loading?

  constant slot url-map :: <string-trie> = make(<string-trie>, object: #f);

  // The top of the directory tree under which the server's
  // configuration, error, and log files are kept.  Other pathnames are
  // merged against this one, so if they're relative they will be
  // relative to this.  The server-root pathname is  relative to the
  // current working directory, unless changed in the config file.
  slot server-root :: <directory-locator> = as(<file-locator>, "."),
    init-keyword: #"server-root";

  constant slot mime-type-map :: <table> = make(<table>);

  // This is the "master switch" for auto-registration of URLs.  If #f
  // then URLs will never be automatically registered based on their
  // file types.  It defaults to #f to be safe.
  // @see auto-register-map
  slot auto-register-pages? :: <boolean> = #f,
    init-keyword: #"auto-register-pages?";

  // Maps from file extensions (e.g., "dsp") to functions that will
  // register a URL responder for a URL.  If a URL matching the file
  // extension is requested, and the URL isn't registered yet, then the
  // function for the URL's file type extension will be called to
  // register the URL and then the URL will be processed normally.  This
  // mechanism is used, for example, to automatically export .dsp URLs
  // as Dylan Server Pages so that it's not necessary to have a "define
  // page" form for every page in a DSP application.
  constant slot auto-register-map :: <string-table> = make(<string-table>);

  // The vhost used if the request host doesn't match any other virtual host.
  // Note that the document root may be changed when the config file is
  // processed, so don't use it except during request processing.
  constant slot default-virtual-host :: <virtual-host>
    = begin
        let stdout-log = make(<stream-log-target>, stream: *standard-output*);
        make(<virtual-host>,
             name: "default",
             activity-log: stdout-log,
             debug-log: stdout-log,
             error-log: make(<stream-log-target>, stream: *standard-error*))
      end;

  // If this is true, then requests directed at hosts that don't match any
  // explicitly named virtual host (i.e., something created with <virtual-host>
  // in the config file) will use the default vhost.  If this is #f when such a
  // request is received, a Bad Request (400) response will be returned.
  slot fall-back-to-default-virtual-host? :: <boolean> = #t;

  // Maps host names to virtual hosts.  Any host name not found in this
  // table maps to default-virtual-host (contingent on the value of
  // fall-back-to-default-virtual-host?).
  constant slot virtual-hosts :: <string-table> = make(<string-table>);

  // Since logging is done on a per-vhost basis, this hack is needed
  // to make logging work before vhosts are initialized.
  constant slot temp-log-target :: <log-target>
    = make(<stream-log-target>, stream: *standard-output*);

  // This may be set true by config file loading code, in which case
  // start-server will abort startup.
  slot abort-startup? :: <boolean> = #f;

end class <http-server-configuration>;


define method get-virtual-host
    (config :: <http-server-configuration>, vhost-name :: <string>)
 => (vhost :: false-or(<virtual-host>))
  element(config.virtual-hosts, vhost-name, default: #f)
end;

// We want the config file values to override the slot default values and the
// "make" keyword arguments to override the config file values.  To accomplish
// this, the config file parsing builds up a set of init arguments which are
// appended to the make args.
//
define table *init-keyword-to-setter-map* = {
    #"debug?" => debugging-enabled?-setter,
    #"server-root" => server-root-setter,
    #"auto-register-pages?" => auto-register-pages?-setter
  };

define method initialize
    (config :: <http-server-configuration>, #rest args, #key config-file)
  next-method();
  // Config file overrides default slot values.
  // Then init keywords override config file values.
  if (config-file)
    load-configuration-file(config, config-file);
    // Now override config file settings with init-args again.
    // Is there a better way?
    local method argument-value (init-keyword)
            block (return)
              for (i from 0 by 2, while: i < args.size - 1)
                if (args[i] == init-keyword)
                  return(args[i + 1]);
                end if;
              end for;
            end block;
          end method argument-value;
    for (setter keyed-by init-keyword in *init-keyword-to-setter-map*)
      setter(argument-value(init-keyword), config);
    end for;
  end if;
end method initialize;

//// CONFIG FILE PROCESSING



// Some variables for use during config file processing.
define thread variable *config* = #f;
define thread variable *vhost* = #f;
define thread variable *directory* = #f;
define thread variable *config-file* = #f;

// todo -- Should warn when unrecognized attributes are used.
//         Makes debugging your config file much easier sometimes.

define method load-configuration-file
    (config :: <http-server-configuration>, filename :: <string>)
  block (return)
    local method error-handler (c :: <error>, next-handler :: <function>)
            if (debugging-enabled?(config))
              next-handler();  // decline to handle the error
            else
              log-error("Error loading config file: %=", c);
              return();
            end;
          end method;
    let handler <error> = error-handler;
    let file = as(<file-locator>, filename);
    log-info("Loading server configuration from %s.", filename);
    let text = file-contents(file);
    if (text)
      let xml :: xml$<document> = xml$parse-document(text, print-warnings?: #t);
      let vhost = default-virtual-host(config);
      dynamic-bind (*config* = config,
                    *config-file* = file,
                    *vhost* = vhost,
                    *directory* = root-directory-spec(vhost))
        process-config-node(xml);
      end;
    else
      log-error("Server configuration file (%s) not found.", filename);
      config.abort-startup? := #t;
    end if;
  end block;
end method load-configuration-file;

define function warn
    (format-string, #rest format-args)
  log-warning("%s: %s",
              as(<string>, *config-file*),
              apply(format-to-string, format-string, format-args));
end;

// Exported
// The xml-parser library doesn't seem to define anything like this.
define method get-attr
    (node :: xml$<element>, attrib :: <symbol>)
 => (value :: false-or(<string>))
  block (return)
    for (attr in xml$attributes(node))
      when (xml$name(attr) = attrib)
        return(xml$attribute-value(attr));
      end;
    end;
  end
end;

// I think the XML parser's class hierarchy is broken.  It seems <tag>
// should inherit from <node-mixin> so that one can descend the node
// hierarchy seemlessly.
define method process-config-node (node :: xml$<tag>) => ()
end;

define method process-config-node (node :: xml$<document>) => ()
  for (child in xml$node-children(node))
    process-config-node(child);
  end;
end;

define method process-config-node (node :: xml$<element>) => ()
  process-config-element(node, xml$name(node));
end;

// Exported.
// Libraries may specialize this.
// Note that the previous comment about the XML parser's class hierarchy
// applies here as well.  Otherwise this would specialize node more tightly.
//
define open generic process-config-element
    (node :: <object>, name :: <object>);

define method process-config-element
    (node :: xml$<element>, name :: <object>)
  warn("Unrecognized configuration setting: %=.  Processing child nodes anyway.",
       name);
  for (child in xml$node-children(node))
    process-config-node(child);
  end;
end;

define method process-config-element
    (node :: xml$<comment>, name :: <object>)
end;

define function true-value?
    (val :: <string>) => (true? :: <boolean>)
  member?(val, #("yes", "true", "on"), test: string-equal?)
end;



//// koala-config.xml elements.  One method for each element name.

define method process-config-element
    (node :: xml$<element>, name == #"koala")
  for (child in xml$node-children(node))
    process-config-node(child);
  end;
end method process-config-element;


define method process-config-element
    (node :: xml$<element>, name == #"virtual-host")
  let name = get-attr(node, #"name");
  if (name)
    let name = trim(name);
    if (get-virtual-host(*config*, name))
      warn("Ignoring duplicate virtual host %=", name);
    else
      let vhost = make(<virtual-host>, name: name);
      *config*.virtual-hosts[name] := vhost;
      dynamic-bind(*vhost* = vhost)
        for (child in xml$node-children(node))
          process-config-element(child, xml$name(child))
        end;
      end;
    end;
  else
    warn("Invalid <virtual-host> spec.  "
         "The 'name' attribute must be specified.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"alias")
  let name = get-attr(node, #"name");
  if (name)
    let name = trim(name);
    if (get-virtual-host(*config*, name))
      warn("There is already a virtual host named %s.  "
           "Ignoring <alias> element.");
    else
      *config*.virtual-hosts[name] := *vhost*;
    end;
  else
    warn("Invalid <ALIAS> element.  The 'name' attribute must be specified.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"default-virtual-host")
  let attr = get-attr(node, #"enabled");
  when (attr)
    let enabled? = true-value?(attr);
    *config*.fall-back-to-default-virtual-host? := enabled?;
    log-info("Fallback to the default virtual host is %s.",
             if (enabled?) "enabled" else "disabled" end);
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"debug-server")
  let attr = get-attr(node, #"value");
  when (attr)
    let enabled? = true-value?(attr);
    *config*.debugging-enabled? := enabled?;
    when (enabled?)
      warn("Server debugging is enabled.  Server may crash if not run inside an IDE!");
    end;
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"port")
  let attr = get-attr(node, #"value");
  if (attr)
    block ()
      let port = string-to-integer(attr);
      if (port & positive?(port))
        vhost-port(*vhost*) := port;
        log-info("Virtual host %s: port = %d", vhost-name(*vhost*), port);
      else
        error("jump to the exception clause :-)");
      end;
    exception (<error>)
      warn("Virtual host %s: Invalid port %=", vhost-name(*vhost*), attr);
    end;
  else
    warn("Invalid <port> spec.  The 'value' attribute must be specified.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"auto-register")
  let attr = get-attr(node, #"enabled");
  if (attr)
    let enabled? = true-value?(attr);
    auto-register-pages?(*vhost*) := enabled?;
    log-info("Virtual host %s: documents will be auto-registered", vhost-name(*vhost*));
  else
    warn("Invalid <auto-register> spec.  "
           "The 'enabled' attribute must be specified as true or false.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"server-root")
  let filename = get-attr(node, #"location");
  if (filename)
    let loc = as(<directory-locator>, filename);
    server-root(*vhost*) := as(<file-locator>, loc);
    log-info("Server root set to %s", loc);
  else
    warn("Invalid <server-root> spec.  "
         "The 'location' attribute must be specified.");
    *config*.abort-startup? := #t;
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"document-root")
  let loc = get-attr(node, #"location");
  if(loc)
    document-root(*vhost*)
      := merge-locators(as(<directory-locator>, loc), server-root(*vhost*));
    log-info("Virtual host %s: document root is %s.",
             vhost-name(*vhost*), document-root(*vhost*));
  else
    warn("Invalid <document-root> spec.  "
           "The 'location' attribute must be specified.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"log")
  let type = get-attr(node, #"type");
  if (~type)
    warn("<log> element ignored ('type' attribute missing).");
  elseif (~member?(type, #("debug", "activity", "error"),
                   test: string-equal?))
    warn("<log> element ignored (unrecognized 'type' attribute %=).  "
           "Type must be 'debug', 'activity', or 'error'.",
         type);
  else
    let location = get-attr(node, #"location");
    let max-size = get-attr(node, #"max-size");
    let default-max-size = 1024 * 1024 * 20;
    block ()
      max-size := string-to-integer(max-size);
    exception (e :: <error>)
      warn("<log> element has invalid max-size attribute (%s).  "
           "The default (%d) will be used.", max-size, default-max-size);
    end;
    let log = iff(location,
                  make(<rolling-file-log-target>,
                       file: merge-locators(as(<file-locator>, location),
                                            *config*.server-root),
                       max-size: max-size | default-max-size),
                  make(<stream-log-target>,
                       stream: iff(string-equal?(type, "error"),
                                   *standard-error*,
                                   *standard-output*)));

    select (type by string-equal?)
      "error", "errors"
        => error-log-target(*vhost*) := log;
      "activity"
        => activity-log-target(*vhost*) := log;
      "debug"
        => debug-log-target(*vhost*) := log;
           let level = get-attr(node, #"level") | "verbose";
           let unrecognized = #f;
           let class = select (level by string-equal?)
                         "copious" => <log-copious>;
                         "verbose" => <log-verbose>;
                         "debug"   => <log-debug>;
                         "info"    => <log-info>;
                         "warning", "warnings" => <log-warning>;
                         "error", "errors" => <log-error>;
                         otherwise =>
                           unrecognized := #t;
                           <log-verbose>;
                       end;
           log-level(log) := make(class);
           if (unrecognized)
             warn("Unrecognized log level: %=", level);
           end;
           log-info("Added log level %=", level);
    end select;
  end if;
end method process-config-element;

define method process-config-element
    (node :: xml$<element>, name == #"administrator")
  // ---TODO
end;

define method process-config-element
    (node :: xml$<element>, name == #"module")
  let name = get-attr(node, #"name");
  if (name)
    load-module(name, *server*.configuration.server-root);
  end;
end;

define class <mime-type> (xml$<printing>)
end class <mime-type>;

define constant $mime-type = make(<mime-type>);

define method process-config-element
    (node :: xml$<element>, name == #"mime-type-map")
  let filename = get-attr(node, #"location");
  if (~filename)
    warn("<mime-type-map> element missing 'location' attribute.  Element ignored.");
  else
    let map-loc = merge-locators(as(<file-locator>, filename),
                                 subdirectory-locator(server-root(*vhost*),
                                                      $config-directory-name));
    let mime-text = file-contents(map-loc);
    if (mime-text)
      block ()
        let mime-xml :: xml$<document> = xml$parse-document(mime-text);
        log-info("Loading mime-type map from %s.", as(<string>, map-loc));
        log-info("%s",
                 with-output-to-string (stream)
                   xml$transform-document(mime-xml, state: $mime-type, stream: stream);
                 end);
      exception (ex :: <error>)
        warn("Error parsing mime-type map %s: %s", filename, ex)
      end;
    else
      warn("mime-type map %s not found", map-loc);
    end if;
  end if;
end method;

define method xml$transform (node :: xml$<element>, name == #"mime-type",
                             state :: <mime-type>, stream :: <stream>)
  let mime-type = get-attr(node, #"id");
  for (child in xml$node-children(node))
    if (xml$name(child) = #"extension")
      let tmap = mime-type-map(*vhost*);
      tmap[as(<symbol>, xml$text(child))] := mime-type;
    else
      warn("Skipping: %s %s %s: not an extension node!",
           mime-type, xml$name(child), xml$text(child));
    end if;
  end for;
end method xml$transform;


// <directory  pattern = "/"
//             allow-directory-listing = "yes"
//             follow-symlinks = "yes"
// />
define method process-config-element
    (node :: xml$<element>, name == #"directory")
  let pattern = get-attr(node, #"pattern");
  if (~pattern)
    warn("Invalid <directory> spec.  "
           "The 'pattern' attribute must be specified.")
  else
    let dirlist? = get-attr(node, #"allow-directory-listing");
    let follow? = get-attr(node, #"follow-symlinks");
    let root-spec = root-directory-spec(*vhost*);
    let parent = *directory*;
    let spec = make(<directory-spec>,
                    parent: parent,
                    pattern: pattern,
                    follow-symlinks?: iff(follow?,
                                          true-value?(follow?),
                                          parent.follow-symlinks?),
                    allow-directory-listing?: iff(dirlist?,
                                                  true-value?(dirlist?),
                                                  parent.allow-directory-listing?));
    add-directory-spec(*vhost*, spec);
    dynamic-bind (*directory* = spec)
      for (child in xml$node-children(node))
        process-config-element(child, xml$name(child));
      end;
    end;
  end;
end;


// TODO:
// <default-document>index.html</default-document>
// <response>301</response>???


//// URL MAP STORE AND LOOKUP

// Register a response function for a given URL.  See find-responder.
define /* exported */ method register-url
    (config :: <http-server-configuration>, url :: <string>, target :: <function>,
     #key replace?, prefix?)
  let (bpos, epos) = trim-whitespace(url, 0, size(url));
  if (bpos = epos)
    error(make(<koala-api-error>,
               format-string: "You cannot register an empty URL: %=",
               format-arguments: list(substring(url, bpos, epos))));
  else
    add-object(config.url-map, url, pair(target, prefix?), replace?: replace?);
  end;
  log-info("URL %s registered", url);
end method register-url;

// Find a responder function, if any.
define method find-responder
    (config :: <http-server-configuration>, url :: <string>)
 => (responder :: false-or(<function>), #rest more)
  local method maybe-auto-register (url)
          when (config.auto-register-pages?)
            // could use safe-locator-from-url, but it's relatively expensive
            let len = size(url);
            let slash = char-position-from-end('/', url, 0, len);
            let dot = char-position-from-end('.', url, slash | 0, len);
            when (dot & dot < len - 1)
              let ext = substring(url, dot + 1, len);
              let reg-fun = element(config.auto-register-map, ext, default: #f);
              reg-fun & reg-fun(url)
            end
          end
        end;
  let url = decode-url(url, 0, size(url));
  let path = split(url, separator: "/");
  let trie = *server*.configuration.url-map;
  let (responder, unmatched-part-of-url) = find-object(trie, path);
  if (responder)
    let fun = head(responder);
    let prefix? = tail(responder);
    values(fun, prefix?, unmatched-part-of-url)
  else
    maybe-auto-register(url)
  end
end find-responder;

// Register a function that will attempt to register a responder for a URL
// if the URL matches the file extension.  The function should normally call
// register-url (or register-page for DSPs) and should return a responder.
//
define function register-auto-responder
    (config :: <http-server-configuration>, file-extension :: <string>, fn :: <function>,
     #key replace? :: <boolean>)
  if (~replace? & element(config.auto-register-map, file-extension, default: #f))
    cerror("Replace the old auto-responder with the new one and continue.",
           "An auto-responder is already defined for file extension %=.",
           file-extension);
  end;
  config.auto-register-map[file-extension] := fn;
end;

// define responder test (config, "/test" /* , secure?: #t */ )
//     (request, response)
//   format(output-stream(response), "<html><body>test</body></html>");
// end;
define macro responder-definer
  { define responder ?:name (?config:expression, ?url:expression)
        (?request:variable, ?response:variable)
      ?:body
    end
  }
  => { define method ?name (?request, ?response) ?body end;
       register-url(?config, ?url, ?name)
     }

  { define directory responder ?:name (?config:expression, ?url:expression)
        (?request:variable, ?response:variable)
      ?:body
    end
  }
  => { define method ?name (?request, ?response) ?body end;
       register-url(?config, ?url, ?name, prefix?: #t)
     }
end;
