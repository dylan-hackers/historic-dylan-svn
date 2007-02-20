Module:    httpi
Synopsis:  Parse config file and create server configuration object.
Author:    Carl Gay
Copyright: Copyright (c) 2001-2004 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define constant $config-directory-name :: <string> = "conf";

// For any time the user specifies something that would cause an invalid
// server configuration.
//
define class <configuration-error> (<koala-error>)
end;

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
    init-keyword: debug?:;

  // Map from URL string to a response function.  The leading slash is removed
  // from URLs because it's easier to use merge-locators that way.
  // TODO: this should be per vhost
  //       then 'define page' needs to specify vhost until dynamic
  //       library loading works.  (ick.)  once dynamic library loading
  //       works we use <module foo> inside <virtual-host> in the config
  //       and bind *virtual-host* while the library is loading?

  slot url-map :: <string-trie> = make(<string-trie>, object: #f),
    init-keyword: url-map:;

  // The top of the directory tree under which the server's
  // configuration, error, and log files are kept.  Other pathnames are
  // merged against this one, so if they're relative they will be
  // relative to this.  The server-root pathname is  relative to the
  // current working directory, unless changed in the config file.
  slot server-root :: <directory-locator> = as(<directory-locator>, "."),
    init-keyword: server-root:;

  slot mime-type-map :: <table> = shallow-copy(*default-mime-type-map*),
    init-keyword: mime-type-map:;

  // The vhost used if the request host doesn't match any other virtual host.
  slot default-virtual-host :: false-or(<virtual-host>),
    init-keyword: default-virtual-host:;

  // Maps host names to virtual hosts.  Any host name not found in this
  // table maps to default-virtual-host.
  slot virtual-hosts :: <string-table> = make(<string-table>),
    init-keyword: virtual-hosts:;

end class <http-server-configuration>;


// We want the config file values to override the slot default values and the
// "make" keyword arguments to override the config file values, so we need to
// know how to set the slot corresponding to each make keyword argument.
//
// Yes, this is hacky because you need to specify the slot init-keywords in
// two places, but it'll do until I get time to write a "define reflective-class"
// macro or something similar, which allows asking what the slot setters and
// init-keywords are.  It would be nice if it were possible to simply use a
// "reflection" module and all this info were available that way.
//
define table *init-keyword-to-setter-map* = {
    debug?: => debugging-enabled?-setter,
    url-map: => url-map-setter,
    server-root: => server-root-setter,
    mime-type-map: => mime-type-map-setter,
    default-virtual-host: => default-virtual-host-setter,
    virtual-hosts: => virtual-hosts-setter
  };

define method initialize
    (config :: <http-server-configuration>,
     #rest args,
     #key config-file,
          log-level :: subclass(<log-level>) = <log-verbose>,
     #all-keys)
  next-method();
  if (~slot-initialized?(config, default-virtual-host))
    // Note that if the user wants to do something more complicated than specify
    // a single log level for everything then they'll have to create the default
    // virtual host themselves and pass it in.
    config.default-virtual-host := make-default-virtual-host(make(log-level));
  end;
  // Config file overrides default slot values.
  // Then init keywords override config file values.
  if (config-file)
    load-configuration-file(config, config-file);
    // Now override config file settings with init-args again.
    for (i from 0 by 2,
         while: i < args.size - 1)
      let keyword = args[i];
      let setter = element(*init-keyword-to-setter-map*, keyword, default: #f);
      if (setter)
        setter(args[i + 1], config);
      end;
    end for;
  end if;
end method initialize;

define function make-default-virtual-host
    (log-level :: <log-level>) => (vhost :: <virtual-host>)
  let stdout-log = make(<stream-log-target>,
                        stream: *standard-output*,
                        log-level: log-level);
  make(<virtual-host>,
       name: "default",
       activity-log: stdout-log,
       debug-log: stdout-log,
       error-log: make(<stream-log-target>,
                       stream: *standard-error*,
                       log-level: log-level));
end;

define method get-virtual-host
    (config :: <http-server-configuration>, vhost-name :: <string>)
 => (vhost :: false-or(<virtual-host>))
  element(config.virtual-hosts, vhost-name, default: #f)
end;


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
      raise(<configuration-error>,
            "Server configuration file (%s) not found.", filename);
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
      let vhost = make(<virtual-host>, name: name, server-configuration: *config*);
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
    warn("Invalid <alias> element.  The 'name' attribute must be specified.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"default-virtual-host")
  let attr = get-attr(node, #"enabled");
  when (attr)
    let enabled? = true-value?(attr);
    if (~enabled?)
      *config*.default-virtual-host := #f;
    end;
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
    (node :: xml$<element>, name == #"server-root")
  let filename = get-attr(node, #"location");
  if (filename)
    let loc = as(<directory-locator>, filename);
    server-root(*vhost*) := as(<file-locator>, loc);
    log-info("Server root set to %s", loc);
  else
    raise(<configuration-error>,
          "Invalid <server-root> spec.  The 'location' attribute must be specified.");
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

define class <mime-type-state> (xml$<xform-state>)
  constant slot mime-type-map :: <table> = make(<table>);
end class <mime-type-state>;

// <mime-type-map location = "config/mime-type-map.xml" clear = "true" />
//
define method process-config-element
    (node :: xml$<element>, name == #"mime-type-map")
  let filename = get-attr(node, #"location");
  if (~filename)
    warn("<mime-type-map> element missing 'location' attribute.  Element ignored.");
  else
    let map-loc = merge-locators(as(<file-locator>, filename),
                                 subdirectory-locator(*vhost*.server-root,
                                                      $config-directory-name));
    block ()
      let new-map = load-mime-type-file(map-loc);
      // Only clear the default map if the mime-type file loads.
      let clear? = get-attr(node, #"clear");
      if (clear? & true-value?(clear?))
        remove-all-keys!(*vhost*.mime-type-map)
      end;
      let vhost-map = *vhost*.mime-type-map;
      for (value keyed-by type in new-map)
        vhost-map[type] := value;
      end;
    exception (ex :: <error>)
      warn("Error parsing mime-type map %s: %s", filename, ex);
    end;
  end;
end method process-config-element;

define function load-mime-type-file
    (file :: <locator>) => (mime-type-map :: <table>)
  let mime-text = file-contents(file, error?: #t);
  let mime-xml :: xml$<document> = xml$parse-document(mime-text);
  log-info("Loading mime-type map from %s.", as(<string>, file));
  let state = make(<mime-type-state>);
  log-info("%s",
           with-output-to-string (stream)
             xml$transform-document(mime-xml, state: state, stream: stream);
           end);
  state.mime-type-map
end function load-mime-type-file;

/* Example document format...
    <mime-type-map>
        <mime-type id="application/x-gzip">
            <extension>gz</extension>
            <extension>tgz</extension>
        </mime-type>
        ...
*/
define method xml$transform 
    (node :: xml$<element>, name == #"mime-type",
     state :: <mime-type-state>, stream :: <stream>)
  let mime-type = get-attr(node, #"id");
  for (child in xml$node-children(node))
    if (xml$name(child) = #"extension")
      let tmap = state.mime-type-map;
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
    raise(<koala-api-error>,
          "You cannot register an empty URL: %=", substring(url, bpos, epos));
  else
    add-object(config.url-map, url, pair(target, prefix?), replace?: replace?);
    log-info("Registered URL %s", url);
  end;
end method register-url;

// Find a responder function, if any.
define method find-responder
    (config :: <http-server-configuration>, url :: <string>)
 => (responder :: false-or(<function>), #rest more)
  let url = decode-url(url, 0, size(url));
  let path = split(url, separator: "/");
  let trie = *server*.configuration.url-map;
  let (responder, unmatched-part-of-url) = find-object(trie, path);
  if (responder)
    let fun = head(responder);
    let prefix? = tail(responder);
    values(fun, prefix?, unmatched-part-of-url)
  end
end find-responder;

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
