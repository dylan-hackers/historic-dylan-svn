Module:    httpi
Synopsis:  For processing the configuration init file, koala-config.xml
Author:    Carl Gay
Copyright: Copyright (c) 2001-2004 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


/*
 * TODO: Should warn when unrecognized attributes are used.
 *       Makes debugging your config file much easier sometimes.
 */

define constant $koala-config-dir :: <string> = "config";
define constant $koala-config-filename :: <string> = "koala-config.xml";

// Holds the current vhost while config elements are being processed.
define thread variable %vhost = $default-virtual-host;

define inline function active-vhost
    () => (vhost :: <virtual-host>)
  if (%vhost == $default-virtual-host
      & ~ *fall-back-to-default-virtual-host?*)
    error("While processing the config file there was an attempt "
          "to set a value for the default virtual host, but fallback "
          "to the default virtual host is disabled so this is useless.  "
          "Either enable fallback to the default virtual host or move "
          "all settings inside a <virtual-host></virtual-host> element.");
  else
    %vhost
  end
end;

define thread variable %dir = root-directory-spec($default-virtual-host);


// Process the server config file, config.xml.
// Assume a user directory structure like:
// koala/
// koala/bin               // server executable and dlls
// koala/www               // default web document root
// koala/config            // koala-config.xml etc
define method configure-server
    (config-file :: false-or(<string>))
  init-server-root();
  let defaults = merge-locators(merge-locators(as(<file-locator>, $koala-config-filename),
                                               as(<directory-locator>, $koala-config-dir)),
                                *server-root*);
  let config-loc = as(<string>,
                      merge-locators(as(<file-locator>, config-file | defaults),
                                     defaults));
  block (return)
    let handler <error> = method (c :: <error>, next-handler :: <function>)
                            if (*debugging-server*)
                              next-handler();  // decline to handle the error
                            else
                              log-error("Error loading config file: %=", c);
                              return();
                            end;
                          end method;
    log-info("Loading server configuration from %s.", config-loc);
    let text = file-contents(config-loc);
    if (text)
      let xml :: xml$<document> = xml$parse-document(text);
      process-config-node(xml);
    else
      log-error("Server configuration file (%s) not found.", config-loc);
      *abort-startup?* := #t;
    end if;
  end block;
end method configure-server;

define function warn
    (format-string, #rest format-args)
  log-warning("%s: %s",
              $koala-config-filename,
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
define open generic process-config-element (node :: <object>, name :: <object>);

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
    let vhost = make(<virtual-host>, name: trim(name));
    add-virtual-host(name, vhost);
    dynamic-bind (%vhost = vhost,
                  %dir = root-directory-spec(vhost))
      for (child in xml$node-children(node))
        process-config-element(child, xml$name(child))
      end;
    end;
  else
    warn("Invalid <VIRTUAL-HOST> spec.  "
           "The 'name' attribute must be specified.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"alias")
  let name = get-attr(node, #"name");
  if (name)
    if ($virtual-hosts[name])
      warn("There is already a virtual host named '%s'.  "
             "Ignoring <ALIAS> element.");
    else
      add-virtual-host(name, active-vhost());
    end
  else
    warn("Invalid <ALIAS> element.  The 'name' attribute must be specified.");
  end;
end;

// I considered making this and debug-server be attributes on the
// top-level <koala> element, but then it's impossible to turn on
// logging first in a general way.
define method process-config-element
    (node :: xml$<element>, name == #"default-virtual-host")
  bind (attr = get-attr(node, #"enabled"))
    when (attr)
      *fall-back-to-default-virtual-host?* := true-value?(attr)
    end;
    when (*fall-back-to-default-virtual-host?*)
      log-info("Fallback to the default virtual host is enabled.");
    end;
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"debug-server")
  bind (attr = get-attr(node, #"value"))
    when (attr)
      *debugging-server* := true-value?(attr);
    end;
    when (*debugging-server*)
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
        vhost-port(active-vhost()) := port;
        log-info("VHost '%s': port = %d", vhost-name(active-vhost()), port);
      else
        error("jump to the exception clause :-)");
      end;
    exception (<error>)
      warn("VHost '%s': Invalid port %=", vhost-name(active-vhost()), attr);
    end;
  else
    warn("Invalid <PORT> spec.  The 'value' attribute must be specified.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"auto-register")
  bind (attr = get-attr(node, #"enabled"))
    iff(attr,
        auto-register-pages?(active-vhost()) := true-value?(attr),
        warn("Invalid <AUTO-REGISTER> spec.  "
               "The 'enabled' attribute must be specified as true or false."));
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"server-root")
  // Note use of %vhost directly rather than active-vhost() here.
  // Don't want to blow out while setting *server-root* just because
  // the config doesn't allow fallback to the default vhost.
  if (%vhost == $default-virtual-host)
    let loc = get-attr(node, #"location");
    if (loc)
      init-server-root(location: loc);
      log-info("Server root set to %s", loc);
    else
      warn("Invalid <SERVER-ROOT> spec.  "
           "The 'location' attribute must be specified.");
      *abort-startup?* := #t;
    end;
  else
    warn("The <SERVER-ROOT> element is only valid at top-level "
           "(inside the <KOALA> element) in the koala config file.  "
           "It will be ignored.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"document-root")
  bind (loc = get-attr(node, #"location"))
    if(loc)
      let vhost = active-vhost();
      document-root(vhost)
        := merge-locators(as(<directory-locator>, loc), *server-root*);
      log-info("VHost '%s': document root = %s.",
               vhost-name(vhost), document-root(vhost));
    else
      warn("Invalid <DOCUMENT-ROOT> spec.  "
             "The 'location' attribute must be specified.");
    end;
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"log")
  let type = get-attr(node, #"type");
  if (~type)
    warn("<LOG> element missing 'type' attribute.");
  elseif (~member?(type, #("debug", "activity", "error"),
                   test: string-equal?))
    warn("Log type %= not recognized.  Should be 'debug', 'activity', "
         "or 'error'.", type);
  else
    let location = get-attr(node, #"location");
    let max-size = get-attr(node, #"max-size");
    block ()
      max-size := string-to-integer(max-size);
    exception (e :: <error>)
      warn("<LOG> element has invalid max-size attribute (%s).  "
           "The default (%d) will be used.", max-size);
    end;
    let log = iff(location,
                  make(<rolling-file-log-target>,
                       file: merge-locators(as(<file-locator>, location),
                                            *server-root*),
                       max-size: max-size | 20000000),
                  make(<stream-log-target>,
                       stream: iff(string-equal?(type, "error"),
                                   *standard-error*,
                                   *standard-output*)));

    select (type by string-equal?)
      "error", "errors"
        => %error-log-target(active-vhost()) := log;
      "activity"
        => %activity-log-target(active-vhost()) := log;
      "debug"
        => %debug-log-target(active-vhost()) := log;
           let level = get-attr(node, #"level") | "info";
           let unrecognized = #f;
           let class = select (level by string-equal?)
                         "copious" => <log-copious>;
                         "verbose" => <log-verbose>;
                         "debug"   => <log-debug>;
                         "info"    => <log-info>;
                         "warning", "warnings" => <log-warning>;
                         "error", "errors" => <log-error>;
                         otherwise =>
                           begin
                             unrecognized := #t;
                             <log-info>;
                           end;
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
    (node :: xml$<element>, name == #"xml-rpc")
  let enable? = get-attr(node, #"enable");
  if (enable? & true-value?(enable?))
    bind (url = get-attr(node, #"url"))
      if (url)
        *xml-rpc-server-url* := url;
        log-info("XML-RPC URL set to %s.", url);
      end;
    end;
    bind (fault-code = get-attr(node, #"internal-error-fault-code"))
      if (fault-code)
        block ()
          let int-code = string-to-integer(fault-code);
          int-code & (*xml-rpc-internal-error-fault-code* := int-code);
          log-info("XML-RPC internal error fault code set to %d.", int-code);
        exception (<error>)
          warn("Invalid XML-RPC fault code, %=, specified.  Must be an integer.",
               fault-code);
        end;
      end if;
    end;
    bind (debug = get-attr(node, #"debug"))
      *debugging-xml-rpc* := (debug & true-value?(debug));
      *debugging-xml-rpc* & log-info("XML-RPC debugging enabled.");
    end;
    init-xml-rpc-server();
  else
    log-info("XML-RPC disabled");
  end if;
end;

define method process-config-element
    (node :: xml$<element>, name == #"module")
  bind (name = get-attr(node, #"name"))
    if (name)
      load-module(name);
    end;
  end;
end;

define class <mime-type> (xml$<printing>)
end class <mime-type>;

define constant $mime-type = make(<mime-type>);

define method process-config-element
    (node :: xml$<element>, name == #"mime-type-map")
  let filename = get-attr(node, #"location");
  let mime-type-loc = as(<string>,
                         merge-locators(as(<file-locator>,
                                           format-to-string("%s/%s",
                                                            $koala-config-dir,
                                                            filename)),
                                        *server-root*));
    let mime-text = file-contents(mime-type-loc);
    if (mime-text)
      let mime-xml :: xml$<document> = xml$parse-document(mime-text);
      log-info("Loading mime-type map from %s.", mime-type-loc);
      log-info("%s",
               with-output-to-string (stream)
                 xml$transform-document(mime-xml, state: $mime-type, stream: stream);
               end);
    else
      warn("mime-type map %s not found", mime-type-loc);
    end if;
end method;

define method xml$transform (node :: xml$<element>, name == #"mime-type",
                             state :: <mime-type>, stream :: <stream>)
  let mime-type = get-attr(node, #"id");
  for (child in xml$node-children(node))
    if (xml$name(child) = #"extension")
      *mime-type-map*[as(<symbol>, xml$text(child))] := mime-type;
    else
      warn("Skipping: %s %s: not an extension node!",
           xml$name(child), xml$text(child));
    end if;
  end for;
end method xml$transform;


// <directory  location = "/"
//             allow-directory-listing = "yes"
//             follow-symlinks = "yes"
// />
define method process-config-element
    (node :: xml$<element>, name == #"directory")
  let pattern = get-attr(node, #"pattern");
  if (~pattern)
    warn("Invalid <DIRECTORY> spec.  "
           "The 'pattern' attribute must be specified.")
  else
    let dirlist? = get-attr(node, #"allow-directory-listing");
    let follow? = get-attr(node, #"follow-symlinks");
    let root-spec = root-directory-spec(active-vhost());
    // TODO: the default value for these should really
    //       be taken from the parent dirspec rather than from root-spec.
    let spec = make(<directory-spec>,
                    pattern: pattern,
                    follow-symlinks?: iff(follow?,
                                          true-value?(follow?),
                                          follow-symlinks?(root-spec)),
                    allow-directory-listing?: iff(dirlist?,
                                                  true-value?(dirlist?),
                                                  allow-directory-listing?(root-spec)));
    add-directory-spec(active-vhost(), spec);
    dynamic-bind (%dir = spec)
      for (child in xml$node-children(node))
        process-config-element(child, xml$name(child));
      end;
    end;
  end;
end;


// TODO:
// <default-document>index.html</default-document>
// <response>301</response>???

