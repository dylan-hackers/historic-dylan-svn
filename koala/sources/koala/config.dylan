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

define thread variable *virtual-host* :: <virtual-host> = $default-virtual-host;

// This is bound to the current vhost while config elements are being
// processed.
define thread variable %vhost = $default-virtual-host;

define constant $root-dir-spec = root-directory-spec($default-virtual-host);

define thread variable %dir = $root-dir-spec;


// Process the server config file, config.xml.
// Assume a user directory structure like:
// koala/
// koala/bin               // server executable and dlls
// koala/www               // default web document root
// koala/config            // koala-config.xml etc
define method configure-server ()
  init-server-root();
  let config-loc = as(<string>,
                      merge-locators(as(<file-locator>,
                                        format-to-string("%s/%s",
                                                         $koala-config-dir,
                                                         $koala-config-filename)),
                                     *server-root*));
  block (return)
    let handler <error> = method (c :: <error>, next-handler :: <function>)
                            if (*debugging-server*)
                              next-handler();  // decline to handle the error
                            else
                              log-error("Error loading Koala configuration file: %=", c);
                              return();
                            end;
                          end method;
    let text = file-contents(config-loc);
    if (text)
      let xml :: xml$<document> = xml$parse-document(text);
      log-info("Loading server configuration from %s.", config-loc);
      process-config-node(xml);
    else
      log-warning("Server configuration file (%s) not found.", config-loc);
      *abort-startup?* := #t;
    end;
  end block;
end configure-server;

define function warn
    (format-string, #rest format-args)
  log-warning("%s: %s",
              $koala-config-filename,
              apply(format-to-string, format-string, format-args));
end;

// The xml-parser library doesn't seem to define anything like this.
define method get-attribute-value
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

define method process-config-element
    (node :: xml$<element>, name :: <object>)
  warn("Unrecognized configuration setting: %=.  Processing child nodes anyway.",
       name);
  for (child in xml$node-children(node))
    process-config-node(child);
  end;
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
end;


define method process-config-element
    (node :: xml$<element>, name == #"virtual-host")
  let name = get-attribute-value(node, #"name");
  if (name)
    log-info("Processing virtual host %s", name);
    let vhost = make(<virtual-host>, name: trim(name));
    log-debug("Document root for vhost %s is %s",
              vhost-name(vhost), as(<string>, document-root(vhost)));
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
  let name = get-attribute-value(node, #"name");
  if (name)
    if ($virtual-hosts[name])
      warn("There is already a virtual host named '%s'.  "
             "Ignoring <ALIAS> element.");
    else
      add-virtual-host(name, %vhost);
    end
  else
    warn("Invalid <ALIAS> element.  The 'name' attribute must be specified.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"port")
  let attr = get-attribute-value(node, #"value");
  if (attr)
    block ()
      let port = string-to-integer(attr);
      if (port & positive?(port))
        vhost-port(%vhost) := port;
        log-info("Port for virtual host '%s' is %d", vhost-name(%vhost), port);
      else
        error("jump to the exception clause :-)");
      end;
    exception (<error>)
      warn("Invalid port specified for virtual host '%s': %=",
           vhost-name(%vhost), attr);
    end;
  else
    warn("Invalid <PORT> spec.  The 'value' attribute must be specified.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"auto-register")
  bind (attr = get-attribute-value(node, #"enabled"))
    iff(attr,
        auto-register-pages?(%vhost) := true-value?(attr),
        warn("Invalid <AUTO-REGISTER> spec.  "
               "The 'enabled' attribute must be specified as true or false."));
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"server-root")
  if (%vhost == $default-virtual-host)
    let loc = get-attribute-value(node, #"location");
    if (loc)
      init-server-root(location: loc);
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
  bind (loc = get-attribute-value(node, #"location"))
    if(loc)
      document-root(%vhost)
        := merge-locators(as(<directory-locator>, loc), *server-root*);
      log-info("setting document root for virtual host '%s' to %s.",
               vhost-name(%vhost), document-root(%vhost));
    else
      warn("Invalid <DOCUMENT-ROOT> spec.  "
             "The 'location' attribute must be specified.");
    end;
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"log")
  let level = get-attribute-value(node, #"level");
  bind (clear = get-attribute-value(node, #"clear"))
    // "clear" doesn't really make sense anymore since log levels
    // are a simple linear hierarchy, but that could change...
    //  --cgay 2005-05-29
    when (clear & true-value?(clear))
      clear-log-levels();
    end;
  end;
  if (~level)
    warn("Invalid <LOG> spec.  "
           "The 'level' attribute must be specified.");
  else
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
    add-log-level(class);
    if (unrecognized)
      warn("Unrecognized log level: %=", level);
    end;
    log-info("Added log level %=", level);
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"administrator")
  // ---TODO
end;

define method process-config-element
    (node :: xml$<element>, name == #"debug-server")
  bind (value = get-attribute-value(node, #"value"))
    when (value)
      *debugging-server* := true-value?(value);
    end;
  end;
  when (*debugging-server*)
    warn("Server debugging is enabled.  Server may crash if not run inside an IDE!");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"xml-rpc")
  bind (url = get-attribute-value(node, #"url"))
    if (url)
      *xml-rpc-server-url* := url;
      log-info("XML-RPC URL set to %s.", url);
    end;
  end;
  bind (fault-code = get-attribute-value(node, #"internal-error-fault-code"))
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
  bind (debug = get-attribute-value(node, #"debug"))
    *debugging-xml-rpc* := (debug & true-value?(debug));
    *debugging-xml-rpc* & log-info("XML-RPC debugging enabled.");
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"module")
  bind (name = get-attribute-value(node, #"name"))
    if (name)
      load-module(name);
    end;
  end;
end;

define method process-config-element
    (node :: xml$<element>, name == #"logfile")
  let name = get-attribute-value(node, #"location");
  let logfile-loc = as(<string>,
                       merge-locators(as(<file-locator>,
                                         format-to-string("%s/%s",
                                                          $koala-config-dir,
                                                          name)),
                                      *server-root*));
  *logfile* := logfile-loc;
  let type = get-attribute-value(node, #"type");
  if (type & as(<string>, type) = "extended")
    *logfile-type* := #"extended";
  end if;
  log-info("Set logfile to %s", logfile-loc);
end method;

define class <mime-type> (xml$<printing>)
end class <mime-type>;

define constant $mime-type = make(<mime-type>);

define method process-config-element
    (node :: xml$<element>, name == #"mime-type-map")
  let filename = get-attribute-value(node, #"location");
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
      xml$transform-document(mime-xml, state: $mime-type, stream: *standard-output*);
    else
      warn("mime-type map %s not found", mime-type-loc);
    end if;
end method;

define method xml$transform (node :: xml$<element>, name == #"mime-type",
                             state :: <mime-type>, stream :: <stream>)
  let mime-type = get-attribute-value(node, #"id");
  for (child in xml$node-children(node))
    if (xml$name(child) = #"extension")
      *mime-type-map*[as(<symbol>, xml$text(child))] := mime-type;
    else
      warn("Skipping: %s %s: not an extension node!",
           xml$name(child), xml$text(child));
    end if;
  end for;
end method xml$transform;


define method process-config-element
    (node :: xml$<element>, name == #"directory")
  let name = get-attribute-value(node, #"name");
  if (~name)
    warn("Invalid <DIRECTORY> spec.  "
           "The 'name' attribute must be specified.")
  else
    let dirlist? = get-attribute-value(node, #"allow-directory-listing");
    let follow? = get-attribute-value(node, #"follow-symlinks");
    let root-spec = root-directory-spec(%vhost);
    // TODO: the default value for these should really
    //       be taken from the parent dirspec rather than from root-spec.
    let spec = make(<directory-spec>,
                    name: name,
                    follow-symlinks?: iff(follow?,
                                          true-value?(follow?),
                                          follow-symlinks?(root-spec)),
                    allow-directory-listing?: iff(dirlist?,
                                                  true-value?(dirlist?),
                                                  allow-directory-listing?(root-spec)));
    add-directory-spec(%vhost, spec);
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
