Module:    httpi
Synopsis:  For processing the configuration init file, koala-config.xml
Author:    Carl Gay
Copyright: Copyright (c) 2001-2002 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define constant $koala-config-dir :: <string> = "config";
define constant $koala-config-filename :: <string> = "koala-config.xml";
define constant $default-document-root :: <string> = "www";


// Process the server config file, config.xml.
// Assume a user directory structure like:
// koala/
// koala/bin               // server executable and dlls
// koala/www               // default web document root
// koala/config            // koala-config.xml etc
define method configure-server ()
  init-server-root();
  init-document-root();
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
    end;
  end block;
end configure-server;

define function ensure-server-root ()
  when (~*server-root*)
    // This works, in both Windows and unix, but logging is less informative...
    // *server-root* := as(<directory-locator>, "..");
    // ...so use application-filename instead.
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

define function init-document-root (#key location)
  ensure-server-root();
  *document-root*
    := merge-locators(as(<directory-locator>, location | $default-document-root),
                      *server-root*);
end;

define method log-config-warning
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

define method process-config-element (node :: xml$<element>, name :: <object>)
  log-config-warning("Unrecognized configuration setting: %=.  Processing child nodes anyway.",
                     name);
  for (child in xml$node-children(node))
    process-config-node(child);
  end;
end;

define function true-value?
    (val :: <string>) => (true? :: <boolean>)
  member?(val, #("yes", "true", "on"), test: string-equal?)
end;

/*
define function false-value?
    (val :: <string>) => (true? :: <boolean>)
  ~true-value?(val)
end;
*/


//// koala-config.xml elements.  One method for each element name.

define method process-config-element (node :: xml$<element>, name == #"koala")
  for (child in xml$node-children(node))
    process-config-node(child);
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"port")
  let attr = get-attribute-value(node, #"value");
  if (attr)
    block ()
      let port = string-to-integer(attr);
      if (port & positive?(port))
        *server-port* := port;
        log-info("Setting server port to %d", port);
      else
        error("jump to the exception clause :-)");
      end;
    exception (<error>)
      log-warning("Invalid port number in configuration file: %=", attr);
    end;
  else
    log-warning("Malformed <port> setting.  'value' must be specified.");
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"auto-register")
  bind (attr = get-attribute-value(node, #"enabled"))
    iff(attr,
        *auto-register-pages?* := true-value?(attr),
        log-warning("Malformed <auto-register> setting.  'enabled' must be specified."));
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"server-root")
  bind (loc = get-attribute-value(node, #"location"))
    iff(~loc,
        log-config-warning("Malformed <server-root> setting.  'location' must be specified."),
        init-server-root(location: loc));
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"document-root")
  bind (loc = get-attribute-value(node, #"location"))
    iff(~loc,
        log-config-warning("Malformed <document-root> setting.  'location' must be specified."),
        init-document-root(location: loc));
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"log")
  let level = get-attribute-value(node, #"level");
  bind (clear = get-attribute-value(node, #"clear"))
    when (clear & true-value?(clear))
      clear-log-levels();
    end;
  end;
  if (~level)
    log-config-warning("Malformed <log> setting.  'level' must be specified.");
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
      log-warning("Unrecognized log level: %=", level);
    end;
    log-info("Added log level %=", level);
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"administrator")
  // ---TODO
end;

define method process-config-element (node :: xml$<element>, name == #"debug-server")
  bind (value = get-attribute-value(node, #"value"))
    when (value)
      *debugging-server* := true-value?(value);
    end;
  end;
  when (*debugging-server*)
    log-warning("Server debugging is enabled.  Server may crash if not run inside an IDE!");
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"xml-rpc")
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
        log-warning("Invalid XML-RPC fault code, %=, specified.  Must be an integer.",
                    fault-code);
      end;
    end if;
  end;
  bind (debug = get-attribute-value(node, #"debug"))
    *debugging-xml-rpc* := (debug & true-value?(debug));
    *debugging-xml-rpc* & log-info("XML-RPC debugging enabled.");
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"module")
  bind (name = get-attribute-value(node, #"name"))
    if (name)
      load-module(name);
    end;
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"logfile")
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

define method process-config-element (node :: xml$<element>, name == #"mime-type-map")
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
      log-warning("mime-type map %s not found", mime-type-loc);
    end if;
end method;

define method xml$transform(node :: xml$<element>, name == #"mime-type",
                            state :: <mime-type>, stream :: <stream>)
  let mime-type = get-attribute-value(node, #"id");
  for (child in xml$node-children(node))
    if ( xml$name(child) = #"extension" )
      *mime-type-map*[as(<symbol>, xml$text(child))] := mime-type;
    else
      log-warning("Skipping: %s %s: not an extension node!", xml$name(child), xml$text(child));
    end if;
  end for;
end method xml$transform;

