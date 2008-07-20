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

define thread variable %server = #f;

define thread variable %dir = #f;

// Holds the current vhost while config elements are being processed.
define thread variable %vhost = #f;

define class <configuration-error> (<koala-api-error>)
end;

define function config-error
    (format-string :: <string>, #rest format-args)
  signal(make(<configuration-error>,
              format-string: format-string,
              format-arguments: format-args))
end;

/*
define variable *options* = make(<table>);

define function append-option
    (key :: <symbol>, option, #key type = <stretchy-vector>)
  let val = element(*options*, key, default: make(type));
  *options*[key] := add!(val, setting);
end;
*/

// API
// Process the server config file, config.xml.
// Assume a user directory structure like:
// koala/
// koala/bin               // server executable and dlls
// koala/www               // default web document root
// koala/config            // koala-config.xml etc
define method configure-server
    (server :: <http-server>, config-file :: false-or(<string>))
  let defaults
    = merge-locators(merge-locators(as(<file-locator>, $koala-config-filename),
                                    as(<directory-locator>, $koala-config-dir)),
                     server.server-root);
  let config-loc
    = as(<string>, merge-locators(as(<file-locator>, config-file | defaults),
                                  defaults));
  block (return)
    let text = file-contents(config-loc);
    if (text)
      log-info("Loading server configuration from %s.", config-loc);
      configure-from-string(server, text, config-loc);
    elseif (config-file)
      // Only blow out if user specified a config file, not if they're taking
      // the default config file.
      config-error("Server configuration file (%s) not found.", config-loc);
    end if;
  end block;
end method configure-server;

// This is separated out so it can be used by the test suite.
//
define method configure-from-string
    (server :: <http-server>, text :: <string>, filename :: <string>)
  // --todo: Fix parse-document to give a reasonable error message
  // instead of just returning #f.
  let xml :: false-or(xml$<document>) = xml$parse-document(text);
  if (xml)
    dynamic-bind (%vhost = server.default-virtual-host,
                  %dir = root-directory-spec(server.default-virtual-host))
      process-config-node(server, xml);
    end;
  else
    config-error("Unable to parse config file %s", filename);
  end;
end method configure-from-string;

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
// hierarchy seamlessly.
define method process-config-node
    (server :: <http-server>, node :: xml$<tag>) => ()
end;

define method process-config-node
    (server :: <http-server>, node :: xml$<document>) => ()
  for (child in xml$node-children(node))
    process-config-node(server, child);
  end;
end;

define method process-config-node
    (server :: <http-server>, node :: xml$<element>) => ()
  log-debug("Processing config element %=", xml$name(node));
  process-config-element(server, node, xml$name(node));
end;

define method process-config-node
    (server :: <http-server>, node :: xml$<xml>) => ()
  config-error("Unexpected XML document element: %s", xml$name(node));
end;

// Exported.
// Libraries may specialize this.
// Note that the previous comment about the XML parser's class hierarchy
// applies here as well.  Otherwise this would specialize node more tightly.
//
define open generic process-config-element
    (server :: <http-server>, node :: <object>, name :: <object>);

define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name :: <object>)
  warn("Unrecognized configuration setting: %=.  Processing child nodes anyway.",
       name);
  for (child in xml$node-children(node))
    process-config-node(server, child);
  end;
end;

define method process-config-element
    (server :: <http-server>, node :: xml$<comment>, name :: <object>)
end;

define function true-value?
    (val :: <string>) => (true? :: <boolean>)
  member?(val, #("yes", "true", "on"), test: string-equal?)
end;



//// koala-config.xml elements.  One method for each element name.

define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"koala")
  for (child in xml$node-children(node))
    process-config-node(server, child);
  end;
end method process-config-element;


define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"listener")
  let address = get-attr(node, #"address");
  let port = get-attr(node, #"port");
  if (address | port)
    block ()
      let port = string-to-integer(port);
      if (%vhost = server.default-virtual-host)
        log-info("Adding listener for %s:%d", address, port);
        add!(server.server-listeners,
             make-listener(format-to-string("%s:%d", address, port)));
      else
        // Maybe later we'll add a way to specify what listeners correspond
        // to what virtual hosts.  Apache apparently does this, but I'm not
        // sure how useful it is.
        log-warning("<listener> (%s) specified inside %s virtual host element.  "
                    "It will be ignored.  Port must be specified at top level.",
                    node, vhost-name(%vhost));
      end;
    exception (<error>)
      warn("Invalid port (%=) specified in listener element.", port);
    end;
  else
    warn("Invalid <LISTENER> specification.  You must specify either the "
         "'address' or 'port' attribute.");
  end;
end method process-config-element;

define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"virtual-host")
  let name = get-attr(node, #"name");
  if (name)
    let vhost = make-virtual-host(server, name: trim(name));
    add-virtual-host(server, vhost, name);
    dynamic-bind (%vhost = vhost,
                  %dir = root-directory-spec(vhost))
      for (child in xml$node-children(node))
        process-config-element(server, child, xml$name(child))
      end;
    end;
  else
    warn("Invalid <VIRTUAL-HOST> spec.  "
           "The 'name' attribute must be specified.");
  end;
end;

define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"alias")
  let name = get-attr(node, #"name");
  if (name)
    block ()
      add-virtual-host(server, %vhost, name);
    exception (err :: <koala-api-error>)
      warn("Invalid <ALIAS> element.  %s", err);
    end;
  else
    warn("Invalid <ALIAS> element.  The 'name' attribute must be specified.");
  end;
end;

// I considered making this and debug-server be attributes on the
// top-level <koala> element, but then it's impossible to turn on
// logging first in a general way.
define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"default-virtual-host")
  bind (attr = get-attr(node, #"enabled"))
    when (attr)
      server.fall-back-to-default-virtual-host? := true-value?(attr)
    end;
    when (server.fall-back-to-default-virtual-host?)
      log-info("Fallback to the default virtual host is enabled.");
    end;
  end;
end;

define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"debug-server")
  bind (attr = get-attr(node, #"value"))
    when (attr)
      server.debugging-enabled? := true-value?(attr);
    end;
    when (server.debugging-enabled?)
      warn("Server debugging is enabled.  "
           "Server may crash if not run inside an IDE!");
    end;
  end;
end;

define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"server-root")
  if (%vhost == server.default-virtual-host)
    let loc = get-attr(node, #"location");
    if (loc)
      server.server-root
        := merge-locators(as(<directory-locator>, loc), server.server-root);
      log-info("Server root set to %s", loc);
    else
      config-error("Invalid <SERVER-ROOT> spec.  "
                   "The 'location' attribute must be specified.");
    end;
  else
    warn("The <SERVER-ROOT> element is only valid at top-level "
           "(inside the <KOALA> element) in the koala config file.  "
           "It will be ignored.");
  end;
end;

define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"document-root")
  bind (loc = get-attr(node, #"location"))
    if (loc)
      document-root(%vhost)
        := merge-locators(as(<directory-locator>, loc), server.server-root);
      log-info("VHost '%s': document root = %s.",
               vhost-name(%vhost), document-root(%vhost));
    else
      warn("Invalid <DOCUMENT-ROOT> spec.  "
           "The 'location' attribute must be specified.");
    end if;
  end;
end;

define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"dsp-root")
  bind (loc = get-attr(node, #"location"))
    if (loc)
      %vhost.dsp-root := merge-locators(as(<directory-locator>, loc), 
                                        server.server-root);
      log-info("VHost '%s': DSP root = %s.",
               vhost-name(%vhost), dsp-root(%vhost));
    else
      warn("Invalid <DSP-ROOT> spec.  "
           "The 'location' attribute must be specified.");
    end if;
  end;
end;


// The main thing this controls right now is whether check template modification
// dates and reparse them if needed.
//
define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"development-mode")
  let enabled? = get-attr(node, #"enabled");
  let enabled? = enabled? & true-value?(enabled?);
  server.development-mode? := enabled?;
  log-info("Development mode is %s.",
           if (enabled?) "on" else "off" end)
end method process-config-element;


define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"log")
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
    let default-size = 20 * 1024 * 1024;
    block ()
      max-size := string-to-integer(max-size);
    exception (ex :: <error>)
      warn("<LOG> element has invalid max-size attribute (%s).  "
           "The default (%d) will be used.", max-size, default-size);
    end;
    let log = iff(location,
                  make(<rolling-file-log-target>,
                       file: merge-locators(as(<file-locator>, location),
                                            server.server-root),
                       max-size: max-size | default-size),
                  make(<stream-log-target>,
                       stream: iff(string-equal?(type, "error"),
                                   *standard-error*,
                                   *standard-output*)));
    select (type by string-equal?)
      "error", "errors"
        => %error-log-target(%vhost) := log;
      "activity"
        => %activity-log-target(%vhost) := log;
      "debug"
        => %debug-log-target(%vhost) := log;
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
    (server :: <http-server>, node :: xml$<element>, name == #"administrator")
  // ---TODO
end;

define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"module")
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
    (server :: <http-server>, node :: xml$<element>, name == #"mime-type-map")
  let filename = get-attr(node, #"location");
  let mime-type-loc
    = as(<string>,
         merge-locators(merge-locators(as(<file-locator>, filename),
                                       as(<directory-locator>, $koala-config-dir)),
                        server.server-root));
  log-info("Loading mime-type map from %s", mime-type-loc);
  let mime-text = file-contents(mime-type-loc);
  if (mime-text)
    let mime-xml :: xml$<document> = xml$parse-document(mime-text);
    let clear = get-attr(node, #"clear");
    if (clear & true-value?(clear))
      log-info("Clearing default mime type mappings.");
      remove-all-keys!(server.server-mime-type-map);
    end;
    with-output-to-string (stream)
      dynamic-bind (%server = server)
        // Transforming the document side-effects the server's mime type map.
        xml$transform-document(mime-xml, state: $mime-type, stream: stream);
      end;
    end;
  else
    warn("mime-type map %s not found", mime-type-loc);
  end if;
end method process-config-element;

define method xml$transform
    (node :: xml$<element>,
     name == #"mime-type",
     state :: <mime-type>,
     stream :: <stream>)
  let mime-type = get-attr(node, #"id");
  let mime-type-map = server-mime-type-map(%server);
  for (child in xml$node-children(node))
    if (xml$name(child) = #"extension")
      mime-type-map[as(<symbol>, xml$text(child))] := mime-type;
    else
      warn("Skipping: %s %s %s: not an extension node!",
           mime-type, xml$name(child), xml$text(child));
    end if;
  end for;
end method xml$transform;


// <directory  location = "/"
//             allow-directory-listing = "yes"
//             follow-symlinks = "yes"
// />
define method process-config-element
    (server :: <http-server>, node :: xml$<element>, name == #"directory")
  let pattern = get-attr(node, #"pattern");
  if (~pattern)
    warn("Invalid <DIRECTORY> spec.  "
           "The 'pattern' attribute must be specified.")
  else
    let dirlist? = get-attr(node, #"allow-directory-listing");
    let follow? = get-attr(node, #"follow-symlinks");
    let root-spec = root-directory-spec(%vhost);
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
    add-directory-spec(%vhost, spec);
    dynamic-bind (%dir = spec)
      for (child in xml$node-children(node))
        process-config-element(server, child, xml$name(child));
      end;
    end;
  end;
end;


// TODO:
// <default-document>index.html</default-document>
// <response>301</response>???

