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
    let exe-dir = locator-directory(as(<file-locator>, application-filename()));
    *server-root* := parent-directory(exe-dir);
  end;
end;

define function init-server-root (#key location)
  ensure-server-root();
  when (location)
    *server-root* := merge-locators(as(<directory-locator>, location), *server-root*);
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

define function false-value?
    (val :: <string>) => (true? :: <boolean>)
  ~true-value?(val)
end;



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
  let attr = get-attribute-value(node, #"enabled");
  if (attr)
    *auto-register-pages?* := true-value?(attr);
  else
    log-warning("Malformed <auto-register> setting.  'enabled' must be specified.");
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"server-root")
  let loc = get-attribute-value(node, #"location");
  if (~loc)
    log-config-warning("Malformed <server-root> setting.  'location' must be specified.");
  else
    init-server-root(location: loc);
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"document-root")
  let loc = get-attribute-value(node, #"location");
  if (~loc)
    log-config-warning("Malformed <document-root> setting.  'location' must be specified.");
  else
    init-document-root(location: loc);
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"log")
  let level = get-attribute-value(node, #"level");
  let clear = get-attribute-value(node, #"clear");
  when (clear & true-value?(clear))
    clear-log-levels();
  end;
  if (~level)
    log-config-warning("Malformed <log> setting.  'level' must be specified.");
  else
    let class = select (level by string-equal?)
                  "debug"   => <log-debug>;
                  "warning" => <log-warning>;
                  "error"   => <log-error>;
                  "header", "headers"  => <log-headers>;
                  otherwise => <log-info>;
                end;
    add-log-level(class);
    log-info("Added log level %=", level);
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"debug-server")
  let value = get-attribute-value(node, #"value");
  when (value)
    *debugging-server* := true-value?(value);
  end;
  when (*debugging-server*)
    log-warning("Server debugging is enabled.  Server may crash if not run inside an IDE!");
  end;
end;

define method process-config-element (node :: xml$<element>, name == #"xml-rpc")
  let url = get-attribute-value(node, #"url");
  if (url)
    *xml-rpc-server-url* := url;
    log-info("XML-RPC URL set to %s.", url);
  end;

  let fault-code = get-attribute-value(node, #"internal-error-fault-code");
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


//---TODO: Read mime types from a file and set *mime-type-map*.  Get a more complete set of types.
// ftp://ftp.isi.edu/in-notes/iana/assignments/media-types/media-types

/*
application/mac-binhex40        hqx
application/mac-compactpro      cpt
application/msword              doc
application/octet-stream        bin dms lha lzh exe class
application/oda                 oda
application/pdf                 pdf
application/postscript          ai eps ps
application/powerpoint          ppt
application/rtf                 rtf
application/x-bcpio             bcpio
application/x-cdlink            vcd
application/x-cpio              cpio
application/x-csh               csh
application/x-director          dcr dir dxr
application/x-dvi               dvi
application/x-gtar              gtar
application/x-gzip
application/x-hdf               hdf
application/x-koan              skp skd skt skm
application/x-latex             latex
application/x-mif               mif
application/x-netcdf            nc cdf
application/x-sh                sh
application/x-shar              shar
application/x-stuffit           sit
application/x-sv4cpio           sv4cpio
application/x-sv4crc            sv4crc
application/x-tar               tar
application/x-tcl               tcl
application/x-tex               tex
application/x-texinfo           texinfo texi
application/x-troff             t tr roff
application/x-troff-man         man
application/x-troff-me          me
application/x-troff-ms          ms
application/x-ustar             ustar
application/x-wais-source       src
application/zip                 zip
audio/basic                     au snd
audio/midi                      mid midi kar
audio/mpeg                      mpga mp2
audio/x-aiff                    aif aiff aifc
audio/x-pn-realaudio            ram rm ra
audio/x-pn-realaudio-plugin     rpm
audio/x-realaudio               ra
audio/x-wav                     wav
chemical/x-pdb                  pdb xyz
image/gif                       gif
image/ief                       ief
image/jpeg                      jpeg jpg jpe
image/png                       png
image/tiff                      tiff tif
image/x-cmu-raster              ras
image/x-portable-anymap         pnm
image/x-portable-bitmap         pbm
image/x-portable-graymap        pgm
image/x-portable-pixmap         ppm
image/x-rgb                     rgb
image/x-xbitmap                 xbm
image/x-xpixmap                 xpm
image/x-xwindowdump             xwd
message/external-body
message/news
message/partial
message/rfc822
multipart/alternative
multipart/appledouble
multipart/digest
multipart/mixed
multipart/parallel
text/html                       html htm
text/plain                      txt
text/richtext                   rtx
text/tab-separated-values       tsv
text/x-setext                   etx
text/x-sgml                     sgml sgm
video/mpeg                      mpeg mpg mpe
video/quicktime                 qt mov
video/x-msvideo                 avi
video/x-sgi-movie               movie
x-conference/x-cooltalk         ice
x-world/x-vrml                  wrl vrml


text/plain                               [RFC2646,RFC2046]
                richtext                            [RFC2045,RFC2046]
                enriched                                    [RFC1896]
                tab-separated-values                   [Paul Lindner]
                html                                        [RFC2854]
                sgml                                        [RFC1874]
                vnd.latex-z                                   [Lubos]
                vnd.fmi.flexstor                             [Hurtta]
		uri-list				    [RFC2483]
		vnd.abc					      [Allen]
		rfc822-headers                              [RFC1892]
		vnd.in3d.3dml				     [Powers]
		prs.lines.tag				      [Lines]
		vnd.in3d.spot                                [Powers]
                css                                         [RFC2318]
                xml                                         [RFC3023]
                xml-external-parsed-entity                  [RFC3023]
		rtf					    [Lindner]
                directory
*/


