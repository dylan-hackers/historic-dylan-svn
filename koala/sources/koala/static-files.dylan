Module:    internals
Synopsis:  Serve static files and directory listings
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define method maybe-serve-static-file
    (request :: <request>, response :: <response>)
 => (found? :: <boolean>)
  let uri :: <string> = request-uri(request);
  let document :: false-or(<physical-locator>) = static-file-locator-from-uri(uri);
  when (document)
    log-debug("%s static file found", uri);
    select (file-type(document))
      #"directory" => directory-responder(request, response, document);
      otherwise  => static-file-responder(request, response, document);
    end;
    #t
  end;
end;

// This returns the appropriate locator for the given URI, or #f if the URI is 
// invalid (for example it doesn't name an existing file below the *document-root*).
// If the URI names a directory this checks for an appropriate default document
// such as index.html and returns a locator for that, if found.
//
define function static-file-locator-from-uri
    (uri :: <string>) => (locator :: false-or(<physical-locator>))
  let locator = safe-locator-from-uri(uri);
  when (locator)
    file-exists?(locator)
    & iff(instance?(locator, <directory-locator>),
          find-default-document(locator) | locator,
          locator)
  end
end;

define function safe-locator-from-uri
    (uri :: <string>) => (locator :: false-or(<physical-locator>))
  when (*document-root*)
    block ()
      let len :: <integer> = size(uri);
      let (bpos, epos) = trim-whitespace(uri, 0, len);
      if (bpos == epos)
        *document-root*
      else
        let relative-uri = iff(uri[bpos] = '/',
                               substring(uri, bpos + 1, epos),
                               iff(bpos > 0 | epos < len,        // try to prevent copying
                                   substring(uri, bpos, epos),
                                   uri));
        if (empty?(relative-uri))
          *document-root*
        else
          let loc = simplify-locator(merge-locators(as(<physical-locator>, relative-uri),
                                                    *document-root*));
          if (instance?(loc, <file-locator>) & locator-name(loc) = "..")
            loc := locator-directory(locator-directory(loc));
          end;
          locator-below-document-root?(loc) & loc
        end if
      end if
    exception (<locator-error>)
      #f
    end
  end
end safe-locator-from-uri;

define method find-default-document
    (locator :: <directory-locator>) => (locator :: <physical-locator>)
  block (return)
    local method is-default? (directory, name, type)
            // portability - string-equal? is incorrect on Unix systems.
            when (type = #"file" & member?(name, *default-document-names*, test: string-equal?))
              return(as(<physical-locator>, concatenate(directory, name)));
            end;
          end;
    do-directory(is-default?, locator);
    locator  // found nothing
  end;
end;

define method locator-below-document-root?
    (locator :: <physical-locator>) => (below? :: <boolean>)
  let root-path = locator-path(*document-root*);
  let loc-path  = locator-path(locator);
  size(loc-path) >= size(root-path)
    & locator-server(*document-root*) = locator-server(locator)
    & every?(path-element-equal?, root-path, loc-path)  // stops when smaller seq empty
end;

// Serves up a static file
define method static-file-responder
    (request :: <request>, response :: <response>, locator :: <locator>)
  with-open-file(in-stream = locator, direction: #"input", if-does-not-exist: #f,
                 element-type: <byte>)
    let extension = locator-extension(locator);
    let sym = extension & ~empty?(extension) & as(<symbol>, extension);
    add-header(response,
               "Content-type",
               element(*mime-type-map*, sym, default: *default-static-content-type*));
    //---TODO: optimize this
    write(output-stream(response), stream-contents(in-stream));
  end;
end;

// Serves up a directory listing as HTML.  The caller has already verified that this
// locator names a directory, even though it may be a <file-locator>, and that the
// directory it names is under *document-root*.
//---TODO: add image links.  deal with access control.
define method directory-responder
    (request :: <request>, response :: <response>, locator :: <locator>)
  let loc :: <directory-locator>
    = iff(instance?(locator, <directory-locator>),
          locator,
          subdirectory-locator(locator-directory(locator), locator-name(locator)));
  let stream = output-stream(response);
  let uri = request-uri(request);
  local method show-file-link (directory, name, type)
          when (name ~= ".." & name ~= ".")
            let locator = as(<physical-locator>, concatenate(directory, name));
            let props = file-properties(locator);
            write(stream, "<tr>\n<td nowrap>");
            display-file-image-link(stream, type, locator-extension(locator));
            format(stream, "</td>\n<td nowrap><a href=\"%s%s\">%s</a></td>\n",
                   name, iff(type = #"directory", "/", ""), name);
            for (key in #[#"size", #"modification-date", #"author"])
              let prop = element(props, key, default: "&nbsp");
              write(stream, "<td nowrap>");
              display-file-property(stream, key, prop, type);
              write(stream, "</td>\n");
            end;
            write(stream, "</tr>\n");
          end;
        end;
  format(stream,
         "<html>\n<head>\n<title>Directory listing of %s</title>\n</head>\n<body>\n"
         "<h2>Directory listing of %s</h2>\n", uri, uri);
  // In FunDev 2.0 SP1 this will never display the "Up to parent directory" because
  // of a bug in the = method for directory locators.  Just always do it for now.
  /*
  unless (loc = *document-root*
          | (instance?(loc, <file-locator>)
             & locator-directory(loc) = *document-root*))
   */
  write(stream, "<a href=\"..\">Up to parent directory</a><p>\n");
  write(stream, "<font face=\"Monospace,Courier\">\n");
  /* end; */
  write(stream, "<table border=\"0\" width=\"100%\" cellpadding=\"2\">\n");
  do-directory(show-file-link, loc);
  write(stream, "</table>\n</font>\n</body>\n</html>\n");
end;

define method display-file-property
    (stream, key, property, file-type :: <symbol>) => ()
  write(stream, "&nbsp;");
end;

define method display-file-property
    (stream, key, property :: <date>, file-type :: <symbol>) => ()
  date-to-stream(stream, property);
end;

define method display-file-property
    (stream, key == #"size", property, file-type :: <symbol>) => ()
  if (file-type == #"file")
    let kb = ceiling/(property, 1024);
    format(stream, "%d KB", kb);
  else
    write(stream, "&nbsp;");
  end;
end;

define open method display-file-image-link
    (stream :: <stream>, file-type :: <symbol>, extension)
end;

define open method display-file-image-link
    (stream :: <stream>, file-type == #"directory", extension)
  //---TODO
end;

define open method display-file-image-link
    (stream :: <stream>, file-type == #"link", extension)
  //---TODO
end;

define open method display-file-image-link
    (stream :: <stream>, file-type :: <symbol>, extension :: <string>)
  display-file-image-link(stream, file-type, as(<symbol>, extension));
end;

define open method display-file-image-link
    (stream :: <stream>, file-type :: <symbol>, extension :: <symbol>)
  //---TODO: Somehow display the icon that the Windows explorer displays next to each file.
end;



