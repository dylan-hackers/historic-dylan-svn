Module:    httpi
Synopsis:  Serve static files and directory listings
Author:    Carl Gay
Copyright: Copyright (c) 2001-2004 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Merges the given URL against the context parameter and ensures that the
// resulting locator refers to a document below the document root.  If not,
// it returns #f to indicate the document location is invalid.
// ---TODO: Consider signalling a real error instead.
//
define method document-location
    (url :: <string>,
     #key context :: <directory-locator> = document-root(*virtual-host*))
 => (locator :: false-or(<physical-locator>))
  block ()
    let len :: <integer> = size(url);
    let (bpos, epos) = trim-whitespace(url, 0, len);
    if (bpos == epos)
      context
    else
      let relative-url = iff(url[bpos] = '/', substring(url, 1, epos), url);
      if (empty?(relative-url))
        context
      else
        let loctype = iff(relative-url[size(relative-url) - 1] == '/',
                          <directory-locator>,
                          <file-locator>);
        let loc = simplify-locator(merge-locators(as(loctype, relative-url), context));
        if (locator-name(loc) = "..")
          loc := locator-directory(locator-directory(loc));
        end;
        log-debug("document-location is %s", as(<string>, loc));
        locator-below-document-root?(loc) & loc
      end if
    end if
  exception (ex :: <locator-error>)
    log-debug("Locator error in document-location: %=", ex);
    #f
  end block
end document-location;

define method maybe-serve-static-file
    (request :: <request>, response :: <response>)
 => (found? :: <boolean>)
  let url :: <string> = request-url(request);
  let document :: false-or(<physical-locator>) = static-file-locator-from-url(url);
  log-debug("Requested document is %s", document);
  when (document)
    let (etag, weak?) = etag(document);
    if (weak?)
      add-header(response, "W/ETag", etag);
    else
      add-header(response, "ETag", etag);
    end if;
    let client-etag = get-header(request, "If-None-Match");
    if (etag = client-etag)
      request.request-method := #"head";
      not-modified(headers: response.response-headers);
    else
      let spec :: <directory-spec> = directory-spec-matching(*virtual-host*, url);
      select (file-type(document))
        #"directory" =>
          if (allow-directory-listing?(spec))
            if (url[size(url) - 1] = '/')
              directory-responder(request, response, document);
            else
              let new-location = concatenate(url, "/");
              moved-permanently-redirect(location: new-location, // 301
                                         header-name: "Location",
                                         header-value: new-location);
            end if;
          else
            access-forbidden-error();  // 403
          end if;
        #"link" =>
          let target = link-target(document);
          block (exit-loop)
            while (#t)
              if (~file-exists?(target)
                    | (~locator-below-document-root?(target)
                         & ~follow-symlinks?(spec)))
                resource-not-found-error(url);
              elseif (file-type(target) == #"link")
                target := link-target(target);
              else
                exit-loop();
              end;
            end;
          end;
          static-file-responder(request, response, target);
        otherwise =>
          static-file-responder(request, response, document);
      end select;
    end if;
    #t
  end when;
end method maybe-serve-static-file;

// @returns the appropriate locator for the given URL, or #f if the URL is 
// invalid (for example it doesn't name an existing file below the document root).
// If the URL names a directory this checks for an appropriate default document
// such as index.html and returns a locator for that, if found.
//
define function static-file-locator-from-url
    (url :: <string>) => (locator :: false-or(<physical-locator>))
  let locator = document-location(url);
  log-debug("document-location returned %=", as(<string>, locator));
  locator
    & file-exists?(locator)
    & iff(instance?(locator, <directory-locator>),
          find-default-document(locator) | locator,
          locator)
end;

define method find-default-document
    (locator :: <directory-locator>) => (locator :: <physical-locator>)
  block (return)
    let default-docs = default-documents(*virtual-host*);
    local method is-default? (directory, name, type)
            let potential :: <file-locator> = as(<file-locator>, name);
            when (type = #"file" & member?(potential, default-docs, test: \=))
              return(merge-locators(potential, as(<directory-locator>, directory)));
            end;
          end;
    do-directory(is-default?, locator);
    locator  // found nothing
  end;
end;

define method locator-below-document-root?
    (locator :: <physical-locator>) => (below? :: <boolean>)
  let relative = relative-locator(locator, document-root(*virtual-host*));
  locator-relative?(relative)  // do they at least share a common ancestor?
    & begin
        let relative-parent = locator-directory(relative);
        ~relative-parent       // is it a file directly in the root dir?
          | begin
              let relative-path = locator-path(relative-parent);
              empty?(relative-path)  // again, is it directly in the root dir?
                | relative-path[0] ~= #"parent"  // does it start with ".."?
            end
      end
end;


// Get MIME Type for file name
define method get-mime-type (locator :: <locator>) => (mime-type :: <string>)
  let extension = locator-extension(locator);
  let sym = extension & ~empty?(extension) & as(<symbol>, extension);
  let mime-type = ((sym & element(*mime-type-map*, sym, default: #f))
                     | default-static-content-type(*virtual-host*));
  log-debug("extension = %=, sym = %=, mime-type = %=", extension, sym, mime-type);
  mime-type;
end method;


// Serves up a static file
define method static-file-responder
    (request :: <request>, response :: <response>, locator :: <locator>)
  with-open-file(in-stream = locator, direction: #"input", if-does-not-exist: #f,
                 element-type: <byte>)
    let mime-type = get-mime-type(locator);
    add-header(response, "Content-Type", mime-type);
    let props = file-properties(locator);
    add-header(response, "Last-Modified",
               as-rfc-1123-date(props[#"modification-date"]));
    //---TODO: optimize this
    write(output-stream(response), stream-contents(in-stream));
  end;
end;

define method etag (locator :: <locator>) => (etag :: <string>, weak? :: <boolean>)
  //generate an etag (use modification date and size)
  // --TODO: algorithm should be changed (md5?), because a file can
  //changes more than once per second without changing size.
  let props = file-properties(locator);
  let now = current-date();
  let timestamp = props[#"modification-date"];
  let time = (date-hours(timestamp) * 60 +
             date-minutes(timestamp)) * 60 +
             date-seconds(timestamp);
  let date = (date-year(timestamp) * 1000 +
             date-month(timestamp)) * 100 +
             date-day(timestamp);
  let weak :: <boolean> = #f;
  let dur :: <day/time-duration> =
    make(<day/time-duration>, days: 0, seconds: 1);
  if (now < timestamp + dur)
    weak := #t;
  end if;
  values(concatenate("\"", integer-to-string(date, base: 16), "-",
                     integer-to-string(time, base: 16), "-",
                     integer-to-string(props[#"size"], base: 16), "\""),
                     weak);
end method etag;


// Serves up a directory listing as HTML.  The caller has already verified that this
// locator names a directory, even though it may be a <file-locator>, and that the
// directory it names is under the document root.
//---TODO: add image links.  deal with access control.
define method directory-responder
    (request :: <request>, response :: <response>, locator :: <locator>)
  let loc :: <directory-locator>
    = iff(instance?(locator, <directory-locator>),
          locator,
          subdirectory-locator(locator-directory(locator), locator-name(locator)));
  let directory-properties = file-properties(locator);
  add-header(response, "Last-Modified",
             as-rfc-1123-date(directory-properties[#"modification-date"]));
  let stream = output-stream(response);
  local
    method show-file-link (directory, name, type)
      unless (name = ".." | name = ".")
        let locator = iff(type = #"directory",
                          subdirectory-locator(as(<directory-locator>, directory), name),
                          merge-locators(as(<file-locator>, name),
                                         as(<directory-locator>, directory)));
        let props = file-properties(locator);
        let link = if (type = #"directory")
                     concatenate(name, "/");
                   else
                     name;
                   end if;
        write(stream, "\t\t\t\t<tr>\n");
        format(stream, "\t\t\t\t<td class=\"name\"><a href=\"%s\">%s</a></td>\n", link, link);
        let mime-type = iff(type = #"file",
                            get-mime-type(locator),
                            "");
        format(stream, "\t\t\t\t<td class=\"mime-type\">%s</td>\n", mime-type);
        for (key in #[#"size", #"modification-date", #"author"],
             alignment in #["right", "left", "left"])
          let prop = element(props, key, default: #f);
          format(stream, "\t\t\t\t<td align=\"%s\" class=\"%s\">",
                 alignment, as(<string>, key));
          if (prop)
            display-file-property(stream, key, prop, type);
          else
            write(stream,"-");
          end if;
          write(stream, "\t\t\t\t</td>\n");
        end;
        write(stream, "\t\t\t</tr>\n");
      end;
    end;
  let url = request-url(request);
  format(stream,
         "<?xml version=\"1.0\"?>\n"
         "<!DOCTYPE html PUBLIC \"-//W3C/DTD XHTML 1.0 Strict//EN\""
         " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
         "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
         "\t<head>\n"
         "\t\t<title>Index of %s</title>\n"
         "\t</head>\n", url);
  format(stream, "\t<body>\n");
  format(stream, "\t\t<table cellspacing=\"4\">\n");
  format(stream, "\t\t\t<caption>Directory listing for %s</caption>\n", url);
  write(stream,  "\t\t\t<col id=\"name\" />\n"
                 "\t\t\t<col id=\"mime-type\" />\n"
                 "\t\t\t<col id=\"size\" />\n"
                 "\t\t\t<col id=\"modification-date\" />\n"
                 "\t\t\t<col id=\"author\" />\n");
  write(stream,  "\t\t\t<thead>\n"
                 "\t\t\t\t<tr>\n"
                 "\t\t\t\t\t<th align=\"left\">Name</th>\n"
                 "\t\t\t\t\t<th align=\"left\">MIME Type</th>\n"
                 "\t\t\t\t\t<th align=\"right\">Size</th>\n"
                 "\t\t\t\t\t<th align=\"left\">Date</th>\n"
                 "\t\t\t\t\t<th align=\"left\">Author</th>\n"
                 "\t\t\t\t</tr>\n"
                 "\t\t\t</thead>\n");
  write(stream,  "\t\t\t<tbody>\n");
  let docroot :: <directory-locator> = document-root(*virtual-host*);
  unless (loc = docroot
          | (instance?(loc, <file-locator>)
             & locator-directory(loc) = docroot))
    write(stream,
          "\t\t\t\t<tr>\n"
          "\t\t\t\t\t<td class=\"name\"><a href=\"../\">../</a></td>\n"
          "\t\t\t\t\t<td class=\"type\"></td>\n"
          "\t\t\t\t\t<td class=\"size\"></td>\n"
          "\t\t\t\t\t<td class=\"modification-date\"></td>\n"
          "\t\t\t\t\t<td class=\"author\" />\n"
          "\t\t\t\t</tr>\n");
  end unless;
  do-directory(show-file-link, loc);
  write(stream,
        "\t\t\t</tbody>\n"
        "\t\t</table>\n"
        "\t</body>\n"
        "</html>\n");
end;

define method display-file-property
    (stream, key, property, file-type :: <file-type>) => ()
end;

define method display-file-property
    (stream, key, property :: <date>, file-type :: <file-type>) => ()
  date-to-stream(stream, property);
end;

define method display-file-property
    (stream, key == #"size", property, file-type :: <file-type>) => ()
  if (file-type == #"file")
    let kilobyte = round/(property, 1024);
    let megabyte = round/(kilobyte, 1024);
    let gigabyte = round/(megabyte, 1024);
    if (gigabyte > 0)
      format(stream, "%d GB", gigabyte);
    elseif (megabyte > 0)
      format(stream, "%d MB", megabyte);
    elseif (kilobyte > 0)
      format(stream, "%d KB", kilobyte);
    else
      format(stream, "%d B", property);
    end if;
  else
    write(stream, "");
  end if;
end;

define method display-file-property
    (stream, key, property :: <string>, file-type :: <file-type>) => ()
  format(stream, property);
end;

define open method display-image-link
    (stream :: <stream>, file-type :: <symbol>, locator :: <directory-locator>)
end;

define open method display-image-link
    (stream :: <stream>, file-type :: <symbol>, locator :: <file-locator>)
  //---TODO: Somehow display the icon that the Windows explorer displays next to each file.
end;



