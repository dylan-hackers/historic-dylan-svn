Module:    internals
Author:    Carl Gay
Synopsis:  Dylan Server Pages
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Users of this library may respond to HTTP requests in two ways:
// (1) Use "define responder" to register a response function for a given URI.  The function
//     will be passed a <request> and a <response>.
// (2) Define a subclass of <page> and implement the methods respond-to-post, respond-to-get,
//     and respond-to-head.  Use "define page", specifying <page> as a superclass to register
//     a page to be used for a particular URI.
// (3) Use "define page", specifying <dylan-server-page> as a superclass and define any "tags"
//     you need with "define tag".  Create a .dsp file that calls the tags with <dsp:my-tag .../>


//---TODO:
// * Automatically parse tag keyword arguments into the type (if any) specified.
//   e.g., "define tag foo (... #key a :: <integer>)" should work.
// * Use some kind of taglib mechanism rather than a global namespace?  See $tag-map.
//   Doesn't seem high priority.  Probably adds mostly-needless complexity.


//
// Generic pages
//

// Holds the map of query keys/vals in "?x=1&y=2" part of the URI (for GET method)
// or form keys/vals for the POST method.
define thread variable *page-values* :: false-or(<string-table>) = #f;

define method get-query-value
    (key :: <string>) => (val :: false-or(<string>))
  *page-values* & element(*page-values*, key, default: #f)
end;

define method count-query-values
    () => (n :: <integer>)
  size(*page-values*)
end;

define method do-query-values
    (f :: <function>)
  for (val keyed-by key in *page-values* | #[])
    f(key, val);
  end;
end;

define variable *page-to-uri-map* :: <table> = make(<table>);

define method page-uris
    (page :: <page>) => (uris :: <sequence>)
  element(*page-to-uri-map*, page, default: list())
end;

define constant get-form-value :: <function> = get-query-value;
define constant do-form-values :: <function> = do-query-values;
define constant count-form-values :: <function> = count-query-values;

define open primary class <page> (<object>)
end;

define method print-object
    (page :: <page>, stream)
  format(stream, "%s", first(page-uris(page)));
end;

define method initialize
    (page :: <page>, #key uri, #all-keys)
  next-method();
  when (uri)
    iff(instance?(uri, <string>),
        register-page(uri, page),
        for (u in uri) register-page(u, page) end);
  end;
end;

// The protocol every page needs to support.
define open generic respond-to-get  (page :: <page>, request :: <request>, response :: <response>);
define open generic respond-to-post (page :: <page>, request :: <request>, response :: <response>);
define open generic respond-to-head (page :: <page>, request :: <request>, response :: <response>);

// Default methods do nothing.
define method respond-to-get  (page :: <page>, request :: <request>, response :: <response>) end;
define method respond-to-head (page :: <page>, request :: <request>, response :: <response>) end;
define method respond-to-post (page :: <page>, request :: <request>, response :: <response>)
  respond-to-get(page, request, response);
end;

// This is the method registered as the response function for all <page>s.
// See register-page.
define method process-page
    (page :: <page>, request :: <request>, response :: <response>)
  dynamic-bind (*page-values* = request-query-values(request))
    select (request.request-method)
      #"POST" => respond-to-post(page, request, response);
      #"GET"  => respond-to-get(page, request, response);
      #"HEAD" => respond-to-head(page, request, response);
      otherwise => unsupported-request-method-error();
    end;
  end;
end process-page;

// Applications should call this to register a page for a particular URI.
define function register-page
    (uri :: <string>, page :: <page>, #key replace?)
  register-response-function(uri, curry(process-page, page), replace?: replace?);
  *page-to-uri-map*[page] := add-new!(page-uris(page), uri);
end register-page;



//
// Page mixin classes and related methods
//

define free class <file-page-mixin> (<object>)
  slot source-location :: <pathname>, init-keyword: #"source";
end;

define method initialize
    (page :: <file-page-mixin>, #key, #all-keys)
  next-method();
  when (~slot-initialized?(page, source-location))
    page.source-location := source-location-from-uri(first(page-uris(page)));
  end;
end;

define method source-location-from-uri
    (uri :: <string>) => (source :: <file-locator>)
  let uri = iff(~empty?(uri) & uri[0] = '/',
                copy-sequence(uri, start: 1),  // get rid of leading slash
                uri);
  merge-locators(as(<file-locator>, uri), *document-root*)
end;

define method page-source-modified?
    (page :: <file-page-mixin>) => (modified? :: <boolean>)
  #t;   //---TODO: check source file mod date.
end;


//
// Static pages
//

define open primary class <static-page> (<expiring-mixin>, <file-page-mixin>, <page>)
  slot contents :: false-or(<string>) = #f;
end;

define method respond-to-get
    (page :: <static-page>, request :: <request>, response :: <response>)
  if (expired?(page) & page-source-modified?(page))
    page.contents := file-contents(source-location(page));
    page.mod-time := current-date();
  end if;
  if (page.contents)
    let stream = output-stream(response);
    write(stream, page.contents);
    force-output(stream);
  else
    resource-not-found-error();
  end;
end;


//
// Dylan Server Pages
//

// A <dsp-template> represents the items in a parsed .dsp file.
define constant <dsp-template> = <stretchy-vector>;

define open primary class <dylan-server-page> (<expiring-mixin>, <file-page-mixin>, <page>)
  // A sequence of strings and functions.  Strings are output directly to the network stream.
  // Functions are tags that are passed the network stream as their only argument.
  slot parsed-template :: <dsp-template>;

  // For debugging.  Should probably remove this eventually.
  slot contents :: false-or(<string>) = #f;
end;

define class <dsp-error> (<simple-error>) end;

// define page my-dsp (<dylan-server-page>) (uri: "/hello", source: make-locator(...), ...)
//   slot foo :: <integer> = bar;
//   ...
// end;
define macro page-definer
    { define page ?:name (?superclasses:*) (?make-args:*)
        ?slot-specs:*
      end }
 => { define class "<" ## ?name ## ">" (?superclasses) ?slot-specs end;
      define variable "*" ## ?name ## "*" = make("<" ## ?name ## ">", ?make-args);
      ignorable("*" ## ?name ## "*");
    }
end;

define macro tag-definer
  { define tag ?tag:name (?page:variable, ?request:variable, ?response:variable ?arguments)
      ?:body
    end }
  => { define method ?tag ## "-tag" (?page, ?request, ?response ?arguments)
         ?body
       end;
       register-tag(?"tag", ?tag ## "-tag");
     }
  arguments:
    { } => { , process-body }
    { , ?arg:variable ?more:* } => { , ?arg ?more }
    { ?more:* } => { , process-body ?more }
end;

define class <tag-call> (<object>)
  slot tag-prefix :: <string>, required-init-keyword: #"prefix";
  slot tag-name :: <string>, required-init-keyword: #"name";
  slot tag-function :: <function>, init-keyword: #"function";
  slot tag-arguments :: <sequence> = #[], init-keyword: #"arguments";
  slot tag-body :: <dsp-template> = empty-dsp-template();
end;

define method empty-dsp-template
    () => (t :: <dsp-template>)
  make(<dsp-template>)
end;

define method get-arg
    (call :: <tag-call>, arg-name :: <symbol>) => (val :: <object>)
  block (return)
    let arguments = tag-arguments(call);
    for (item in arguments, i from 0)
      when (item = arg-name)
        return(arguments[i + 1]);
      end;
    end;
  end;
end get-arg;

define constant $tag-map :: <string-table> = make(<string-table>);

define method register-tag
    (tag-name :: <string>, tag :: <function>, #key replace?)
  when (element($tag-map, tag-name, default: #f))
    cerror("Replace the old tag with the new tag and continue",
           "A tag named %= is already defined", tag-name);
  end;
  $tag-map[tag-name] := tag;
end;

define method respond-to-get
    (page :: <dylan-server-page>, request :: <request>, response :: <response>)
  //log-debug("respond-to-get(%s ...)", source-location(page));
  display-page(page, request, response);
end;

define open method display-page
    (page :: <dylan-server-page>, request :: <request>, response :: <response>)
  when (expired?(page) & page-source-modified?(page))
    page.parsed-template := parse-page(page);
  end;
  display-template(page, page.parsed-template, request, response);
end;

define method display-template
    (page :: <dylan-server-page>, template :: <dsp-template>, request :: <request>, response :: <response>)
  let stream = output-stream(response);
  for (item in template)
    select (item by instance?)
      <string>
        => write(stream, item);
      <dsp-template>  // e.g., the "include" directive was used.
        => display-template(page, item, request, response);
      <function>
        => item(page, request, response);
      <tag-call>
        => apply(item.tag-function, page, request, response,
                 curry(display-template, page, item.tag-body, request, response),
                 item.tag-arguments);
      otherwise
        => error("Invalid DSP template element");
    end;
  end for;
end display-template;

define method parse-page
    (page :: <dylan-server-page>)
  let string = file-contents(source-location(page));
  if (~string)
    resource-not-found-error();
  else
    //log-debug("Parsing page %s", as(<string>, source-location(page)));
    page.contents := string;
    page.mod-time := current-date();
    page.parsed-template := parse-template(page, page.contents, 0, size(page.contents));
  end;
end parse-page;

//---TODO: Handle XML/HTML comments correctly
define method parse-template
    (page :: <dylan-server-page>, contents :: <byte-string>, bpos :: <integer>, epos :: <integer>)
 => (template :: <dsp-template>)
  let template :: <dsp-template> = make(<dsp-template>);
  local method find-tag-end (tag-start :: <integer>)
                         => (tend :: false-or(<integer>), has-body? :: <boolean>)
          // Note this assumes DSP tag elements don't contain any '/' or '>' chars.
          let close-angle = char-position('>', contents, tag-start + 4, epos);
          when (close-angle)
            iff (contents[close-angle - 1] == '/',
                 values(close-angle - 1, #f),
                 values(close-angle, #t))
          end;
        end,
        method add-entry (entry)
          add!(template, entry);
        end,
        method process-dsp-tag (call, tag-start, tag-end, has-body?) => (scan-pos :: <integer>)
          add-entry(call);
          if (~has-body?)
            tag-end + iff(has-body?, 1, 2);
          else
            let close-tag-name = format-to-string("</dsp:%s>", tag-name(call));
            let end-tag-start = string-position(contents, close-tag-name, tag-end + 1, epos);
            if (end-tag-start)
              let subtemplate = parse-template(page, contents, tag-end + 1, end-tag-start);
              call.tag-body := subtemplate;
              //add-entry(subtemplate);
            else
              error("Couldn't find closing tag %s in template %s",
                    close-tag-name, as(<string>, page.source-location));
            end;
            end-tag-start + size(close-tag-name);
          end;
        end method,
        method process-dsp-directive (call, tag-start, tag-end, has-body?) => (scan-pos :: <integer>)
          iff(string-equal?(tag-name(call), "include"),
              process-include-directive(call, tag-start, tag-end, has-body?),
              error("Unrecognized DSP directive %= at position %d",
                    tag-name(call), tag-start));
        end,
        method process-include-directive (call, tag-start, tag-end, has-body?) => (scan-pos :: <integer>)
          let url = get-arg(call, #"url");
          let loc = merge-locators(as(<file-locator>, url), *document-root*);
          let contents = file-contents(loc);
          if (contents)
            let tmplt = parse-template(page, contents, 0, size(contents));
            add-entry(tmplt);
          else
            error("Included file %= not found.", url);
          end;
          tag-end + 2
        end;
  let scan-pos :: <integer> = bpos;
  let chunk-pos :: <integer> = bpos;          // beginning of current non-tag chunk
  block (return)
    let iter = 0;
    while (scan-pos < epos)
      iter := iter + 1;
      iff(remainder(iter, 1000) = 0,
          break());
      let tag-start :: false-or(<integer>) = char-position('<', contents, scan-pos, epos);
      if (~tag-start)
        // put the remainder of the page in the template as a string.
        iff(chunk-pos < epos,
            add-entry(substring(contents, chunk-pos, epos)));
        return();
      elseif (looking-at?("<dsp:", contents, tag-start, epos)
              | looking-at?("<%dsp:", contents, tag-start, epos))
        let directive? = (contents[tag-start + 1] = '%');
        let chunk = substring(contents, chunk-pos, tag-start);
        iff(~empty?(chunk), add-entry(chunk));
        let (tag-end, has-body?) = find-tag-end(tag-start);
        if (tag-end)
          let call = parse-tag(page, contents, tag-start, tag-end,
                               iff(directive?, "%dsp", "dsp"), directive?);
          scan-pos := iff(directive?,
                          process-dsp-directive(call, tag-start, tag-end, has-body?),
                          process-dsp-tag(call, tag-start, tag-end, has-body?));
          chunk-pos := scan-pos;
        else
          // didn't find the end of the dsp tag.  what to do???
          log-warning("No end tag found for tag at character position %d.",
                      tag-start);
          add-entry(substring(contents, tag-start, epos));
          return();
        end;
      else
        // tag-start points to '<' but not to a known tag prefix like "<dsp:"
        scan-pos := tag-start + 1;
      end if;
    end while;
  end block;
  template
end parse-template;

// buffer is the string containing the dsp tag.  bpos is the index of "<dsp:" and epos
// is the index of the closing "/>" or ">".
define function parse-tag
    (page :: <dylan-server-page>, buffer :: <string>, bpos :: <integer>, epos :: <integer>,
     prefix :: <string>, directive?)
 => (tag-call :: <tag-call>, name)
  let name-start = bpos + size(prefix) + 2;  // 2 for the < and : characters
  let wpos = whitespace-position(buffer, name-start, epos);
  let name-end = wpos | epos;
  let name = copy-sequence(buffer, start: name-start, end: name-end);
  let tag-args = extract-tag-args(buffer, name-end, epos);
  if (directive?)
    make(<tag-call>, prefix: prefix, name: name, arguments: tag-args)
  else
    let tag-fun = element($tag-map, name, default: #f);
    if (tag-fun)
      make(<tag-call>, prefix: prefix, name: name, function: tag-fun, arguments: tag-args)
    else
      error("No tag function was found for tag %= in template %=.",
            name, as(<string>, page.source-location));
    end
  end
end parse-tag;

// Parse the key1="val1" key2="val2" arguments from a call to a DSP tag.
// Values may be quoted with either single or double quotes (or nothing, but quoting is recommended).
// There is no way to escape the quote characters.
define method extract-tag-args
    (buffer :: <byte-string>, bpos :: <integer>, epos :: <integer>)
 => (args :: <vector>)
  local method extract-key/val (buffer :: <byte-string>, start :: <integer>)
          let eq-pos = char-position('=', buffer, start, epos);
          when (eq-pos)
            let key-end = min(whitespace-position(buffer, start, eq-pos) | eq-pos, eq-pos);
            let key = if (key-end > start)
                        as(<symbol>, substring(buffer, start, key-end))
                      end;
            let val-start = skip-whitespace(buffer, eq-pos + 1, epos);
            when (val-start)
              let quote-char = buffer[val-start];
              let quote-char? = member?(quote-char, "'\"");
              let val-end = iff(quote-char?,
                                char-position(quote-char, buffer, val-start + 1, epos),
                                whitespace-position(buffer, val-start + 1, epos))
                            | epos;
              values(key, substring(buffer, iff(quote-char?, val-start + 1, val-start), val-end), val-end)
            end
          end
        end;
  let args :: <stretchy-vector> = make(<stretchy-vector>);
  // iterate once for each key/val pair
  iterate loop (start = skip-whitespace(buffer, bpos, epos))
    when (start & (start < epos))
      let (key, val, key/val-end) = extract-key/val(buffer, start);
      when (key & val)
        add!(args, key);
        add!(args, val);
      end;
      when (key/val-end)
        loop(skip-whitespace(buffer, key/val-end + 1, epos));
      end;
    end;
  end;
  args
end extract-tag-args;

define method respond-to-head
    (page :: <dylan-server-page>, request :: <request>, response :: <response>)
  //---TODO
end;


