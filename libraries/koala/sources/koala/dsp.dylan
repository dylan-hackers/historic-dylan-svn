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
//
// See ../example/*.dylan for usage examples.


//---TODO:
// * Automatically parse tag keyword arguments into the type (if any) specified.
//   e.g., "define tag foo (... #key a :: <integer>)" should work.
// * Use some kind of taglib mechanism rather than a global namespace?  See $tag-map.
//   Doesn't seem high priority.  Probably adds mostly-needless complexity.


///
/// Generic pages
///

// Holds the map of query keys/vals in the "?x=1&y=2" part of the URI (for GET method)
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

// Is there any need to maintain POSTed values separately from GET query values?
define constant get-form-value :: <function> = get-query-value;
define constant do-form-values :: <function> = do-query-values;
define constant count-form-values :: <function> = count-query-values;


/// <page-context>

// Gives the user a place to store values that will have a lifetime
// equal to the duration of the page processing (i.e., during process-page).  The
// name is stolen from JSP's PageContext class, but it's not intended to serve the
// same purpose.

define class <page-context> (<attributes-mixin>)
end;

define thread variable *page-context* :: false-or(<page-context>) = #f;

// API
define method page-context
    () => (context :: false-or(<page-context>))
  *page-context*
end;


/// URL mapping

// Maps page objects to their canonical URIs.
define variable *page-to-uri-map* :: <table> = make(<table>);

define method page-uri
    (page :: <page>) => (uri :: false-or(<string>))
  element(*page-to-uri-map*, page, default: #f)
end;


/// <page>

define open primary class <page> (<object>)
end;

define method print-object
    (page :: <page>, stream)
  format(stream, "%s", page-uri(page));
end;

define method initialize
    (page :: <page>, #key uri :: <string>, aliases,  #all-keys)
  next-method();
  register-page(uri, page);
  when (aliases)
    for (alias in iff(instance?(aliases, <string>),
                      list(aliases),
                      aliases))
      register-alias-uri(alias, uri);
    end;
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
define method process-page (page :: <page>,
                            request :: <request>,
                            response :: <response>)
  dynamic-bind (*page-values* = request-query-values(request),
                *page-context* = allocate-resource(<page-context>))
    select (request.request-method)
      #"POST"   => respond-to-post(page, request, response);
      #"GET"    => respond-to-get(page, request, response);
      #"HEAD"   => respond-to-head(page, request, response);
      otherwise => unsupported-request-method-error();
    end;
  end;
end process-page;

// Applications should call this to register a page for a particular URI.
define function register-page (uri :: <string>,
                               page :: <page>,
                               #key replace?)
  register-uri(uri, curry(process-page, page), replace?: replace?);
  *page-to-uri-map*[page] := uri;
end register-page;



//
// Page mixin classes and related methods
//

define free class <file-page-mixin> (<object>)
  slot source-location :: <pathname>, init-keyword: #"source";
  slot contents :: false-or(<string>) = #f;
end;

define method initialize
    (page :: <file-page-mixin>, #key, #all-keys)
  next-method();
  when (~slot-initialized?(page, source-location))
    page.source-location := document-location(page-uri(page));
  end;
end;

// Return a locator for the given URL under the *document-root*.
define method document-location
    (uri :: <string>, #key context :: false-or(<directory-locator>))
 => (source :: <file-locator>)
  let uri = iff(~empty?(uri) & uri[0] = '/',
                copy-sequence(uri, start: 1),  // get rid of leading slash
                uri);
  merge-locators(as(<file-locator>, uri), context | *document-root*)
end;

define method page-directory
    (page :: <file-page-mixin>) => (locator :: <directory-locator>)
  locator-directory(source-location(page))
end;

define method page-source-modified?
    (page :: <file-page-mixin>) => (modified? :: <boolean>)
  #t;   //---TODO: check source file mod date.
end;


//
// Static pages
//

define open primary class <static-page> (<expiring-mixin>, <file-page-mixin>, <page>)
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
    resource-not-found-error(uri: request-uri(request));
  end;
end;


//
// Templates, tags, taglibs
//

define class <taglib> (<object>)
  slot name :: <string>, required-init-keyword: #"name";
  slot default-prefix :: <string>, required-init-keyword: #"prefix";
  slot tag-map :: <string-table> = make(<string-table>);
end;

// This taglib is used if the page doesn't contain a %dsp:taglib directive.
define constant $default-taglib
  = make(<taglib>,
         name: "dsp default taglib",
         prefix: "dsp");

define method find-tag (taglib :: <taglib>, name :: <string>)
                    => (tag :: false-or(<tag>))
  element(tag-map(taglib), name, default: #f)
end;

define constant $taglib-map :: <string-table> = make(<string-table>);

define method find-taglib
    (name :: <string>) => (taglib :: false-or(<taglib>))
  element($taglib-map, name, default: #f)
end;

define method register-taglib
    (name :: <string>, prefix :: <string>)
  register-taglib(name, make(<taglib>, name: name, prefix: prefix));
end;

define method register-taglib
    (name :: <string>, taglib :: <taglib>)
  when (element($taglib-map, name, default: #f))
    cerror("Replace the old tag library with the new one and continue",
           "A tag library named %= is already defined.",
           name);
  end;
  $taglib-map[name] := taglib;
end;


define class <tag> (<object>)
  slot name :: <string>, required-init-keyword: #"name";
  slot allow-body? :: <boolean>, required-init-keyword: #"allow-body?";
  slot tag-function :: <function>, required-init-keyword: #"function";
end;

// perhaps subclass <tag>?
define class <tag-call> (<object>)
  slot name :: <string>, required-init-keyword: #"name";
  slot prefix :: <string>, required-init-keyword: #"prefix";
  slot tag :: <tag>, init-keyword: #"tag";
  slot arguments :: <sequence> = #[], init-keyword: #"arguments";
  slot body :: <dsp-template> = make(<dsp-template>);
end;

define method get-arg
    (call :: <tag-call>, arg-name :: <symbol>) => (val :: <object>)
  block (return)
    let arguments = arguments(call);
    for (item in arguments, i from 0)
      when (item = arg-name)
        return(arguments[i + 1]);
      end;
    end;
  end;
end;

define method register-tag (tag-name :: <string>,
                            taglib :: <taglib>,
                            tag-fun :: <function>,
                            #key replace?, allow-body? :: <boolean>)
  when (element(taglib.tag-map, tag-name, default: #f))
    cerror("Replace the old tag with the new tag and continue",
           "A tag named %= is already defined in tag library %=.",
           tag-name, taglib.name);
  end;
  taglib.tag-map[tag-name] := make(<tag>,
                                   name: tag-name,
                                   function: tag-fun,
                                   allow-body?: allow-body?);
end;

// A <dsp-template> represents the items in a parsed .dsp file.
define class <dsp-template> (<object>)
  slot contents :: <stretchy-vector> = make(<stretchy-vector>);
  slot taglib :: <taglib> = $default-taglib, init-keyword: #"taglib";
end;


//
// Dylan Server Pages
//

define open primary class <dylan-server-page> (<expiring-mixin>, <file-page-mixin>, <page>)
  // A sequence of strings and functions.  Strings are output directly to the network stream.
  // Functions are tags that are passed the network stream as their only argument.
  slot page-template :: <dsp-template>;
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

/*
// A tag with no body or args, e.g., <bt:current-username/>
define tag simple (page, response) do-stuff; end;
=> define method simple-tag
       (page, response, #all-keys) do-stuff; end;
   register-tag("simple", simple-tag, body: #f);

// A tag with no body and one arg.  e.g., <xx:show-it key1="blah"/>
define tag foo (page, response, #key key1) do-stuff; end;
=> define method foo-tag
       (page, response, #key key1, #all-keys) ... end;
   register-tag("foo", foo-tag, body: #f);

//---*** TODO:
define tag foo (page, response, key1) do-stuff; end;
=> parse error, no #key supplied before key1

// A tag with body and one arg, e.g., <xx:when test="blah">...</xx:when>
define body tag bar (page, response, body, #key test) do-stuff; end;
=> define method bar-tag
      (page, response, body, #key test, #all-keys) do-stuff; end;
   register-tag("bar", bar-tag, body: #t);

//---*** TODO:
define body tag bar (page, response, body, key1) do-stuff; end;
=> parse error, no #key supplied before key1
*/

//---*** TODO: Make the "in taglib" part optional.  There are probably a lot
// of small web apps that don't need to define their own taglib, so they can
// just use $default-taglib.
define macro tag-definer
  // Basic tags, no do-body arg
  { define tag ?tag:name in ?taglib:name (?page:variable, ?response:variable ?arguments:*)
      ?:body
    end }
    => { define method ?tag ## "-tag"
             (?page, ?response ?arguments, #all-keys)
           ?body
         end;
         register-tag(?"tag", ?taglib, ?tag ## "-tag", allow-body?: #f);
       }
  // Same as above but with the "body" modifier.
  { define body tag ?tag:name in ?taglib:name
        (?page:variable, ?response:variable, ?do-body:variable ?arguments:*)
      ?:body
    end }
    => { define method ?tag ## "-tag"
             (?page, ?response, ?do-body ?arguments, #all-keys)
           ?body
         end;
         register-tag(?"tag", ?taglib, ?tag ## "-tag", allow-body?: #t);
       }
  /* this doesn't work.  i'd like to require #key if any args are given.
  arguments:
    { } => { }
    { , #key ?more:* } => { , #key ?more }
  */
end;

define method respond-to-get
    (page :: <dylan-server-page>, request :: <request>, response :: <response>)
  //log-debug("respond-to-get(%s ...)", source-location(page));
  display-page(page, request, response);
end;

define open method display-page
    (page :: <dylan-server-page>, request :: <request>, response :: <response>)
  when (expired?(page) & page-source-modified?(page))
    page.page-template := parse-page(page);
  end;
  display-template(page.page-template, page, request, response);
end;

define method display-template (tmplt :: <dsp-template>,
                                page :: <dylan-server-page>,
                                request :: <request>,
                                response :: <response>)
  let stream = output-stream(response);
  for (item in tmplt.contents)
    select (item by instance?)
      <string>
        => write(stream, item);
      <dsp-template>  // e.g., the "include" directive was used.
        => display-template(item, page, request, response);
      <function>
        => item(page, request, response);
      <tag-call>
        => begin
             let tag :: <tag> = item.tag;
             if (tag.allow-body?)
               apply(tag.tag-function, page, response,
                     curry(display-template, item.body, page, request, response),
                     request: request,
                     item.arguments);
             else
               apply(tag.tag-function, page, response, request: request,
                     item.arguments);
             end;
           end;
      otherwise
        => error("Invalid DSP template element");
    end;
  end for;
end display-template;

define method parse-page
    (page :: <dylan-server-page>)
  let string = file-contents(source-location(page));
  if (~string)
    resource-not-found-error(uri: page-uri(page));
  else
    //log-debug("Parsing page %s", as(<string>, source-location(page)));
    page.contents := string;
    page.mod-time := current-date();
    page.page-template := parse-template(page, string, 0, size(string));
  end;
end parse-page;

define function find-tag-end (tag-start :: <integer>,
                              contents :: <byte-string>,
                              bpos :: <integer>,
                              epos :: <integer>)
 => (tend :: false-or(<integer>), has-body? :: <boolean>)
  // Note this assumes DSP tag elements don't contain any '/' or '>' chars.
  let close-angle = char-position('>', contents, tag-start + 4, epos);
  when (close-angle)
    iff (contents[close-angle - 1] == '/',
         values(close-angle - 1, #f),
         values(close-angle, #t))
  end
end;

        
define function process-dsp-tag (page :: <dylan-server-page>,
                                 contents :: <byte-string>,
                                 call :: <tag-call>,
                                 prefix :: <string>,
                                 epos :: <integer>,      // end of current (sub)template
                                 tag-start :: <integer>, // pos of '<' char in start tag
                                 tag-end :: <integer>,   // pos of '>' char in start tag
                                 has-body?)
 => (scan-pos :: <integer>)
  if (~has-body?)
    tag-end + 2
  else
    let close-tag-name = format-to-string("</%s:%s>", prefix, call.name);
    let end-tag-start = string-position(contents, close-tag-name, tag-end + 1, epos);
    if (end-tag-start)
      let subtemplate = parse-template(page, contents, tag-end + 1, end-tag-start);
      call.body := subtemplate;
    else
      error("Couldn't find closing tag %s in template %s",
            close-tag-name, as(<string>, page.source-location));
    end;
    end-tag-start + size(close-tag-name);
  end;
end;

//---*** TODO: Handle XML/HTML comments correctly.  Should probably re-implement
//             this with a simple state machine parser.
define method parse-template (page :: <dylan-server-page>,
                              buffer :: <byte-string>,
                              bpos :: <integer>,
                              epos :: <integer>)
 => (template :: <dsp-template>)
  let tmplt :: <dsp-template> = make(<dsp-template>);
  let taglib :: <taglib> = tmplt.taglib;
  let prefix :: <string> = taglib.default-prefix;
  let full-prefix :: <string> = format-to-string("<%s:", prefix);
  local method add-entry (entry)
          add!(tmplt.contents, entry);
        end,
        method process-dsp-directive (call, tag-start, tag-end, has-body?)
            => (scan-pos :: <integer>)
          select (call.name by string-equal?)
            "include" => process-include-directive(call, tag-start, tag-end, has-body?);
            "taglib"  => process-taglib-directive(call, tag-start, tag-end, has-body?);
            otherwise => error("Unrecognized DSP directive %= at position %d",
                               call.name, tag-start);
          end;
        end,
        method process-include-directive (call, tag-start, tag-end, has-body?)
            => (scan-pos :: <integer>)
          when (has-body?)
            error("Invalid include directive in template %=.  "
                  "The include directive can't have a body.", page.source-location);
          end;
          let url = get-arg(call, #"url");
          let contents = file-contents(document-location(url, context: page-directory(page)));
          if (contents)
            add-entry(parse-template(page, contents, 0, size(contents)));
          else
            error("Included file %= not found.", url);
          end;
          tag-end + 2
        end,
        method process-taglib-directive (call, tag-start, tag-end, has-body?)
            => (scan-pos :: <integer>)
          when (has-body?)
            error("Invalid taglib directive in template %=.  "
                  "The taglib directive can't have a body.", page.source-location);
          end;
          let tlib-name = get-arg(call, #"name");
          let tlib-prefix = get-arg(call, #"prefix");
          if (~tlib-name)
            error("Invalid taglib directive in template %=.  "
                  "You must specify a taglib name with name=\"taglib-name\".",
                  page.source-location);
          else
            let tlib = find-taglib(tlib-name);
            if (~tlib)
              error("Invalid taglib directive in template %=.  "
                    "The tag library named %= was not found.",
                    tlib-name);
            else
              taglib := tlib;
              prefix := tlib-prefix | tlib.default-prefix;
              full-prefix := format-to-string("<%s:", prefix);
            end;
          end;
          tag-end + 2
        end method;
  let scan-pos :: <integer> = bpos;
  let chunk-pos :: <integer> = bpos;          // beginning of current non-tag chunk
  block (return)
    while (scan-pos < epos)
      let tag-start :: false-or(<integer>) = char-position('<', buffer, scan-pos, epos);
      if (~tag-start)
        // put the remainder of the page in the template as a string.
        iff(chunk-pos < epos,
            add-entry(substring(buffer, chunk-pos, epos)));
        return();
      elseif (looking-at?(full-prefix, buffer, tag-start, epos)
              | looking-at?("<%dsp:", buffer, tag-start, epos))
        let directive? = (buffer[tag-start + 1] = '%');
        let chunk = substring(buffer, chunk-pos, tag-start);
        iff(~empty?(chunk), add-entry(chunk));
        let (tag-end, has-body?) = find-tag-end(tag-start, buffer, epos, bpos);
        if (tag-end)
          let call = parse-tag(page, buffer, tag-start, tag-end, taglib,
                               iff(directive?, "%dsp", prefix), directive?);
          scan-pos := if (directive?)
                        process-dsp-directive(call, tag-start, tag-end, has-body?)
                      else
                        add-entry(call);
                        process-dsp-tag(page, buffer, call, prefix, epos,
                                        tag-start, tag-end, has-body?)
                      end;
          chunk-pos := scan-pos;
        else
          // didn't find the end of the dsp tag.  what to do???
          log-warning("No end tag found for tag at character position %d.",
                      tag-start);
          add-entry(substring(buffer, tag-start, epos));
          return();
        end;
      else
        // tag-start points to '<' but not to a known tag prefix like "<dsp:"
        scan-pos := tag-start + 1;
      end if;
    end while;
  end block;
  tmplt
end parse-template;

// buffer is the string containing the dsp tag.  bpos is the index of "<dsp:" and epos
// is the index of the closing "/>" or ">".
define function parse-tag (page :: <dylan-server-page>,
                           buffer :: <string>,
                           bpos :: <integer>,
                           epos :: <integer>,
                           taglib :: <taglib>,
                           prefix :: <string>, directive?)
 => (tag-call :: <tag-call>, name)
  let name-start = bpos + size(prefix) + 2;  // 2 for the < and : characters
  let wpos = whitespace-position(buffer, name-start, epos);
  let name-end = wpos | epos;
  let name = copy-sequence(buffer, start: name-start, end: name-end);
  let tag-args = extract-tag-args(buffer, name-end, epos);
  if (directive?)
    make(<tag-call>, name: name, prefix: prefix, arguments: tag-args)
  else
    let tag = find-tag(taglib, name);
    if (tag)
      make(<tag-call>, prefix: prefix, name: name, tag: tag, arguments: tag-args)
    else
      error("In template %=, the tag %= was not found.",
            as(<string>, page.source-location),
            name);
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


/// Labels             (needs better name)

// Functions that can be looked up by name and thus can be used from within DSP tags
// like <%dsp:if test="my-label">...</%dsp:if>

// ---*** TODO: If these things pan out, use a macro to define 'em.

define variable *label-map* :: <string-table> = make(<string-table>);

define method register-label
    (name :: <string>, label :: <function>)
  *label-map*[name] := label;
end;

define method get-label
    (name :: <string>)
 => (label :: <function>)
  element(*label-map*, name, default: #f)
end;


/// Utilities

//---*** TODO
define function quote-html
    (text :: <string>) => (quoted-text :: <string>)
  text
end;


