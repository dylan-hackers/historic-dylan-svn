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


//// Generic pages


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


//// <page-context>

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


//// URL mapping

// Maps page objects to their canonical URIs.
define variable *page-to-uri-map* :: <table> = make(<table>);

define method page-uri
    (page :: <page>) => (uri :: false-or(<string>))
  element(*page-to-uri-map*, page, default: #f)
end;


//// <page>

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
  constant slot name :: <string>, required-init-keyword: #"name";
  constant slot default-prefix :: <string>, required-init-keyword: #"prefix";
  constant slot tag-map :: <string-table> = make(<string-table>);
end;

define constant $dsp-directive-taglib
  = make(<taglib>, name: "%dsp", prefix: "%dsp");

// This taglib is used if the page doesn't contain a %dsp:taglib directive.
define constant $default-taglib
  = make(<taglib>, name: "dsp", prefix: "dsp");

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


// Represents a tag _definition_.
define class <tag> (<object>)
  constant slot name :: <string>, required-init-keyword: #"name";
  constant slot allow-body? :: <boolean>, required-init-keyword: #"allow-body?";
  constant slot tag-function :: <function>, required-init-keyword: #"function";
end;

// Represents a specific call to a tag in a DSP template.
// Also used to represent DSP directives, such as <%dsp:include>,
// in which case the tag slot is not used.
define class <tag-call> (<object>)
  constant slot name :: <string>, required-init-keyword: #"name";
  constant slot prefix :: <string>, required-init-keyword: #"prefix";
  constant slot tag :: <tag>, init-keyword: #"tag";
  constant slot arguments :: <sequence> = #[], init-keyword: #"arguments";
  slot body :: false-or(<dsp-template>) = #f, init-keyword: #"body";
end;

define class <if-tag-call> (<tag-call>)
  slot else-body :: false-or(<dsp-template>) = #f;
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
  end
end;

define method execute
    (call :: <tag-call>, page, request, response);
  let tag :: <tag> = call.tag;
  if (tag.allow-body?)
    apply(tag.tag-function, page, response,
          curry(display-template, call.body, page, request, response),
          request: request,
          call.arguments);
  else
    apply(tag.tag-function, page, response, request: request,
          call.arguments);
  end;
end;

define method execute
    (call :: <if-tag-call>, page, request, response);
  let test = get-arg(call, #"test");
  let predicate = test & get-label(test);
  let body = iff(predicate & predicate(page, request),
                 call.body,
                 call.else-body);
  when (body)
    display-template(body, page, request, response);
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

define method as
    (class :: subclass(<string>), call :: <tag-call>) => (s :: <string>)
  with-output-to-string(out)
    format(out, "<%s:%s", call.prefix, call.name);
    for (arg in call.arguments,
         i from 1)
      format(out, iff(odd?(i), " %s=", "%="), arg);
    end;
    format(out, ">");
  end;
end;

        

// A <dsp-template> represents the items in a parsed .dsp file.
define class <dsp-template> (<object>)
  constant slot contents :: <string>, required-init-keyword: #"contents";
  // When the the bug that prevents the <substring> class from working
  // is fixed, nuke these two slots.
  constant slot content-start :: <integer>, required-init-keyword: #"content-start";
           slot content-end   :: <integer>, required-init-keyword: #"content-end";
  constant slot entries :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot parent  :: false-or(<dsp-template>) = #f, init-keyword: #"parent";
  constant slot source-location :: false-or(<locator>) = #f, init-keyword: #"source";
           slot mod-date; // ---*** TODO
end;

define method add-entry!
    (tmplt :: <dsp-template>, entry :: <object>)
  add!(tmplt.entries, entry);
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
define tag simple in my-taglib (page, response) do-stuff; end;
=> define method simple-tag
       (page, response, #all-keys) do-stuff; end;
   register-tag("simple", simple-tag, body: #f);

// A tag with no body and one arg.  e.g., <xx:show-it key1="blah"/>
define tag foo in my-taglib (page, response, #key key1) do-stuff; end;
=> define method foo-tag
       (page, response, #key key1, #all-keys) ... end;
   register-tag("foo", foo-tag, body: #f);

//---*** TODO:
define tag foo in my-taglib (page, response, key1) do-stuff; end;
=> parse error, no #key supplied before key1

// A tag with body and one arg, e.g., <xx:when test="blah">...</xx:when>
define body tag bar in my-taglib (page, response, body, #key test) do-stuff; end;
=> define method bar-tag
      (page, response, body, #key test, #all-keys) do-stuff; end;
   register-tag("bar", bar-tag, body: #t);

//---*** TODO:
define body tag bar in my-taglib (page, response, body, key1) do-stuff; end;
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
  for (item in tmplt.entries)
    select (item by instance?)
      <string>
        => write(stream, item);
      // A subtemplate is created for tag bodies and for the "include" directive.
      <dsp-template>
        => display-template(item, page, request, response);
      <function>
        => item(page, request, response);
      <tag-call>
        => execute(item, page, request, response);
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
    // More than two taglib directives seems unlikely...
    let taglibs = make(<stretchy-vector>, capacity: 2);
    add!(taglibs, pair(default-prefix($default-taglib), $default-taglib));
    let tmplt = make(<dsp-template>,
                     parent: #f,
                     contents: string,
                     content-start: 0,
                     content-end: size(string),
                     source: source-location(page));
    parse-template(page, tmplt, taglibs, list());
    tmplt
  end;
end parse-page;

// @param bpos points directly after a '<' char in buffer.
// @return tag-prefix and its associated taglib.
define function parse-tag-prefix
    (buffer, taglib-specs, bpos, epos) => (prefix, taglib)
  local method parse-prefix (spec-index :: <integer>)
          if (spec-index >= size(taglib-specs))
            iff(looking-at?("%dsp:", buffer, bpos, epos),
                values("%dsp", #"directive"),
                values(#f, #f))
          else
            let spec = taglib-specs[spec-index];
            let prefix = head(spec);
            let taglib = tail(spec);
            iff(looking-at?(concatenate(prefix, ":"), buffer, bpos, epos),
                values(prefix, taglib),
                parse-prefix(spec-index + 1))
          end
        end;
  parse-prefix(0);
end;

// Parse a DSP directive (a <%dsp:xxx> tag) and its body.  DSP directives may
// not follow the simple XML <tag>...body...</tag> format.  e.g., %dsp:if has
// the format <%dsp:if>...body1...<%dsp:else>...body2...</%dsp:if>.
// @return the index following the end tag.
define function parse-dsp-directive
    (page, tmplt, taglibs, tag-stack, call, tag-start, body-start, has-body?)
 => (scan-pos :: <integer>)
  select (call.name by string-equal?)
    "include"
      => parse-include-directive(page, tmplt, taglibs, tag-stack, call,
                                   tag-start, body-start, has-body?);
    "taglib"
      => parse-taglib-directive(page, tmplt, taglibs, call, tag-start,
                                  body-start, has-body?);
    "if"
      => parse-if-directive(page, tmplt, taglibs, tag-stack, call,
                            tag-start, body-start, has-body?);
    otherwise
      => error("Unrecognized DSP directive %= at position %d",
               call.name, tag-start);
  end;
end;

// Parse a directive of the form
//   <%dsp:if test="foo">...body...<%dsp:else>...body...</%dsp:if>
//
define function parse-if-directive
    (page, parent-tmplt, taglibs, tag-stack, call, tag-start, body-start, has-body?)
 => (scan-pos :: <integer>)
  let test = get-arg(call, #"test");
  if (~test)
    log-warning("Invalid %dsp:if directive in template %s:%d.",
                as(<string>, page.source-location), tag-start);
    log-warning("'test=something' was not specified, so the result will always be false.");
  elseif (~has-body?)
    log-warning("Invalid %dsp:if directive IGNORED in template %=.  A body should be specified.",
                page.source-location);
    body-start
  else
    let buffer = parent-tmplt.contents;
    local method parse-body (start-pos)
            let sub = make(<dsp-template>,
                           parent: parent-tmplt,
                           contents: buffer,
                           content-start: start-pos,
                           content-end: size(buffer));
            let scan-pos = parse-template(page, sub, taglibs, pair(call, tag-stack));
            sub.content-end := scan-pos;
            values(sub, scan-pos)
          end;
    let (true-body, pos) = parse-body(body-start);
    call.body := true-body;
    let else-tag :: <byte-string> = "<%dsp:else>";
    if (pos > size(else-tag)
        & looking-at?(else-tag, buffer, pos - size(else-tag), pos))
      let (false-body, new-pos) = parse-body(pos);
      call.else-body := false-body;
      pos := new-pos
    end;
    add-entry!(parent-tmplt, call);
    pos
  end
end parse-if-directive;

define function parse-include-directive
    (page, tmplt, taglibs, tag-stack, call, tag-start, body-start, has-body?)
 => (scan-pos :: <integer>)
  when (has-body?)
    log-warning("Invalid include tag %s in template %s:%d.  "
                as(<string>, call), as(<string>, page.source-location), tag-start);
    log-warning("The include directive doesn't allow a body; it should end in '/>'.");
  end;
  let url = get-arg(call, #"url");
  let source = document-location(url, context: page-directory(page));
  let contents = file-contents(source);
  if (contents)
    let subtemplate = make(<dsp-template>,
                           source: source,
                           parent: tmplt,
                           contents: contents,
                           content-start: 0,
                           content-end: size(contents));
    parse-template(page, subtemplate, taglibs, tag-stack);
    add-entry!(tmplt, subtemplate);
  else
    error("In template %=, included file %= not found.",
          page.source-location, url);
  end;
  body-start
end;

/** Note that the end of comment string may have whitespace between -- and >.
 @param bpos points directly after the opening comment string "<!--".
 @return the position in buffer directly following of the next end of comment
         string, or size(buffer) if the comment isn't terminated.
*/
define function html-comment-end
    (buffer :: <string>, bpos :: <integer>) => (comment-end :: <integer>)
  block (return)
    let epos :: <integer> = size(buffer);
    iterate loop (pos = bpos)
      when (pos < epos - 3)       // 3 to account for "-->"
        let potential-end = string-position(buffer, "--", pos, epos);
        when (potential-end)
          let non-white = skip-whitespace(buffer, potential-end + 2, epos);
          iff(non-white < epos & buffer[non-white] = '>',
              return(non-white + 1),
              loop(potential-end + 1));
        end;
      end;
    end;
    return(size(buffer));  // comment not terminated
  end block
end;

/**
This is an ad-hoc recursive descent parser for a Dylan Server Page template.
It searches for the next recognizable start tag or DSP directive in the given
template (between tmplt.content-start and tmplt.content-end).  It adds plain
content (i.e., the text between recognized tags) to the current template. Tags
are parsed and added to the template as <tag-call>s.  If the tag has a body,
parse-template calls itself recursively to parse the body, and returns when
it finds the matching end tag.  (This allows for nesting tags of the same name.)

@param page is the top-level page being parsed.
@param tmplt is the current (sub)template being side-effected.
@param taglibs are pairs of the form #(prefix . taglib) created by taglib
       directives in the page.  The default taglib (dsp) is always present.
       Since taglib directives apply from where they occur to the bottom of the
       page, taglibs is a <stretchy-vector> so new items can be added as they're found.
@param tag-stack is the stack of tags seen so far in the recursive descent parser.
       i.e., we expect to see closing tags for each one, in order.  It is a list
       of <tag-call> objects.
*/

define function parse-taglib-directive
    (page, tmplt, taglibs, call, tag-start, body-start, has-body?)
 => (scan-pos :: <integer>)
  when (has-body?)
    //---*** TODO: fix this to simply include the body in the parent template.
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
    iff(~tlib,
        error("Invalid taglib directive in template %=.  "
              "The tag library named %= was not found.",
              tlib-name),
        add!(taglibs, pair(tlib-prefix, tlib)));
  end;
  body-start
end;

define method parse-template (page :: <dylan-server-page>,
                              tmplt :: <dsp-template>,
                              taglibs :: <stretchy-vector>,
                              tag-stack :: <list>)
 => (end-of-template-index :: <integer>)

  let buffer :: <string> = tmplt.contents;
  let bpos :: <integer> = tmplt.content-start;
  let epos :: <integer> = size(buffer);  // was tmplt.content-end;
  let scan-pos :: <integer> = bpos;
  let html-pos :: <integer> = bpos;          // beginning of current non-tag chunk
  let end-tag = ~empty?(tag-stack)
                & format-to-string("</%s:%s>", head(tag-stack).prefix, head(tag-stack).name);
  block (return)
    while (scan-pos < epos)
      let tag-start :: false-or(<integer>) = char-position('<', buffer, scan-pos, epos);
      if (~tag-start)
        // put the remainder of the buffer in the template as a string.
        iff(html-pos < epos,
            add-entry!(tmplt, substring(buffer, html-pos, epos)));
        return(epos);
      elseif (looking-at?("<!--", buffer, tag-start, epos))
        scan-pos := html-comment-end(buffer, tag-start + 4);
      elseif (end-tag & looking-at?(end-tag, buffer, tag-start, epos))
        // done parsing the body of a tag as a subtemplate
        iff(html-pos < tag-start,
            add-entry!(tmplt, substring(buffer, html-pos, tag-start)));
        return(tag-start + size(end-tag))
      elseif (end-tag
              & string-equal?(end-tag, "</%dsp:if>")
              & looking-at?("<%dsp:else>", buffer, tag-start, epos))
        // special case for %dsp:if, the only tag with a non-standard format.
        // <%dsp:if>...body...<%dsp:else>...body...</%dsp:if>
        iff(html-pos < tag-start,
            add-entry!(tmplt, substring(buffer, html-pos, tag-start)));
        return(tag-start + size("<%dsp:else>"))
      else
        let (prefix, taglib) = parse-tag-prefix(buffer, taglibs, tag-start + 1, epos);
        if (~prefix)
          // tag-start points to '<' but not to a known tag prefix like "<%dsp:"
          scan-pos := tag-start + 1;
        else
          // ok, found a valid-looking tag prefix like "<%dsp:" in a known taglib.
          let directive? = (taglib = #"directive");
          iff(html-pos < tag-start,
              add-entry!(tmplt, substring(buffer, html-pos, tag-start)));
          let (call, has-body?, body-start)
            = parse-start-tag(page, buffer, tag-start,
                              iff(directive?, $default-taglib, taglib),
                              prefix, directive?);
          scan-pos := if (directive?)
                        parse-dsp-directive(page, tmplt, taglibs, tag-stack, call,
                                            tag-start, body-start, has-body?)
                      else
                        add-entry!(tmplt, call);
                        if (has-body?)
                          call.body := make(<dsp-template>,
                                            parent: tmplt,
                                            contents: tmplt.contents,
                                            content-start: body-start,
                                            content-end: epos);
                          call.body.content-end
                            := parse-template(page, call.body, taglibs, pair(call, tag-stack));
                        else
                          body-start
                        end if
                      end if;
          html-pos := scan-pos;
        end if;
      end if;
    end while;
    epos        // didn't return from block early, so must be at end of buffer
  end block
end parse-template;

// @param buffer is the string containing the dsp tag.
// @param bpos is the index of (for example) "<prefix:" in buffer.
// @param prefix is e.g. "%dsp".
// @param taglib is the taglib corresponding to prefix.
// @param directive? is true iff prefix is "%dsp".
define function parse-start-tag (page :: <dylan-server-page>,
                                 buffer :: <string>,
                                 bpos :: <integer>,
                                 taglib :: <taglib>,
                                 prefix :: <string>,
                                 directive?)
 => (tag-call :: <tag-call>, has-body?, body-start)
  let name-start = bpos + size(prefix) + 2;  // 2 for the < and : characters
  let epos = size(buffer);
  let name-end = end-of-word(buffer, name-start, epos);
  let name = copy-sequence(buffer, start: name-start, end: name-end);
  let (tag-args, has-body?, end-index)
    = extract-tag-args(buffer, name-end, epos);
  local method make-tag-call ()
          if (directive?)
            make(iff(string-equal?(name, "if"), <if-tag-call>, <tag-call>),
                 name: name, prefix: prefix, arguments: tag-args)
          else
            let tag = find-tag(taglib, name);
            iff(tag,
                make(<tag-call>,
                     name: name, prefix: prefix, tag: tag, arguments: tag-args),
                error("In template %=, the tag %= was not found.",
                      as(<string>, page.source-location),
                      name));
          end
        end method;
  values (make-tag-call(), has-body?, end-index)
end parse-start-tag;

define function end-of-word (buffer :: <string>, bpos :: <integer>, epos :: <integer>)
  local method delim? (char :: <character>) => (b :: <boolean>)
          char = '>' | whitespace?(char)
        end;
  min(char-position-if(delim?, buffer, bpos, epos),
      string-position(buffer, "/>", bpos, epos))
end;

// Parse the key1="val1" key2="val2" arguments from a call to a DSP tag.
// Values may be quoted with either single or double quotes (or nothing, but quoting is recommended).
// There is no way to escape the quote characters.
define method extract-tag-args
    (buffer :: <byte-string>, bpos :: <integer>, epos :: <integer>)
 => (args :: <vector>, has-body? :: <boolean>, body-start :: <integer>)
  local method end-of-key? (char :: <character>) => (b :: <boolean>)
          char = '=' | char = '>' | whitespace?(char)
        end,
        method extract-key/val (buffer :: <byte-string>, key-start :: <integer>)
          let key-end = min(char-position-if(end-of-key?, buffer, key-start, epos),
                            string-position(buffer, "/>", key-start, epos));
          if (~key-end | key-end = key-start)
            error("invalid dsp tag.  couldn't find end of keyword argument");
          else
            let key = as(<symbol>, substring(buffer, key-start, key-end));
            let eq-pos = skip-whitespace(buffer, key-end, epos);
            let char = buffer[eq-pos];
            if (char = '>' | looking-at?("/>", buffer, eq-pos, epos))
              // a key with no value.  e.g., <xx:foo nowrap> where nowrap has no value.
              values(as(<symbol>, substring(buffer, key-start, key-end)),
                     #f,
                     skip-whitespace(buffer, key-end, epos))
            else
              assert(buffer[eq-pos] = '=', "expected '='");
              let val-start = skip-whitespace(buffer, eq-pos + 1, epos);
              let quote-char = buffer[val-start];
              let quote-char? = (quote-char = '\'' | quote-char = '"');
              let val-end = iff(quote-char?,
                                char-position(quote-char, buffer, val-start + 1, epos),
                                end-of-word(buffer, val-start, epos))
                          | epos;
              values(key,
                     substring(buffer, iff(quote-char?, val-start + 1, val-start), val-end),
                     iff(quote-char?, val-end + 1, val-end))
            end if
          end if
        end method;
  let args :: <stretchy-vector> = make(<stretchy-vector>, capacity: 3);
  // iterate once for each key/val pair
  iterate loop (start = skip-whitespace(buffer, bpos, epos))
    if (start >= epos)
      values(args, #f, epos)
    elseif (looking-at?(">", buffer, start, epos))
      values(args, #t, start + 1)
    elseif (looking-at?("/>", buffer, start, epos))
      values(args, #f, start + 2)
    else
      let (key, val, key/val-end) = extract-key/val(buffer, start);
      when (key)
        //---*** TODO: parse val based on type specified in tag-definer.
        add!(args, key);
        add!(args, val);  // possibly #f
      end;
      loop(skip-whitespace(buffer, key/val-end, epos));
    end if
  end iterate
end extract-tag-args;

define method respond-to-head
    (page :: <dylan-server-page>, request :: <request>, response :: <response>)
  //---TODO
end;


//// Labels             (needs better name)

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


//// Utilities

//---*** TODO
define function quote-html
    (text :: <string>) => (quoted-text :: <string>)
  text
end;


//// Tags

define body tag \if in $dsp-directive-taglib
    (page :: <dylan-server-page>,
     response :: <response>,
     do-body :: <function>,
     #key)
  // This tag is handled specially by the display-template
  do-body();
end;


