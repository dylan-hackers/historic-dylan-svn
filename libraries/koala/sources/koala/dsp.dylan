Module:    dsp
Author:    Carl Gay
Synopsis:  Dylan Server Pages
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Users of this library may respond to HTTP requests in two ways:
// (1) Use "define responder" to register a response function for a given URL.  The function
//     will be passed a <request> and a <response>.
// (2) Define a subclass of <page> and implement the methods respond-to-post, respond-to-get,
//     and respond-to-head.  Use "define page", specifying <page> as a superclass to register
//     a page to be used for a particular URL.
// (3) Use "define page", specifying <dylan-server-page> as a superclass and define any "tags"
//     you need with "define tag".  Create a .dsp file that calls the tags with <dsp:my-tag .../>
//
// See ../example/*.dylan for usage examples.


define variable *debugging-dsp* :: <boolean> = #f;

define class <dsp-error> (<simple-error>) end;

define class <dsp-parse-error> (<dsp-error>) end;

define function parse-error
    (format-string :: <string>, #rest format-arguments)
  signal(make(<dsp-parse-error>,
              format-string: format-string,
              format-arguments: apply(vector, format-arguments)))
end;

define class <tag-argument-parse-error> (<dsp-parse-error>) end;


//// Logging

define class <log-dsp-warning> (<log-warning>) end;
define class <log-dsp-error> (<log-error>) end;
define class <log-dsp-info> (<log-info>) end;
define class <log-dsp-debug> (<log-debug>) end;

define constant log-dsp-warning = curry(log-message, make(<log-dsp-warning>));
define constant log-dsp-error   = curry(log-message, make(<log-dsp-error>));
define constant log-dsp-info    = curry(log-message, make(<log-dsp-info>));
define constant log-dsp-debug   = curry(log-message, make(<log-dsp-debug>));

begin
  // ---TODO: This should be a configuration setting.  Default to <log-dsp-info>.
  add-log-level(<log-dsp-debug>);
end;


//// Generic pages


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

// Maps page objects to their canonical URLs.
define variable *page-to-url-map* :: <table> = make(<table>);

define method page-url
    (page :: <page>) => (url :: false-or(<string>))
  element(*page-to-url-map*, page, default: #f)
end;


//// <page>

define open primary class <page> (<object>)
end;

define method print-object
    (page :: <page>, stream)
  format(stream, "%s", page-url(page));
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
  with-resource (pc = <page-context>)
    dynamic-bind (*page-context* = pc)
      select (request.request-method)
        #"POST"   => respond-to-post(page, request, response);
        #"GET"    => respond-to-get (page, request, response);
        #"HEAD"   => respond-to-head(page, request, response);
        otherwise => unsupported-request-method-error();
      end;
    end;
  end;
end process-page;

// Applications should call this to register a page for a particular URL.
define function register-page
    (url :: <string>, page :: <page>, #key replace?)
 => (responder :: <function>)
  bind (responder = curry(process-page, page))
    register-url(url, responder, replace?: replace?);
    *page-to-url-map*[page] := url;
    responder
  end
end;

// ---TODO: Test this and export it.
// Register URLs for all files matching the given pathname spec as instances
// of the given page class.
define method register-pages-as
    (path :: <pathname>, page-class :: subclass(<file-page-mixin>),
     #key descend? = #t, file-type)
  // url-dir always ends in '/'
  local method doer (url-dir, directory, name, type)
          format-out("name = %=\n", name);
          select (type)
            #"file" =>
              let file = merge-locators(as(<physical-locator>, name),
                                        as(<directory-locator>, directory));
              register-page(name, make(page-class,
                                       source: file,
                                       url: concatenate(url-dir, name)));
            #"directory" =>
              let dir = subdirectory-locator(as(<directory-locator>, directory), name);
              format-out("dir = %=\n", as(<string>, dir));
              when (descend?)
                do-directory(curry(doer, concatenate(url-dir, name, "/")), dir);
              end;
          end;
        end;
  do-directory(curry(doer, "/"), path);
end;

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
    page.source-location := document-location(page-url(page));
  end;
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
    resource-not-found-error(url: request-url(request));
  end;
end;


//
// Templates, tags, taglibs
//

define class <taglib> (<object>)
  constant slot name :: <string>, required-init-keyword: #"name";
  constant slot default-prefix :: <string>, required-init-keyword: #"prefix";
  constant slot tag-map :: <string-table> = make(<string-table>);
  constant slot named-method-map :: <string-table> = make(<string-table>);
end;

//define taglib example ()
//  default-prefix: "ex";
//  named-methods: logged-in?,
//                 table-has-rows?,
//                 row-generator;
//end;

// This can probably use #key to only allow true property lists in the callers?
define macro taglib-definer
  { define taglib ?:name () ?properties:* end }
  => { register-taglib(taglib-aux1(?"name", ?properties),
                       taglib-aux2(?"name", ?properties));
       taglib-aux3(?"name", ?properties); }
end;

// This just strips out everything except "name: foo;"
define macro taglib-aux1
  { taglib-aux1(?name:expression, ?props) }
  => { begin ?props end | ?name }
props:
  { }
  => { #f }
  { name: ?val:expression; ... }
  => { ?val; ... }
  // ---*** TODO: LHS should only allow a keyword.  Is that doable?
  { ?key:expression ?val:*; ... }
  => { ... }
end;

// This just strips out everything except "default-prefix: foo;"
define macro taglib-aux2
  { taglib-aux2(?name:expression, ?props) }
  => { begin ?props end | ?name }
props:
  { }
  => { #f }
  { default-prefix: ?val:expression; ... }
  => { ?val; ... }
  { ?key:expression ?val:*; ... }
  => { ... }
end;

// This just strips out everything except "named-methods: foo ...;"
define macro taglib-aux3
  { taglib-aux3(?taglib:expression, ?props) }
  => { let _taglib = find-taglib(?taglib);
       ?props }
props:
  { }
  => { }
  { named-methods: ?funs; ... }
  => { ?funs; ... }
  { ?key:expression ?val:expression; ... }
  => { ... }
funs:
  { }
  => { }
  { ?fun:name, ... }
  => { define named-method ?fun in _taglib; ... }
  { ?fun:name = ?val:expression, ... }
  => { define named-method ?fun in _taglib = ?val; ... }
end;

define method find-tag
    (taglib :: <taglib>, name :: <string>) => (tag :: false-or(<tag>))
  element(tag-map(taglib), name, default: #f)
end;

// Map names to tag libraries.
define constant $taglib-map :: <string-table> = make(<string-table>);

define method find-taglib
    (name :: <string>) => (taglib :: false-or(<taglib>))
  element($taglib-map, name, default: #f)
end;

define method register-taglib
    (name :: <string>, prefix :: <string>)
 => (taglib :: <taglib>)
  register-taglib(name, make(<taglib>, name: name, prefix: prefix));
end;

define method register-taglib
    (name :: <string>, taglib :: <taglib>)
 => (taglib :: <taglib>)
  when (element($taglib-map, name, default: #f))
    cerror("Replace the old tag library with the new one and continue",
           "A tag library named %= is already defined.",
           name);
  end;
  $taglib-map[name] := taglib;
end;

// All pages automatically have access to the following two taglibs.
// These defs must follow the register-taglib def.
define taglib %dsp () end;
define constant $%dsp-taglib :: <taglib> = find-taglib("%dsp");

define taglib dsp () end;
define constant $dsp-taglib :: <taglib> = find-taglib("dsp");


//// Named methods

// Functions that can be looked up by name and thus can be used from within DSP tags
// like <dsp:if test="my-predicate">...</dsp:if>

define constant <named-method> = <function>;

define method register-named-method
    (taglib :: <taglib>, name :: <string>, fun :: <named-method>)
  named-method-map(taglib)[name] := fun;
end;

define method get-named-method
    (taglib :: <sequence>, name :: <string>)
 => (fun :: false-or(<named-method>))
  block (return)
    for (lib in taglib)
      // lib is pair(prefix, taglib)
      let f = get-named-method(tail(lib), name);
      f & return(f);
    end;
  end;
end;

define method get-named-method
    (taglib :: <taglib>, name :: <string>)
 => (fun :: false-or(<named-method>))
  element(named-method-map(taglib), name, default: #f)
end;

define method get-named-method
    (taglib :: <string>, name :: <string>)
 => (fun :: false-or(<named-method>))
  let tlib = find-taglib(taglib);
  tlib & get-named-method(tlib, name);
end;

define macro named-method-definer
  { define ?modifiers:* named-method ?:name ?taglib-spec (?arglist:*)
      ?valspec-and-body:* end }
  => { define ?modifiers method ?name (?arglist) ?valspec-and-body end;
       register-named-method(find-taglib(?"taglib-spec"), ?"name", ?name) }

  taglib-spec:
    { } => { dsp }
    { in ?taglib:expression } => { ?taglib }
end;



// Represents a tag _definition_.
define class <tag> (<object>)
  constant slot name :: <string>, required-init-keyword: #"name";
  constant slot allow-body? :: <boolean>, required-init-keyword: #"allow-body?";
  constant slot tag-function :: <function>, required-init-keyword: #"function";
  constant slot parameter-names :: <sequence>, required-init-keyword: #"parameter-names";
  constant slot parameter-types :: <sequence>, required-init-keyword: #"parameter-types";
end;

define method get-parameter-type
    (tag :: <tag>, parameter :: <symbol>) => (type :: false-or(<type>))
  block (return)
    for (name in parameter-names(tag),
         type in parameter-types(tag))
      name = parameter & return(type);
    end
  end
end;

// The user may add methods to this generic in order to parse tag 
// arguments automatically for a given type.
define generic parse-tag-arg
    (name, arg :: <string>, type :: <object>) => (value :: <object>);

// Default method just returns the argument unparsed.
define method parse-tag-arg
    (name, arg :: <string>, type :: <object>) => (value :: <string>)
  arg
end;

define method parse-tag-arg
    (name, arg :: <string>, type :: subclass(<integer>)) => (value :: <integer>)
  string-to-integer(arg)
end;

define method parse-tag-arg
    (name, arg :: <string>, type == <boolean>) => (value :: <boolean>)
  select (arg by string-equal?)
    "true", "yes", "#t" => #t;
    "false", "no", "#f" => #f;
    otherwise =>
      log-dsp-warning("Tag call argument %= should be a boolean value such as"
                      " true/false or yes/no.  false will be used.", arg);
      #f;
  end;
end;

define method parse-tag-arg
    (name, arg :: <string>, type == <symbol>) => (value :: <symbol>)
  as(<symbol>, arg)
end;

// Users can't define this parser because active-taglibs isn't exported.
// Think about exporting it or passing its value to parse-tag-arg.
define method parse-tag-arg
    (param, arg :: <string>, type == <named-method>) => (value :: <named-method>)
  get-named-method(active-taglibs(), arg)
  | signal(make(<tag-argument-parse-error>,
                format-string: "%= is not a named-method.  While parsing the %= argument in a <%s:%s> tag.",
                format-arguments: vector(arg, param, *tag-call*.prefix, *tag-call*.name)))
end;

// So tags can accept parameters of type <date>.
define method parse-tag-arg
    (name, arg :: <string>, type == <date>) => (value :: <date>)
  select (arg by string-equal?)
    "now", "current"
      => current-date();
    otherwise
      //---TODO: Parse dates here.
      => signal(make(<dsp-error>,
                     format-string: "Date parsing not yet implemented."));
  end;
end;


// Represents a specific call to a tag in a DSP template.
// Also used to represent DSP directives, such as <%dsp:include>,
// in which case the tag slot is not used.
define class <tag-call> (<object>)
  constant slot name :: <string>, required-init-keyword: #"name";
  constant slot prefix :: <string>, required-init-keyword: #"prefix";
  constant slot tag :: false-or(<tag>), init-keyword: #"tag";
  constant slot taglib :: <taglib>, init-keyword: #"taglib";
  // @see extract-tag-args
  slot arguments :: <sequence> = #[], init-keyword: #"arguments";
  slot body :: false-or(<dsp-template>) = #f, init-keyword: #"body";
  // The taglibs in effect at the call site.  Used for looking up named methods.
  constant slot taglibs :: <sequence>, required-init-keyword: #"taglibs";
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

define thread variable *tag-call* :: false-or(<tag-call>) = #f;

define function active-taglibs
    () => (taglibs :: <sequence>)
  iff(*tag-call*, *tag-call*.taglibs, #[])
end;

define function tag-call-arguments
    () => (args :: <sequence>)
  iff(*tag-call*, *tag-call*.arguments, #[])
end;

// Apply the given function to the name and value of each tag call argument
// for the current tag, unless the name is in the exclude list.
//
define function map-tag-call-arguments
    (f :: <function>, #key exclude :: <sequence> = #[])
  let name = #f;
  for (item in tag-call-arguments(),
       i from 0)
    iff(even?(i),
        name := item,
        unless (member?(name, exclude))
          f(name, item)
        end);
  end;
end;

define function show-tag-call-arguments
    (stream, #key exclude :: <sequence> = #[])
  map-tag-call-arguments(method (name, value)
                           format(stream, " %s=%=", name, value);
                         end,
                         exclude: exclude);
end;

define method execute
    (call :: <tag-call>, page, request, response);
  let tag :: <tag> = call.tag;
  // Might consider wrapping do-body in a method that logs a warning if the tag
  // isn't supposed to allow a body but one was supplied.
  let do-body
    = iff(call.body,
          curry(display-template, call.body, page, request, response),
          method () end);
  dynamic-bind (*tag-call* = call)
    apply(tag.tag-function, page, response, do-body, call.arguments);
  end;
end;

define method register-tag
    (tag :: <tag>, taglib :: <taglib>, #key replace?)
 => (tag :: <tag>)
  when (element(taglib.tag-map, tag.name, default: #f))
    cerror("Replace the old tag with the new tag and continue",
           "A tag named %= is already defined in tag library %=.",
           tag.name, taglib.name);
  end;
  taglib.tag-map[tag.name] := tag;
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

// define page my-dsp (<dylan-server-page>) (url: "/hello", source: make-locator(...), ...)
//   slot foo :: <integer> = bar;
//   ...
// end;
define macro page-definer
    { define page ?:name (?superclasses:*) (?make-args:*)
        ?slot-specs:*
      end }
 => { define class "<" ## ?name ## ">" (?superclasses) ?slot-specs end;
      define variable "*" ## ?name ## "*" = make("<" ## ?name ## ">", ?make-args);
      register-page-urls("*" ## ?name ## "*", ?make-args);
    }
end;

define function register-page-urls
    (page :: <page>, #key url :: <string>, alias, #all-keys)
 => (responder :: <function>)
  let responder = register-page(url, page);
  when (alias)
    for (alias in iff(instance?(alias, <string>),
                      list(alias),
                      alias))
      register-alias-url(alias, url);
    end;
  end;
  responder
end;


// define tag foo in tlib (page, response) () do-stuff end
// define body tag foo in tlib (page, response, do-body) (foo, bar :: <integer>) do-stuff end
//
define macro tag-definer
  // There are two syntaxes (one with the "body" modifier and one without) so that
  // we can ensure that if the tag isn't supposed to have a body the body will be
  // processed once and only once.  It sounds weird, but the alternative is to simply
  // not display the body (if there is one), which might be very hard to debug, or
  // to make the user remember to deal with the body in each tag.
  { define tag ?tag:name ?taglib-spec
        (?page:variable, ?response:variable) (?tag-parameters:*)
      ?:body
    end }
  => { define tag-aux #f ?tag ?taglib-spec
           (?page, ?response, _do-body) (?tag-parameters)
         ?body;       // semicolon is needed even when ?body ends in semicolon.
         _do-body();  // process the tag body
       end
     }
  { define body tag ?tag:name ?taglib-spec
        (?page:variable, ?response:variable, ?do-body:variable) (?tag-parameters:*)
      ?:body
    end }
  => { define tag-aux #t ?tag ?taglib-spec
           (?page, ?response, ?do-body) (?tag-parameters)
         ?body
       end
     }

  taglib-spec:
    { } => { dsp }
    { in ?taglib:name } => { ?taglib }

end tag-definer;


define macro tag-aux-definer
  { define tag-aux ?allow-body:expression ?tag:name ?taglib:name
        (?page:variable, ?response:variable, ?do-body:variable)
        (?tag-parameters:*)
      ?:body
    end }
  => { register-tag(make(<tag>,
                         name: ?"tag",
                         function: method (?page, ?response, ?do-body, #key ?tag-parameters, #all-keys)
                                     ?body
                                   end,
                         allow-body?: ?allow-body,
                         parameter-names: snarf-tag-parameter-names(?tag-parameters),
                         parameter-types: snarf-tag-parameter-types(?tag-parameters)),
                    find-taglib(?"taglib"));
     }
end tag-aux-definer;

// snarf-tag-parameter-names(v1, v2 = t1, v3 :: t2, v4 :: t3 = d1)
define macro snarf-tag-parameter-names
  { snarf-tag-parameter-names(?params) }
    => { vector(?params) }
  params:
    { } => { }
    { ?param, ... }
      => { ?param, ... }
  param:
    { ?var:name }
      => { ?#"var" }
    { ?var:name = ?default:expression }
      => { ?#"var" }
    { ?var:name :: ?type:expression }
      => { ?#"var" }
    { ?var:name :: ?type:expression = ?default:expression }
      => { ?#"var" }
end;

// snarf-tag-parameter-types(v1, v2 = t1, v3 :: t2, v4 :: t3 = d1)
define macro snarf-tag-parameter-types
  { snarf-tag-parameter-types(?params) }
    => { vector(?params) }
  params:
    { } => { }
    { ?param, ... }
      => { ?param, ... }
  param:
    { ?var:name }
      => { <object> }
    { ?var:name = ?default:expression }
      => { <object> }
    { ?var:name :: ?type:expression }
      => { ?type }
    { ?var:name :: ?type:expression = ?default:expression }
      => { ?type }
end;

define body tag %%placeholder-for-unparsable-tags in dsp
    (page :: <dylan-server-page>, response :: <response>, process-body :: <function>)
    ()
  format(output-stream(response), " <!--TAG PARSE ERROR--> ");
  process-body();
end;

define constant $placeholder-tag = find-tag($dsp-taglib, "%%placeholder-for-unparsable-tags");

define function make-dummy-tag-call
    (prefix :: <string>, name :: <string>) => (call :: <tag-call>)
  make(<tag-call>,
       name: name,
       prefix: prefix,
       tag: $placeholder-tag,
       taglibs: #[])
end;


// Default method on respond-to-get processes the DSP template and displays
// the result.  Subclasses can either call this with next-method() or call
// process-template explicitly.
//
define method respond-to-get
    (page :: <dylan-server-page>, request :: <request>, response :: <response>)
  process-template(page, request, response);
end;

// Subclasses of <dylan-server-page> can call this in their respond-to-get/post
// methods if they decide they want the DSP template to be processed.  (They
// may also skip template processing by calling some other respond-to-get/post
// method, throwing an exception, etc.
//
define open method process-template
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
        => signal(make(<dsp-error>,
                       format-string: "Invalid DSP template element"));
    end;
  end for;
end display-template;

define function initial-taglibs-for-parse-template
    () => (taglibs :: <stretchy-vector>)
  // More than 3 user taglib directives seems unlikely...
  let taglibs :: <stretchy-vector> = make(<stretchy-vector>, capacity: 4);
  add!(taglibs, pair(default-prefix($dsp-taglib), $dsp-taglib));
  taglibs
end;

define method parse-page
    (page :: <dylan-server-page>)
  let string = file-contents(source-location(page));
  if (~string)
    resource-not-found-error(url: page-url(page));
  else
    pt-debug("Parsing page %s", as(<string>, source-location(page)));
    page.contents := string;
    page.mod-time := current-date();
    let tmplt = make(<dsp-template>,
                     parent: #f,
                     contents: string,
                     content-start: 0,
                     content-end: size(string),
                     source: source-location(page));
    parse-template(page, tmplt, initial-taglibs-for-parse-template(), list());
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
    otherwise
      => parse-error("Unrecognized DSP directive %= at position %d",
                     call.name, tag-start);
  end;
end;

define function parse-include-directive
    (page, tmplt, taglibs, tag-stack, call, tag-start, body-start, has-body?)
 => (scan-pos :: <integer>)
  when (has-body?)
    log-dsp-warning("Invalid include tag %s in template %s:%d.  ",
                    as(<string>, call), as(<string>, page.source-location), tag-start);
    log-dsp-warning("The include directive doesn't allow a body; it should end in '/>'.");
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
    parse-template(page, subtemplate, initial-taglibs-for-parse-template(), tag-stack);
    add-entry!(tmplt, subtemplate);
  else
    parse-error("In template %=, included file %= not found.",
                page.source-location, url);
  end;
  body-start
end;

// Note that the end of comment string may have whitespace between -- and >.
// @param bpos points directly after the opening comment string "<!--".
// @return the position in buffer directly following of the next end of comment
//         string, or size(buffer) if the comment isn't terminated.
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
    parse-error("Invalid taglib directive in template %=.  "
                "The taglib directive can't have a body.",
                page.source-location);
  end;
  let tlib-name = get-arg(call, #"name");
  let tlib-prefix = get-arg(call, #"prefix");
  if (~tlib-name)
    parse-error("Invalid taglib directive in template %=.  "
                "You must specify a taglib name with name=\"taglib-name\".",
                page.source-location);
  else
    let tlib = find-taglib(tlib-name);
    iff(~tlib,
        parse-error("Invalid taglib directive in template %=.  "
                    "The tag library named %= was not found.",
                    tlib-name),
        add!(taglibs, pair(tlib-prefix | tlib-name, tlib)));
  end;
  body-start
end;

define constant $debugging-templates :: <boolean> = #f;

define function pt-debug
    (format-string, #rest args)
  when ($debugging-templates)
    apply(log-dsp-debug, format-string, args);
  end;
end;

// @param page is really only passed so page.source-location can be used in error messages.
// @param tmplt is the <dsp-template> that is being parsed.  It is side-effected.
// @param taglibs are the taglibs in effect for the parse.  Each time a %dsp:taglib
//        directive is encountered the new taglib is added to the end.  Note that the
//        scope of a taglib directive is from where it occurs to the end of the page.
//        Each entry is pair(taglib-prefix, taglib).
// @param tag-stack represents the nesting of tag calls in a page, so we know what
//        end tag to expect.
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
  pt-debug("parse-template: enter.  scan-pos = %d, tag-end = %=",
           scan-pos, end-tag);
  block (return)
    while (scan-pos < epos)
      let tag-start :: false-or(<integer>) = char-position('<', buffer, scan-pos, epos);
      if (~tag-start)
        // put the remainder of the buffer in the template as a string.
        iff(html-pos < epos,
            add-entry!(tmplt, substring(buffer, html-pos, epos)));
        pt-debug("parse-template: No tag-start, returning epos = %d.", epos);
        return(epos);
      elseif (looking-at?("<!--", buffer, tag-start, epos))
        pt-debug("parse-template: Found HTML comment start. Skipping to end.");
        scan-pos := html-comment-end(buffer, tag-start + 4);
      elseif (end-tag & looking-at?(end-tag, buffer, tag-start, epos))
        // done parsing the body of a tag as a subtemplate
        iff(html-pos < tag-start,
            add-entry!(tmplt, substring(buffer, html-pos, tag-start)));
        pt-debug("parse-template: Found end tag %=. Returning %d.",
                  end-tag, tag-start + size(end-tag));
        return(tag-start + size(end-tag))
      else
        let (tag-prefix, taglib) = parse-tag-prefix(buffer, taglibs, tag-start + 1, epos);
        if (~tag-prefix)
          // tag-start points to '<' but not to a known tag prefix like "<%dsp:"
          scan-pos := tag-start + 1;
        else
          // ok, found a valid-looking tag prefix like "<%dsp:" in a known taglib.
          let directive? = (taglib = #"directive");
          iff(html-pos < tag-start,
              add-entry!(tmplt, substring(buffer, html-pos, tag-start)));
          let (call, has-body?, body-start)
            = parse-start-tag(page, buffer, tag-start,
                              iff(directive?, $%dsp-taglib, taglib),
                              taglibs, tag-prefix, directive?);
          pt-debug("parse-template: Done parsing start tag %s:%s.  body-start = %d.",
                    call.prefix, call.name, body-start);
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

// Parse an opening DSP tag like <xx:foo arg=blah ...> or <xx:foo .../>
// If an error occurs during parsing, a dummy tag is returned that will
// display a placeholder when the DSP page is rendered and a warning will
// be logged.
// @param buffer is the string containing the dsp tag.
// @param bpos is the index of (for example) "<prefix:" in buffer.
// @param prefix is e.g. "dsp".
// @param taglib is the taglib corresponding to prefix.
// @param taglibs are the taglibs in effect at the tag call site.
// @param directive? is true iff prefix is "%dsp".
define function parse-start-tag (page :: <dylan-server-page>,
                                 buffer :: <string>,
                                 bpos :: <integer>,
                                 taglib :: <taglib>,
                                 taglibs :: <stretchy-vector>,
                                 prefix :: <string>,
                                 directive?)
 => (tag-call :: <tag-call>, has-body?, body-start)
  let name-start = bpos + size(prefix) + 2;  // 2 for the < and : characters
  let epos = size(buffer);
  let name-end = end-of-word(buffer, name-start, epos);
  let name = copy-sequence(buffer, start: name-start, end: name-end);
  let tag = find-tag(taglib, name);
  let tag-call = if (directive? | tag)
                   make(<tag-call>,
                        name: name,
                        prefix: prefix,
                        tag: tag,
                        taglib: taglib,
                        taglibs: copy-sequence(taglibs))
                 else
                   log-dsp-warning("In template %=, the tag %= was not found.",
                                   as(<string>, page.source-location),
                                   name);
                   tag := $placeholder-tag;
                   make-dummy-tag-call(prefix, name);
                 end;
  // *tag-call* is bound here so that it will be the same during parsing
  // as it is during execution.  parse-tag-arg needs it.
  dynamic-bind (*tag-call* = tag-call)
    let (tag-args, has-body?, end-index) = extract-tag-args(buffer, name-end, epos, tag);
    tag-call.arguments := tag-args;
    when (has-body? & ~tag.allow-body?)
      log-dsp-warning("While parsing template %s, at position %=:"
                      " The %s:%s tag call should end with \"/>\" since this tag doesn't allow a body."
                      " The tag body will be processed anyway.",
                      as(<string>, page.source-location), bpos, prefix, name);
    end;
    values (tag-call, has-body?, end-index)
  end
end parse-start-tag;

define function end-of-word (buffer :: <string>, bpos :: <integer>, epos :: <integer>)
  local method delim? (char :: <character>) => (b :: <boolean>)
          char = '>' | whitespace?(char)
        end;
  min(char-position-if(delim?, buffer, bpos, epos),
      string-position(buffer, "/>", bpos, epos) | epos)
end;

// Parse the key1="val1" key2="val2" arguments from a call to a DSP tag.  Values may be
// quoted with either single or double quotes (or nothing, but quoting is recommended).
// There is no way to escape the quote characters.
// @return a sequence of pairs, each containing a symbol of the argument name and the parsed
//         argument value.
define method extract-tag-args
    (buffer :: <byte-string>, bpos :: <integer>, epos :: <integer>, tag :: false-or(<tag>))
 => (args :: <sequence>, has-body? :: <boolean>, body-start :: <integer>)
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
              values(key,
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
  // iterate once for each key/val pair
  iterate loop (start = skip-whitespace(buffer, bpos, epos),
                args = list())
    if (start >= epos)
      values(args, #f, epos)
    elseif (looking-at?(">", buffer, start, epos))
      values(args, #t, start + 1)
    elseif (looking-at?("/>", buffer, start, epos))
      values(args, #f, start + 2)
    else
      let (param, val, key/val-end) = extract-key/val(buffer, start);
      let ptype = param & tag & get-parameter-type(tag, param);
      loop(skip-whitespace(buffer, key/val-end, epos),
           iff(param,
               pair(param, pair(parse-tag-arg(param, val, ptype), args)),
               args));
    end if
  end iterate
end extract-tag-args;

define method respond-to-head
    (page :: <dylan-server-page>, request :: <request>, response :: <response>)
  //---*** TODO
end;


//// Configuration

define function auto-register-dylan-server-page
    (url :: <string>) => (responder :: <function>)
  register-page(url, make(<dylan-server-page>,
                          source: document-location(url)))
end;


