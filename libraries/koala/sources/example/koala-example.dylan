Module:    koala-example
Synopsis:  Examples of how to use the Koala HTTP server
Author:    Carl Gay

/*

Start this example project and go to /example/home.dsp and you will be led through a 
series of pages that demonstrate the features of Dylan Server Pages.  You should
be able to find the code corresponding to a particular URL by searching for that
URL in this file.

You write your web application as an executable that uses the koala HTTP server dll.
Look for the call to "start-server" in this file for an example of how to start up the
server.  Following are examples of various ways to define static and dynamic handlers
for web URIs.

Note that any URIs registered for dynamic pages hide URIs corresponding to files in
the document root.  The dynamic URI takes precedence.

*/

// Start the Koala server early because it sets configuration variables like *document-root*.
begin
  let args = application-arguments();
  let port = ((size(args) > 0 & string-to-integer(args[0]))
              | 7020);
  start-server(port: port);
end;


/// Responders -- the lowest level API for responding to a URI

// Responds to a single URI.
define responder test1 ("/test1")
    (request :: <request>,
     response :: <response>)
  select (request-method(request))
    #"get", #"post"
      => format(output-stream(response), "<html><body>This is a test.</body></html>");
  end;
end;



/// Page abstraction

// Slightly higher level than responders.  Gives you the convenience of not having to figure out
// whether it's a GET, POST, HEAD, request, and the ability to dispatch on your own page classes.
// Just define methods for respond-to-get, respond-to-post, and/or respond-to-head that dispatch
// on your page class.  Note that the default methods for GET and HEAD do nothing and the default
// method for POST calls the method for GET.

// This defines a <hello-world-page> class which is a subclass of <page>, and a variable
// called *hello-world-page* which is an instance of <hello-world-page>.  It associates
// the URI /hello with the *hello-world-page* instance.  The value for the uri: arg may
// also be a sequence of URIs if you want to associate the page with multiple URIs.
//
define page hello-world-page (<page>)
    (uri: "/hello-world",
     aliases: "/hello")
end;

// Respond to a GET for the /hello URI.  Note the use of do-query-values to find all the values
// passed in the URI (e.g., /hello?foo=1&bar=2).  You can also use get-query-value to get a 
// specific query value, and count-query-values can be used to find out how many there are.
// Note that respond-to-post automatically calls respond-to-get, unless you override it.
//
define method respond-to-get (page :: <hello-world-page>,
                              request :: <request>,
                              response :: <response>)
  let stream :: <stream> = output-stream(response);
  format(stream, "<html>\n<head><title>Hello World</title></head>\n<body>Hello there.<p>");
  format(stream, "%s<br>", if (count-query-values() > 0)
                             "Query values are:"
                           else
                             "No query values were passed in the URL."
                           end);
  do-query-values(method (key, val)
                    format(stream, "key = %s, val = %s<br>\n", key, val);
                  end);
  format(stream, "</body></html>");
end;


/// Dylan Server Pages

// Dylan Server Pages are also defined with the "define page" macro, but you
// also specify the source: argument which is a file that contains normal
// HTML plus DSP tags.  The default method for respond-to-get parses the DSP
// source file and displays it.  Any HTML is output directly to the output
// stream, and tags invoke the corresponding tag definition code.

// Note that the .dsp source file isn't necessarily under the *document-root*
// directory.

// Define a class that will be used for all our example pages.  If we define
// all our tags to be specialized on this class they can be used in any example page.
//
define class <example-page> (<dylan-server-page>)
end;

define constant $example-taglib
  = register-taglib("example", "ex");

define method logged-in?
    (page, request)
  let session = get-session(request);
  session & get-attribute(session, #"username");
end;

begin
  register-label("logged-in?", logged-in?);
end;


// A simple error reporting mechanism.  Store errors in the page context
// so they can be displayed when the next page is generated.  The idea is
// that pages should use the <dsp:show-errors/> tag if they can be
// the target of a POST that might generate errors.

define method note-form-error
    (message :: <string>, #rest args)
  note-form-error(list(message, copy-sequence(args)));
end;

define method note-form-error
    (error :: <sequence>, #rest args)
  let context :: <page-context> = page-context();
  let errors = get-attribute(context, #"errors") | make(<stretchy-vector>);
  add!(errors, error);
  set-attribute(context, #"errors", errors);
end;

define tag show-errors in $example-taglib
    (page :: <example-page>, response :: <response>, #key)
  let errors = get-attribute(page-context(), #"errors");
  when (errors)
    let out = output-stream(response);
    format(out, "<FONT color='red'>Please fix the following errors:<P>\n<UL>\n");
    for (err in errors)
      // this is pretty consy
      format(out, "<LI>%s\n",
             quote-html(apply(format-to-string, first(err), second(err))));
    end;
    format(out, "</UL></FONT>\n");
  end;
end;


// Even though the home page doesn't use any DSP features we have to
// define a page for it since it's not under the *document-root*.
//
define page example-home-page (<example-page>)
    (uri: "/example/home.dsp",
     aliases: "/example",
     source: document-location("example/home.dsp"))
end;

define page example-login-page (<example-page>)
    (uri: "/example/login.dsp",
     source: document-location("example/login.dsp"))
end;

define page example-logout-page (<example-page>)
    (uri: "/example/logout.dsp",
     source: document-location("example/logout.dsp"))
end;

define method respond-to-get (page :: <example-logout-page>,
                              request :: <request>,
                              response :: <response>)
  let session = get-session(request);
  remove-attribute(session, #"username");
  remove-attribute(session, #"password");
  next-method();  // Must call this if you want the DSP template to be processed.
end;

// The login page POSTs to the welcome page...
define page example-welcome-page (<example-page>)
    (uri: "/example/welcome.dsp",
     source: document-location("example/welcome.dsp"))
end;

// ...so handle the POST by storing the form values in the session.
define method respond-to-post (page :: <example-welcome-page>,
                               request :: <request>,
                               response :: <response>)
  let username = get-query-value("username");
  let password = get-query-value("password");
  let username-supplied? = username & username ~= "";
  let password-supplied? = password & password ~= "";
  if (username-supplied? & password-supplied?)
    let session = get-session(request);
    set-attribute(session, #"username", username);
    set-attribute(session, #"password", password);
    next-method();  // process the DSP template for the welcome page.
  else
    note-form-error("You must supply both a username and password.");
    // ---*** TODO: Calling respond-to-get probably isn't quite right.
    // If we're redirecting to another page should the query/form values
    // be cleared first?  Probably want to call process-page instead,
    // but with the existing request?
    respond-to-get(*example-login-page*, request, response);
  end;
end;

// Note this tag is defined on <example-page> so it can be accessed from any
// page in this example web application.
define tag current-username in $example-taglib
    (page :: <example-page>, response :: <response>, #key request :: <request>)
  let username
    = get-form-value("username")
      | get-attribute(get-session(request), #"username");
  username & write(output-stream(response), username);
end;


//// iterator

define page iterator-page (<example-page>)
    (uri: "/example/iterator.dsp",
     source: document-location("example/iterator.dsp"))
end;

define thread variable *repetition-number* = 0;

// An iterating tag.  Note the use of the "body" modifier in "define body tag".
// When this modifier is used the tag accepts an extra argument, in this case
// called "do-body".  do-body is a function of zero arguments that will execute
// the body of the tag.  It may be invoked any number of times.  Use
// variables or object state to communicate with the tags that are executed
// during the execution of the body part.  Note the use of get-query-value to
// get the argument "n" that can be passed in the URL or in the POST.
// See iterator.dsp for how this tag is invoked.
//
define body tag repeat in $example-taglib
    (page :: <example-page>, response :: <response>, do-body :: <function>, #key)
  let n-str = get-query-value("n");
  let n = (n-str & string-to-integer(n-str)) | 5;
  for (i from 1 to n)
    dynamic-bind (*repetition-number* = i)
      do-body();
    end;
  end;
end;

define tag display-iteration-number in $example-taglib
    (page :: <example-page>, response :: <response>, #key)
  format(output-stream(response), "%d", *repetition-number*);
end;

//--------------old example code--------------------------------------------

define page dsp-test-page (<dylan-server-page>)
    (uri: "/test.dsp",
     source: document-location("example/test.dsp"))
end;

// Defines a tag that looks like <dsp:hello/> in the DSP source file.  i.e.,
// it has no body.
define tag hello in $example-taglib
    (page :: <dsp-test-page>, response :: <response>, #key)
  format(output-stream(response), "Hello, cruel world!");
end;


define tag show-keys in $example-taglib
    (page :: <dsp-test-page>, response :: <response>, #key arg1, arg2)
  format(output-stream(response),
         "The value of arg1 is %=.  The value of arg2 is %=.", arg1, arg2);
end;


// HTML table generation

// Maps table names (strings) to functions that take no args and return several values:
// (1) The number of rows in the table. (2) a function to be called to process the body
// of the table element.  The function will be passed
define variable *table-name-map* :: <string-table> = make(<string-table>);

begin
  *table-name-map*["blah-table"] := method () 4 end;
  *table-name-map*["zero-table"] := method () 0 end;
end;

define variable *iteration-number* :: <integer> = -1;
define variable *iteration-sequence* :: <sequence> = #[];

// I would call this "iterate" but it conflicts with the existing Dylan iterate macro.
define body tag iterator in $example-taglib
    (page :: <dylan-server-page>,
     response :: <response>,
     do-body :: <function>,
     #key name)
  let iterations-fun = name & element(*table-name-map*, name, default: #f);
  if (iterations-fun)
    let n-rows :: <integer> = iterations-fun();
    for (i from 1 to n-rows)
      dynamic-bind (*iteration-number* = i)
        do-body();
      end;
    end;
  else
    // For debugging.  Should probably remove this.   
    format(output-stream(response),
           "<!-- no iteration function found for %s iterator. -->\n",
           name);
  end;
end;

define body tag no-iterations in $example-taglib
    (page :: <dylan-server-page>,
     response :: <response>,
     do-body :: <function>,
     #key name)
  let iterations-fun = name & element(*table-name-map*, name, default: #f);
  if (iterations-fun)
    let n-rows :: <integer> = iterations-fun();
    when (n-rows == 0)
      do-body();
    end;
  else
    format(output-stream(response),
           "<!-- no iteration function found for %s iterator. -->\n",
           name);
  end;
end tag no-iterations;

define tag iteration-number in $example-taglib
    (page :: <dylan-server-page>, response :: <response>, #key)
  format(output-stream(response), "%d", *iteration-number*);
end;

define tag row-bgcolor in $example-taglib
    (page :: <dylan-server-page>, response :: <response>, #key)
  write(output-stream(response),
        if(even?(*iteration-number*)) "#EEEEEE" else "#FFFFFF" end);
end;

define tag demo-sessions in $example-taglib
    (page :: <dsp-test-page>, response :: <response>, #key request :: <request>)
  let session :: <session> = get-session(request);
  let x = get-attribute(session, #"xxx");
  format(output-stream(response), "The value of session attribute xxx is %=.", x);
  set-attribute(session, #"xxx", (x & x + 1) | 0);
end;



/// Main

// Starts up the web server with the specified port.  Loop sleeping forever so the application
// doesn't exit.  (Need to figure out how to make this unnecessary.)
define function main () => ()
  while (#t)
    sleep(1);
  end;
end;

begin
  main();
end;


