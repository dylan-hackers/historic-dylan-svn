Module:    koala-example
Synopsis:  Examples of how to use the Koala HTTP server
Author:    Carl Gay

/*

You write your web application as an executable that uses the koala HTTP server dll.
See the "main" function at the end of this file for an example of how to start up the
server.  Following are examples of various ways to define static and dynamic handlers
for web URIs.

Note that any URIs registered for dynamic pages hide URIs corresponding to files in
the document root.  The dynamic URI takes precedence.

*/

/// A bit of utility code...

define variable *application-directory* :: <locator>
  = begin
      let exe-loc = as(<file-locator>, application-filename());
      let bin-directory = locator-directory(exe-loc);
      locator-directory(bin-directory)  // actually returns the parent directory
    end;

define function new-locator
    (path :: <string>) => (loc :: <locator>)
  merge-locators(as(<file-locator>, path), *application-directory*)
end;


/// Responders -- the lowest level API for responding to a URI

// Responds to a single URI.
define responder test1 ("/test1")
    (request :: <request>, response :: <response>)
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
define page hello-world-page (<page>) (uri: list("/hello", "/hello-world"))
end;

// Respond to a GET for the /hello URI.  Note the use of do-query-values to find all the values
// passed in the URI (e.g., /hello?foo=1&bar=2).  You can also use get-query-value to get a 
// specific query value, and count-query-values can be used to find out how many there are.
// Note that respond-to-post automatically calls respond-to-get, unless you override it.
//
define method respond-to-get (page :: <hello-world-page>, request :: <request>, response :: <response>)
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

// Dylan Server Pages are also defined with the "define page" macro, but you also specify the
// source: argument which is a file that contains normal HTML plus DSP tags.  The default
// method for respond-to-get parses the DSP source file and displays it.  Any HTML is output
// directly to the output stream, and tags invoke the corresponding tag definition code.

define page dsp-test-page (<dylan-server-page>)
    (uri: "/test.dsp",
     source: new-locator("test.dsp"))
end;

// Defines a tag that looks like <dsp:hello/> in the DSP source file.  i.e., it has no body.
define tag hello
    (page :: <dsp-test-page>, request :: <request>, response :: <response>)
  ignore(page);
  format(output-stream(response), "Hello, cruel world!");
end;


define thread variable *repetition-number* = 0;

// An iterating tag.  Note that the only difference from a standard tag definition
// is that this one accepts a third argument (called "process-body" in this case).
// process-body is bound to a function that takes no args and will process the body
// of the iteration tag.  Use variables or object state to communicate with the
// tags that are executed during the execution of the body part.  See test.dsp
// for how this tag is invoked.
define tag repeat
    (page :: <dsp-test-page>, request :: <request>, response :: <response>, process-body :: <function>)
  let n-str = get-query-value("n");
  let n = (n-str & string-to-integer(n-str)) | 5;
  for (i from 1 to n)
    dynamic-bind (*repetition-number* = i)
      process-body();
    end;
  end;
end;

define tag display-iteration-number
    (page :: <dsp-test-page>, request :: <request>, response :: <response>)
  format(output-stream(response), "%d", *repetition-number*);
end;

define tag show-keys
    (page :: <dsp-test-page>, request :: <request>, response :: <response>, #key arg1, arg2)
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
define tag iterator
    (page :: <dylan-server-page>, request :: <request>, response :: <response>,
     process-body :: <function>, #key name)
  let iterations-fun = name & element(*table-name-map*, name, default: #f);
  if (iterations-fun)
    let n-rows :: <integer> = iterations-fun();
    for (i from 1 to n-rows)
      dynamic-bind (*iteration-number* = i)
        process-body();
      end;
    end;
  else
    // For debugging.  Should probably remove this.   
    format(output-stream(response),
           "<!-- no iteration function found for %s iterator. -->\n",
           name);
  end;
end;

define tag no-iterations
    (page :: <dylan-server-page>, request :: <request>, response :: <response>,
     process-body :: <function>, #key name)
  let iterations-fun = name & element(*table-name-map*, name, default: #f);
  if (iterations-fun)
    let n-rows :: <integer> = iterations-fun();
    when (n-rows == 0)
      process-body();
    end;
  else
    format(output-stream(response),
           "<!-- no iteration function found for %s iterator. -->\n",
           name);
  end;
end tag no-iterations;

define tag iteration-number
    (page :: <dylan-server-page>, request :: <request>, response :: <response>)
  format(output-stream(response), "%d", *iteration-number*);
end;

define tag row-bgcolor
    (page :: <dylan-server-page>, request :: <request>, response :: <response>)
  write(output-stream(response),
        if(even?(*iteration-number*)) "#EEEEEE" else "#FFFFFF" end);
end;

define tag demo-sessions
    (page :: <dsp-test-page>, request :: <request>, response :: <response>)
  let session :: <session> = get-session(request);
  let x = get-attribute(session, #"xxx");
  format(output-stream(response), "The value of session attribute xxx is %=.", x);
  set-attribute(session, #"xxx", (x & x + 1) | 0);
end;

define page login-page (<dylan-server-page>)
    (uri: "/login.dsp",
     source: new-locator("login.dsp"))
end;

define tag current-username
    (page :: <login-page>, request :: <request>, response :: <response>)
  let username = get-attribute(get-session(request), #"username");
  username & write(output-stream(response), username);
end;

define method respond-to-post
    (page :: <dsp-test-page>, request :: <request>, response :: <response>)
  let username = get-query-value("username");
  let password = get-query-value("password");
  when (username | password)
    let session = get-session(request);
    set-attribute(session, #"username", username);
    set-attribute(session, #"password", password);
  end;
  next-method();  // process the DSP template
end;

define tag maybe-display-welcome
    (page :: <dsp-test-page>, request :: <request>, response :: <response>)
  let username = get-attribute(get-session(request), #"username");
  when (username)
    format(output-stream(response), "<h2>Welcome %s!</h2>\n", username);
  end;
end;
  


/// Main

// Starts up the web server with the specified port.  Loop sleeping forever so the application
// doesn't exit.  (Need to figure out how to make this unnecessary.)
define function main () => ()
  let args = application-arguments();
  let port = ((size(args) > 0 & string-to-integer(args[0]))
              | 7020);
  start-server(port: port);
  while (#t)
    sleep(1);
  end;
end;

begin
  main();
end;


