Module:    koala-basics
Synopsis:  Examples of how to use the Koala HTTP server
Author:    Carl Gay

/*

As distributed, the examples in this file should be available starting at
/demo/home.dsp.  Just start koala-app.exe and this example should be loaded
as a Koala module.

Each page demonstrates a feature of Dylan Server Pages.  You should be able
to find the code corresponding to a particular URL by searching for that
URL in this file.  Some XML-RPC methods are defined near the bottom.

During development it's much easier to write your web application as an executable
that uses the koala HTTP server project, since you can run and debug it directly 
under Functional Developer.  To do this, you'll need to call start-server to get
the HTTP server started up.  (See "UNCOMMENT THIS" below.)

In production the idea is to run koala-app.exe, build your web app as a DLL, and
put the DLL in the koala-app "modules" directory.  This way multiple web apps can
be run on the same server.  Of course, you may not need multiple apps...

Following are examples of various ways to define static and dynamic handlers
for web URLs.

Note that any URLs registered for dynamic pages hide URLs corresponding to files in
the document root directory.  i.e., the dynamic URL takes precedence.

*/

//// URLs

// Using relative URLs in a web application strikes me as asking for trouble
// in general.  For example, if you register an alias URL "/demo" for a page
// "/demo/home.dsp" then URLs that appear in the home.dsp template would be
// relative to / when the page is processed, rather than relative to /demo/.
// So always use absolute URLs.

define constant $demo-base-url :: <byte-string> = "/demo";

// This is used throughout this file to create absolute URLs.
define function demo-url
    (url :: <byte-string>) => (absolute-url :: <byte-string>)
  concatenate($demo-base-url, url)
end;



//// Responders -- the lowest level API for responding to a URL

// Responds to a single URL.
define responder responder1 (demo-url("/responder1"))
  select (request-method(current-request()))
    #"get", #"post"
      => format(output-stream(current-response()),
                "<html><body>This is the output of a 'define responder' form."
                "<p>Use your browser's Back button to return to the example."
                "</body></html>");
  end;
end;

// Responds to a single directory (i.e., prefix) URL.
define directory responder dir1 ("/dir1")
  let request = current-request();
  select (request.request-method)
    #"get", #"post"
      => format(output-stream(current-response()),
                "<html><body>This is a directory responder.  The part of the url after "
                "the directory was %s."
                "<p>Use your browser's Back button to return to the example."
                "</body></html>",
                request.request-url-tail);
  end;
end;



//// Page abstraction

// Slightly higher level than responders.  Gives you the convenience of not
// having to figure out whether it's a GET, POST, HEAD, request, and the ability
// to dispatch on your own page classes.  Just define methods for
// respond-to that dispatch on your page class.  Note that the default methods
// for GET and HEAD do nothing and the default method for POST calls the method
// for GET.

// This defines a <hello-world-page> class which is a subclass of <page>, and a
// variable called *hello-world-page* which is an instance of
// <hello-world-page>.  It associates the URLs /hello-world and /hello with the
// *hello-world-page* instance.
//
define page hello-world-page (<page>)
    (url: demo-url("/hello-world"),
     alias: demo-url("/hello"))
end;

// Respond to a GET for <hello-world-page>.  Note the use of do-query-values to
// find all the values passed in the URL (e.g., /hello?foo=1&bar=2).  You can
// also use get-query-value to get a specific query value, and
// count-query-values can be used to find out how many there are.  Note that
// respond-to(#"post", ...) automatically calls respond-to(#"get", unless you
// override it.
//
define method respond-to
    (request-method == #"get", page :: <hello-world-page>)
  let stream :: <stream> = output-stream(current-response());
  format(stream, "<html>\n<head><title>Hello World</title></head>\n"
                 "<body>Hello there.<p>");
  format(stream, "%s<br>", if (count-query-values() > 0)
                             "Query values are:"
                           else
                             "No query values were passed in the URL."
                           end);
  do-query-values(method (key, val)
                    format(stream, "key = %s, val = %s<br>\n", key, val);
                  end);
  format(stream, "<p>Use your browser's Back button to return to the demo.</body></html>");
end;


//// Dylan Server Pages

// Dylan Server Pages are also defined with the "define page" macro, but you
// also specify the source: argument which is a file that contains normal
// HTML plus DSP tags.  The default method for respond-to GET parses the DSP
// source file and displays it.  Any HTML is output directly to the output
// stream, and tags invoke the corresponding tag definition code.

// Note that the .dsp source file doesn't have to be under the *document-root*
// directory.

// Define a class that will be used for all our example pages.  It must be a
// subclass of <dylan-server-page> so that all the template parsing will happen.
// If we define all our tags to be specialized on this class they can be used
// in any example page.
//
define class <demo-page> (<dylan-server-page>)
end;

// Define a tag library in which to put all tag definitions.  This isn't
// strictly necessary; tag defs can go in the existing 'dsp' tag library
// but then you run the risk of overriding built-in DSP tags or other
// user-defined tags in the dsp taglib.
//
define taglib demo ()
end;

define tag base-url in demo
    (page :: <demo-page>)
    ()
  format(output-stream(current-response()), $demo-base-url);
end;

define page home-page (<demo-page>)
    (url: demo-url("/home.dsp"),
     alias: "/demo/",
     source: "demo/home.dsp")
end;

define page hello-page (<demo-page>)
    (url: demo-url("/hello.dsp"),
     source: "demo/hello.dsp")
end;

// Defines a tag that looks like <demo:hello/> in the DSP source file.  i.e.,
// it has no body.
define tag hello in demo
    (page :: <demo-page>)
    ()
  format(output-stream(current-response()), "Hello, world!");
end;

define page args-page (<demo-page>)
    (url: demo-url("/args.dsp"),
     source: "demo/args.dsp")
end;

// This tag demonstrates the use of tag keyword arguments.  The tag call looks
// like this:  <demo:show-keys arg1="100" arg2="foo"/>
// Note that since arg1 is typed as an <integer> it is automatically parsed to
// an <integer>.  To define new tag argument parsers, add methods to the
// parse-tag-arg generic.
//
define tag show-keys in demo
    (page :: <demo-page>)
    (arg1 :: <integer>, arg2)
  format(output-stream(current-response()),
         "The value of arg1 + 1 is %=.  The value of arg2 is %=.",
         arg1 + 1, arg2);
end;


define named-method logged-in? in demo
    (page :: <demo-page>)
  let session = get-session(current-request());
  session & get-attribute(session, #"username");
end;

define page example-login-page (<demo-page>)
    (url: demo-url("/login.dsp"),
     source: "demo/login.dsp")
end;

define page example-logout-page (<demo-page>)
    (url: demo-url("/logout.dsp"),
     source: "demo/logout.dsp")
end;

define method respond-to
    (request-method == #"get", page :: <example-logout-page>)
  let session = get-session(current-request());
  remove-attribute(session, #"username");
  remove-attribute(session, #"password");
  next-method();  // Must call this if you want the DSP template to be processed.
end;

// The login page POSTs to the welcome page...
define page example-welcome-page (<demo-page>)
    (url: demo-url("/welcome.dsp"),
     source: "demo/welcome.dsp")
end;

// ...so handle the POST by storing the form values in the session.
define method respond-to
    (request-method == #"post", page :: <example-welcome-page>)
  let username = get-query-value("username");
  let password = get-query-value("password");
  let username-supplied? = username & username ~= "";
  let password-supplied? = password & password ~= "";
  if (username-supplied? & password-supplied?)
    let session = get-session(current-request());
    set-attribute(session, #"username", username);
    set-attribute(session, #"password", password);
    next-method();  // process the DSP template for the welcome page.
  else
    note-form-error("You must supply <b>both</b> a username and password.");
    // ---*** TODO: Calling respond-to(#"get", ...) probably isn't quite right.
    // If we're redirecting to another page should the query/form values
    // be cleared first?  Probably want to call process-page instead,
    // but with the existing request?
    respond-to(#"get", *example-login-page*);
  end;
end;

// Note this tag is defined on <demo-page> so it can be accessed from any
// page in this example web application.
define tag current-username in demo
    (page :: <demo-page>)
    ()
  let response = current-response();
  let username
    = get-query-value("username")
      | get-attribute(get-session(get-request(response)), #"username");
  username & write(output-stream(response), username);
end;


//// iterator

define page iterator-page (<demo-page>)
    (url: demo-url("/iterator.dsp"),
     source: "demo/iterator.dsp")
end;

define thread variable *repetition-number* = 0;

// An iterating tag.  Note the use of the "body" modifier in "define body tag".
// When this modifier is used the tag accepts a third argument, in this case
// called "do-body".  do-body is a function of zero arguments that will execute
// the body of the tag.  It may be invoked any number of times.  Use thread
// variables or object state to communicate with the tags that are invoked
// during the execution of the body part.  Note the use of get-query-value to
// get the argument "n" that can be passed in the URL or in the POST.
// See iterator.dsp for how this tag is invoked.
//
define body tag repeat in demo
    (page :: <demo-page>, do-body :: <function>)
    ()
  let n-str = get-query-value("n");
  let n = (n-str & string-to-integer(n-str)) | 5;
  for (i from 1 to n)
    dynamic-bind (*repetition-number* = i)
      do-body();
    end;
  end;
end;

define tag display-iteration-number in demo
    (page :: <demo-page>)
    ()
  format(output-stream(current-response()), "%d", *repetition-number*);
end;


//// table generation

define page table-page (<demo-page>)
    (url: demo-url("/table.dsp"),
     source: "demo/table.dsp")
end;

// This method is used as the row-generator function for a dsp:table call.
// It must return a <sequence>.
define named-method animal-generator in demo
    (page :: <table-page>)
  #[#["dog", "perro", "gou3"],
    #["cat", "gato", "mao1"],
    #["cow", "vaca", "niu2"]]
end;

// The row-generator for the table with no rows.
define named-method no-rows-generator in demo
    (page :: <table-page>)
  #[]
end;

define tag english-word in demo
    (page :: <demo-page>)
    ()
  let row = current-row();
  format(output-stream(current-response()), "%s", row[0]);
end;

define tag spanish-word in demo
    (page :: <demo-page>)
    ()
  let row = current-row();
  format(output-stream(current-response()), "%s", row[1]);
end;

define tag pinyin-word in demo
    (page :: <demo-page>)
    ()
  let row = current-row();
  format(output-stream(current-response()), "%s", row[2]);
end;

define tag row-bgcolor in demo
    (page :: <demo-page>)
    ()
  write(output-stream(current-response()),
        if(even?(current-row-number())) "#EEEEEE" else "#FFFFFF" end);
end;



//// XML-RPC (use any XML-RPC client to call these)

begin
  register-xml-rpc-method("test.zero",
                          method () end);
  register-xml-rpc-method("test.one",
                          method () 1 end);
  register-xml-rpc-method("test.two",
                          method () "two" end);
  register-xml-rpc-method("test.three",
                          method () vector(1, "two", 3.0) end);
  register-xml-rpc-method("test.four",
                          method ()
                            let result = make(<table>);
                            result["x"] := vector(vector(7), 8);
                            result["y"] := "my <dog> has fleas";
                            result
                          end);
end;




/// Main

begin
  // If you don't need to add any new command-line arguments you can just
  // call koala-main directly.  It requires that you pass --config <filename>
  // on the command line.
  koala-main();
end;

