Module: koala-test-suite

define constant $xml-header :: <string>
  = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n";

// Make an XML document string that contains the given string.
define function koala-document
    (content :: <string>) => (doc :: <string>)
  concatenate($xml-header, "<koala>\n", content, "\n</koala>\n")
end;

// Try to configure a server with the given document (a string containing
// a Koala XML configuration description).  
define function configure
    (configuration :: <string>)
 => (server :: <http-server>)
  let server = make(<http-server>);
  configure-from-string(server, configuration);
  server
end function configure;
  
define test basic-config-test ()
  let texts = #("",
                "<barbaloot>",
                "<koala>gubbish</koala>",
                "&!*#)!^%");
  for (text in texts)
    check-condition(fmt("Invalid config (%=) causes <configuration-error>", text),
                    <configuration-error>,
                    configure(text));
  end for;
  check-no-errors("Empty <koala> element",
                  configure(koala-document("")));
  check-no-errors("Unknown element ignored",
                  configure(koala-document("<unknown></unknown>")));
end test basic-config-test;

define test listener-config-test ()
  let texts = #(// valid
                "<listener address=\"123.45.67.89\" port=\"2222\" />",
                "<listener address=\"123.45.67.89\" />",
                "<listener port=\"2222\" />",
                // invalid
                // ideally i'd like these to signal a specific error.
                "<listener address=\"123.45.67.89\" port=\"xxx\" />",
                "<listener />",
                "<listener address=\"xxx\" port=\"2222\" />");
  for (text in texts)
    check-no-errors(text, configure(koala-document(text)));
  end;
end test listener-config-test;

define test alias-config-test ()
  let server = make-server();
  add-responder(server, "/abc", echo-responder);
  let text = "<koala><alias url=\"/def\" target=\"/abc\"/></koala>";
  configure-from-string(server, text);
  with-http-server (server = server)
    with-http-connection(conn = test-url("/"))
      send-request(conn, "GET", "/def");
      let response = read-response(conn, follow-redirects: #f);
      check-equal("/def returned response code 301?",
                  response.response-code, 301);
      check-equal("/def was redirected to /abc?",
                  get-header(response, "Location"), "/abc");
    end;
  end;
end test alias-config-test;

// Verify that the <document-root> setting is respected, by setting it
// to the directory containing application-filename() and then requesting
// the executable file.
define test test-document-root ()
  let app = as(<file-locator>, application-filename());
  let dir = as(<string>, locator-directory(app));
  let text = fmt("<directory url=\"/\" location=\"%s\" allow-static=\"yes\"/>\n", dir);
  let server = make-server();  // includes default listener
  configure-from-string(server, koala-document(text));
  with-http-server (server = server)
    let app-url = test-url(concatenate("/", locator-name(app)));
    with-http-connection (conn = app-url)
      send-request(conn, "GET", app-url);
      check-no-errors("<document-root> is respected?", read-response(conn));
    end;
  end;
end test test-document-root;

define suite configuration-test-suite ()
  test basic-config-test;
  test listener-config-test;
  test alias-config-test;
  test test-document-root;
end;


/*
<koala>
  <debug-server value="off" />
  <log type="debug"
       location="c:/cgay/dylan/debug.log"
       level="debug"
       max-size="20000000" />
  <log type="activity"
       location="c:/cgay/dylan/activity.log"
       max-size="20000000" />
  <log type="error"
       location="c:/cgay/dylan/error.log"
       max-size="20000000" />
  <server-root location="c:/cgay/dylan" />
  <document-root location="www" />
  <dsp-root location="c:/cgay/dylan/trunk/libraries/network/koala/www" />
  <directory url = "/"
             path = "..."
             allow-cgi = "yes"
             follow-symlinks = "yes"
             allow-directory-listing = "yes" />
  <default-virtual-host enabled="yes"/>
  <listener address="0.0.0.0" port="8080" />
  <mime-type-map location="mime-type-map.xml" clear="true"/>
  <administrator
     email="you@your.domain"
     name="yourname" />
  <auto-register enabled="no" />
  <xml-rpc
    url="/RPC2"
    enable="yes"
    internal-error-fault-code="0"
    debug="no"
    />
  <virtual-host name="127.0.0.1">
      <document-root location = "www/127.0.0.1" />
      <directory pattern = "/"
                 allow-directory-listing = "no" />
  </virtual-host>
</koala>
*/
