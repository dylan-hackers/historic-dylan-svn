Module: koala-test-suite

define constant $header :: <string> = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n";

// Make an XML document string that contains the given string.
define function koala-doc
    (content :: <string>) => (doc :: <string>)
  concatenate($header, "<koala>\n", content, "\n</koala>\n")
end;

// Try to configure a server with the given document (a string containing
// a Koala XML configuration description).  
define function configure
    (configuration :: <string>)
 => (server :: <http-server>)
  let server = make(<http-server>);
  configure-from-string(server, configuration);
  server
end;
  
define test basic-configuration-test ()
  check-no-errors("empty file",
                  configure(""));
  check-no-errors("Empty <koala> element",
                  configure(koala-doc("")));
  check-no-errors("Unknown element ignored",
                  configure(koala-doc("<unknown></unknown>")));
end test basic-configuration-test;

define suite configuration-test-suite ()
  test basic-configuration-test;
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
  <directory pattern = "/"
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
