Module:    lib-curl
Synopsis:  An FFI wrapper for Curl.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// Setup types
define constant <curl> = <c-void*>;
define constant <curl-option> = <c-int>;
define constant <curl-code> = <c-int>;

// Error codes
define constant $curle-ok = 0;
define constant $curle-unsupported-protocol = 1;
define constant $curle-failed-init = 2;
define constant $curle-url-malformat = 3;
define constant $curle-url-maxlformat-user = 4;
define constant $curle-couldnt-resolve-proxy = 5;
define constant $curle-couldnt-resolve-host = 6;
define constant $curle-couldnt-connect = 7;
define constant $curle-ftp-weird-server-reply = 8;
define constant $curle-ftp-access-denied = 9;
define constant $curle-ftp-user-password-incorrect = 10;
define constant $curle-ftp-weird-pass-reply = 11;
define constant $curle-ftp-weird-user-reply = 12;
define constant $curle-ftp-weird-pasv-reply = 13;
define constant $curle-ftp-weird-227-reply = 14;
define constant $curle-ftp-cant-get-host = 15;
define constant $curle-ftp-cant-reconnect = 16;
define constant $curle-ftp-couldnt-set-binary = 17;
define constant $curle-partial-file = 18;
define constant $curle-ftp-couldnt-retr-file = 19;
define constant $curle-ftp-write-error = 20;
define constant $curle-ftp-quote-error = 21;
define constant $curle-http-not-found = 22;
define constant $curle-write-error = 23;
define constant $curle-malformat-user = 24;
define constant $curle-ftp-couldnt-stor-file = 25;
define constant $curle-read-error = 26;
define constant $curle-out-of-memory = 27;
define constant $curle-operation-timeouted = 28;
define constant $curle-ftp-couldnt-set-ascii = 29;
define constant $curle-ftp-port-failed = 30;
define constant $curle-ftp-couldnt-user-rest = 31;
define constant $curle-ftp-couldnt-get-size = 32;
define constant $curle-http-range-error = 33;
define constant $curle-http-post-error = 34;
define constant $curle-ssl-connect-error = 35;
define constant $curle-ftp-bad-download-resume = 36;
define constant $curle-file-couldnt-read-file = 37;
define constant $curle-ldap-cannot-bind = 38;
define constant $curle-ldap-search-failed = 39;
define constant $curle-library-not-found = 40;
define constant $curle-function-not-found = 41;
define constant $curle-aborted-by-callback = 42;
define constant $curle-bad-function-argument = 43;
define constant $curle-bad-calling-order = 44;
define constant $curle-http-port-failed = 45;
define constant $curle-bad-password-entered = 46;
define constant $curle-too-many-redirects = 47;
define constant $curle-unknown-telnet-option = 48;
define constant $curle-telnet-option-syntax = 49;
define constant $curle-obsolete = 50;
define constant $curle-ssl-peer-certificate = 51;

// Curl Option types
define constant $curlopttype-long = 0;
define constant $curlopttype-objectpoint = 10000;
define constant $curlopttype-functionpoint = 20000;

define macro cinit
{ cinit(?name:name, ?type:name, ?number:expression) }
=> { define constant "$curlopt-" ## ?name = "$curlopttype-" ## ?type + ?number }
end macro cinit;

// The first one is unused
cinit(nothing, long, 0);

// This is the FILE * or void * the regular output should be written to.
cinit(file, objectpoint, 1);

// The full URL to get/put 
cinit(url,  objectpoint, 2);

// Port number to connect to, if other than default. Specify the CONF_PORT
// flag in the CURLOPT_FLAGS to activate this 
cinit(port, long, 3);

// Name of proxy to use. Specify the CONF_PROXY flag in the CURLOPT_FLAGS to
// activate this 
cinit(proxy, objectpoint, 4);
  
// Name and password to use when fetching. Specify the CONF_USERPWD flag in
// the CURLOPT_FLAGS to activate this 
cinit(userpwd, objectpoint, 5);

// Name and password to use with Proxy. Specify the CONF_PROXYUSERPWD 
// flag in the CURLOPT_FLAGS to activate this 
cinit(proxyuserpwd, objectpoint, 6);

// Range to get, specified as an ASCII string. Specify the CONF_RANGE flag
// in the CURLOPT_FLAGS to activate this
cinit(range, objectpoint, 7);

// Specified file stream to upload from (use as input): 
cinit(infile, objectpoint, 9);

// Buffer to receive error messages in, must be at least CURL_ERROR_SIZE
// * bytes big. If this is not used, error messages go to stderr instead: 
cinit(errorbuffer, objectpoint, 10);

// Function that will be called to store the output (instead of fwrite). The
// * parameters will use fwrite() syntax, make sure to follow them. 
cinit(writefunction, functionpoint, 11);

// Function that will be called to read the input (instead of fread). The
// * parameters will use fread() syntax, make sure to follow them. 
cinit(readfunction, functionpoint, 12);

// Time-out the read operation after this amount of seconds 
cinit(timeout, long, 13);

// If the CURLOPT_INFILE is used, this can be used to inform libcurl about
//   * how large the file being sent really is. That allows better error
//   * checking and better verifies that the upload was succcessful. -1 means
//   * unknown size. 
cinit(infilesize, long, 14);

// POST input fields. 
cinit(postfields, objectpoint, 15);

// Set the referer page (needed by some CGIs) 
cinit(referer, objectpoint, 16);

// Set the FTP PORT string (interface name, named or numerical IP address)
//     Use i.e '-' to use default address. 
cinit(ftpport, objectpoint, 17);

// Set the User-Agent string (examined by some CGIs) 
cinit(useragent, objectpoint, 18);

// If the download receives less than "low speed limit" bytes/second
//   * during "low speed time" seconds, the operations is aborted.
//   * You could i.e if you have a pretty high speed connection, abort if
//   * it is less than 2000 bytes/sec during 20 seconds.   

// Set the "low speed limit" 
cinit(low-speed-limit, long , 19);

// Set the "low speed time" 
cinit(low-speed-time, long, 20);

// Set the continuation offset 
cinit(resume-from, long, 21);

// Set cookie in request: 
cinit(cookie, objectpoint, 22);

// This points to a linked list of headers, struct HttpHeader kind 
cinit(httpheader, objectpoint, 23);

// This points to a linked list of post entries, struct HttpPost 
cinit(httppost, objectpoint, 24);

// name of the file keeping your private SSL-certificate 
cinit(sslcert, objectpoint, 25);

// password for the SSL-certificate 
cinit(sslcertpasswd, objectpoint, 26);
  
// send TYPE parameter? 
cinit(crlf, long, 27);

// send linked-list of QUOTE commands 
cinit(quote, objectpoint, 28);

// send FILE * or void * to store headers to, if you use a callback it
//     is simply passed to the callback unmodified 
cinit(writeheader, objectpoint, 29);

// send linked list of MoreDoc structs 
cinit(moredocs, objectpoint, 30);

// point to a file to read the initial cookies from, also enables
//     "cookie awareness" 
cinit(cookiefile, objectpoint, 31);

// What version to specifly try to use.
//     3 = SSLv3, 2 = SSLv2, all else makes it try v3 first then v2 
cinit(sslversion, long, 32);

// What kind of HTTP time condition to use, see defines 
cinit(timecondition, long, 33);

// Time to use with the above condition. Specified in number of seconds
//     since 1 Jan 1970 
cinit(timevalue, long, 34);

// Custom request, for customizing the get command like
//     HTTP: DELETE, TRACE and others
//     FTP: to use a different list command
cinit(customrequest, objectpoint, 36);

// HTTP request, for odd commands like DELETE, TRACE and others 
cinit(stderr, objectpoint, 37);

// send linked-list of post-transfer QUOTE commands 
cinit(postquote, objectpoint, 39);

// Pass a pointer to string of the output using full variable-replacement
//     as described elsewhere. 
cinit(writeinfo, objectpoint, 40);

// Previous FLAG bits 
cinit(verbose, long, 41);      /* talk a lot */
cinit(header, long, 42);       /* throw the header out too */
cinit(noprogress, long, 43);   /* shut off the progress meter */
cinit(nobody, long, 44);       /* use HEAD to get http document */
cinit(failonerror, long, 45);  /* no output on http error codes >= 300 */
cinit(upload, long, 46);       /* this is an upload */
cinit(post, long, 47);         /* HTTP POST method */
cinit(ftplistonly, long, 48);  /* Use NLST when listing ftp dir */

cinit(ftpappend, long, 50);    /* Append instead of overwrite on upload! */
cinit(netrc, long, 51);        /* read user+password from .netrc */
cinit(followlocation, long, 52);  /* use Location: Luke! */

  /* This FTPASCII name is now obsolete, to be removed, use the TRANSFERTEXT
     instead. It goes for more protocols than just ftp... */
cinit(ftpascii, long, 53);     /* use TYPE A for transfer */

cinit(transfertext, long, 53); /* transfer data in text/ASCII format */
cinit(put, long, 54);          /* PUT the input file */

cinit(mute, long, 55);         /* OBSOLETE OPTION, removed in 7.8 */

  /* Function that will be called instead of the internal progress display
   * function. This function should be defined as the curl_progress_callback
   * prototype defines. */
cinit(progressfunction, functionpoint, 56);

  /* Data passed to the progress callback */
cinit(progressdata, objectpoint, 57);

  /* We want the referer field set automatically when following locations */
cinit(autoreferer, long, 58);

  /* Port of the proxy, can be set in the proxy string as well with:
     "[host]:[port]" */
cinit(proxyport, long, 59);

  /* size of the POST input data, if strlen() is not good to use */
cinit(postfieldsize, long, 60);

  /* tunnel non-http operations through a HTTP proxy */
cinit(httpproxytunnel, long, 61);

  /* Set the interface string to use as outgoing network interface */
cinit(interface, objectpoint, 62);

  /* Set the krb4 security level, this also enables krb4 awareness.  This is a
   * string, 'clear', 'safe', 'confidential' or 'private'.  If the string is
   * set but doesn't match one of these, 'private' will be used.  */
cinit(krb4level, objectpoint, 63);

  /* Set if we should verify the peer in ssl handshake, set 1 to verify. */
cinit(ssl-verifypeer, long, 64);
  
  /* The CApath or CAfile used to validate the peer certificate
     this option is used only if SSL_VERIFYPEER is true */
cinit(cainfo, objectpoint, 65);

  /* Function pointer to replace the internal password prompt */
cinit(passwdfunction, functionpoint, 66);

  /* Custom pointer that gets passed as first argument to the password
     function */
cinit(passwddata, objectpoint, 67);
  
  /* Maximum number of http redirects to follow */
cinit(maxredirs, long, 68);

  /* Pass a pointer to a time_t to get a possible date of the requested
     document! Pass a NULL to shut it off. */
cinit(filetime, objectpoint, 69);

  /* This points to a linked list of telnet options */
cinit(telnetoptions, objectpoint, 70);

  /* Max amount of cached alive connections */
cinit(maxconnects, long, 71);

  /* What policy to use when closing connections when the cache is filled
     up */
cinit(closepolicy, long, 72);

  /* Callback to use when CURLCLOSEPOLICY_CALLBACK is set */
cinit(closefunction, functionpoint, 73);

  /* Set to explicitly use a new connection for the upcoming transfer.
     Do not use this unless you're absolutely sure of this, as it makes the
     operation slower and is less friendly for the network. */
cinit(fresh-connect, long, 74);

  /* Set to explicitly forbid the upcoming transfer's connection to be re-used
     when done. Do not use this unless you're absolutely sure of this, as it
     makes the operation slower and is less friendly for the network. */
cinit(forbid-reuse, long, 75);

  /* Set to a file name that contains random data for libcurl to use to
     seed the random engine when doing SSL connects. */
cinit(random-file, objectpoint, 76);

  /* Set to the Entropy Gathering Daemon socket pathname */
cinit(egdsocket, objectpoint, 77);

  /* Time-out connect operations after this amount of seconds, if connects
     are OK within this time, then fine... This only aborts the connect
     phase. [Only works on unix-style/SIGALRM operating systems] */
cinit(connecttimeout, long, 78);

  /* Function that will be called to store headers (instead of fwrite). The
   * parameters will use fwrite() syntax, make sure to follow them. */
cinit(headerfunction, functionpoint, 79);

// Curl functions
define c-function curl-easy-init
  result value :: <curl>;
  c-name: "curl_easy_init";
end c-function curl-easy-init;

define c-function curl-easy-cleanup
  input parameter handle :: <curl>;
  c-name: "curl_easy_cleanup";
end c-function curl-easy-cleanup;

define generic curl-easy-setopt(handle :: <curl>, option :: <integer>, #rest args)
 => (r :: <integer>);

define macro easy-setopt-definer 
{ define easy-setopt ?:name
    ?params:*
  end }
=> { define c-function "curl-easy-setopt-" ## ?name
       input parameter handle :: <curl>;
       input parameter option :: <curl-option>;
       ?params
       result value :: <c-int>;
       c-name: "curl_easy_setopt";
     end c-function;

     define method curl-easy-setopt(handle :: <curl>, 
                                    option == "$curlopt-" ## ?name,
                                    #rest args) 
      => (r :: <integer>)
       apply("curl-easy-setopt-" ## ?name, handle, option, args);
     end method curl-easy-setopt;
   }
end macro easy-setopt-definer;

define easy-setopt file
  input parameter pointer :: <c-void*>; 
end easy-setopt file;

define easy-setopt writefunction
  input parameter function :: <c-function-pointer>; 
end easy-setopt writefunction;

define easy-setopt infile
  input parameter pointer :: <c-void*>; 
end easy-setopt infile;

define easy-setopt readfunction
  input parameter function :: <c-function-pointer>; 
end easy-setopt readfunction;

define easy-setopt infilesize
  input parameter size :: <c-long>; 
end easy-setopt infilesize;

define easy-setopt url
  input parameter url :: <c-string>; 
end easy-setopt url;

define easy-setopt proxy
  input parameter proxy :: <c-string>; 
end easy-setopt proxy;

define easy-setopt proxyport
  input parameter port :: <c-long>; 
end easy-setopt proxyport;

define easy-setopt httpproxytunnel
  input parameter value :: <c-int>; 
end easy-setopt httpproxytunnel;

define easy-setopt verbose
  input parameter value :: <c-int>; 
end easy-setopt verbose;

define easy-setopt header
  input parameter value :: <c-int>; 
end easy-setopt header;

define easy-setopt noprogress
  input parameter value :: <c-int>; 
end easy-setopt noprogress;

define easy-setopt nobody
  input parameter value :: <c-int>; 
end easy-setopt nobody;

define easy-setopt failonerror
  input parameter value :: <c-int>; 
end easy-setopt failonerror;

define easy-setopt upload
  input parameter value :: <c-int>; 
end easy-setopt upload;

define easy-setopt post
  input parameter value :: <c-int>; 
end easy-setopt post;

define easy-setopt ftplistonly
  input parameter value :: <c-int>; 
end easy-setopt ftplistonly;

define easy-setopt ftpappend
  input parameter value :: <c-int>; 
end easy-setopt ftpappend;

define easy-setopt netrc
  input parameter value :: <c-int>; 
end easy-setopt netrc;

define easy-setopt followlocation
  input parameter value :: <c-int>; 
end easy-setopt followlocation;

define easy-setopt transfertext
  input parameter value :: <c-int>; 
end easy-setopt transfertext;

define easy-setopt userpwd
  input parameter userpwd :: <c-string>; 
end easy-setopt userpwd;

define easy-setopt proxyuserpwd
  input parameter userpwd :: <c-string>; 
end easy-setopt proxyuserpwd;

define easy-setopt errorbuffer
  input parameter buffer :: <c-string>; 
end easy-setopt errorbuffer;

define easy-setopt timeout
  input parameter value :: <c-long>; 
end easy-setopt timeout;

define easy-setopt postfields
  input parameter fields :: <c-string>; 
end easy-setopt postfields;

define easy-setopt postfieldsize
  input parameter value :: <c-int>; 
end easy-setopt postfieldsize;

define easy-setopt referer
  input parameter referer :: <c-string>; 
end easy-setopt referer;

define easy-setopt autoreferer
  input parameter referer :: <c-int>; 
end easy-setopt autoreferer;

define easy-setopt useragent
  input parameter useragent :: <c-string>; 
end easy-setopt useragent;

define easy-setopt ftpport
  input parameter port :: <c-string>; 
end easy-setopt ftpport;

define easy-setopt low-speed-limit
  input parameter limit :: <c-long>; 
end easy-setopt low-speed-limit;

define easy-setopt low-speed-time
  input parameter time :: <c-long>; 
end easy-setopt low-speed-time;

define easy-setopt resume-from
  input parameter from :: <c-long>; 
end easy-setopt resume-from;

define easy-setopt cookie
  input parameter cookie :: <c-string>; 
end easy-setopt cookie;

define easy-setopt httpheader
  input parameter list :: <c-void*>; 
end easy-setopt httpheader;

define easy-setopt httppost
  input parameter list :: <c-void*>; 
end easy-setopt httppost;

define easy-setopt sslcert
  input parameter filename :: <c-string>; 
end easy-setopt sslcert;

define easy-setopt sslcertpasswd
  input parameter passwd :: <c-string>; 
end easy-setopt sslcertpasswd;

define easy-setopt crlf
  input parameter crlf :: <c-int>; 
end easy-setopt crlf;

define easy-setopt quote
  input parameter commands :: <c-void*>; 
end easy-setopt quote;

define easy-setopt postquote
  input parameter commands :: <c-void*>; 
end easy-setopt postquote;

define easy-setopt writeheader
  input parameter pointer :: <c-void*>; 
end easy-setopt writeheader;

define easy-setopt headerfunction
  input parameter function :: <c-function-pointer>; 
end easy-setopt headerfunction;

define easy-setopt cookiefile
  input parameter filename :: <c-string>; 
end easy-setopt cookiefile;

define easy-setopt sslversion
  input parameter version :: <c-long>; 
end easy-setopt sslversion;

define easy-setopt timecondition
  input parameter version :: <c-long>; 
end easy-setopt timecondition;

define easy-setopt timevalue
  input parameter version :: <c-long>; 
end easy-setopt timevalue;

define easy-setopt customrequest
  input parameter request :: <c-string>; 
end easy-setopt customrequest;

define easy-setopt stderr
  input parameter file :: <c-void*>; 
end easy-setopt stderr;

define easy-setopt interface
  input parameter interface :: <c-string>; 
end easy-setopt interface;

define easy-setopt krb4level
  input parameter level :: <c-string>; 
end easy-setopt krb4level;

define easy-setopt progressfunction
  input parameter function :: <c-function-pointer>; 
end easy-setopt progressfunction;

define easy-setopt progressdata
  input parameter data :: <c-void*>; 
end easy-setopt progressdata;

define easy-setopt ssl-verifypeer
  input parameter value :: <c-long>; 
end easy-setopt ssl-verifypeer;
      
define easy-setopt cainfo
  input parameter info :: <c-string>; 
end easy-setopt cainfo;

define easy-setopt passwdfunction
  input parameter function :: <c-function-pointer>; 
end easy-setopt passwdfunction;

define easy-setopt passwddata
  input parameter data :: <c-void*>; 
end easy-setopt passwddata;

define easy-setopt filetime
  input parameter value :: <c-long>; 
end easy-setopt filetime;

define easy-setopt maxredirs
  input parameter value :: <c-long>; 
end easy-setopt maxredirs;

define easy-setopt maxconnects
  input parameter value :: <c-long>; 
end easy-setopt maxconnects;

define easy-setopt closepolicy
  input parameter value :: <c-long>; 
end easy-setopt closepolicy;

define easy-setopt fresh-connect
  input parameter value :: <c-long>; 
end easy-setopt fresh-connect;

define easy-setopt forbid-reuse
  input parameter value :: <c-long>; 
end easy-setopt forbid-reuse;

define easy-setopt random-file
  input parameter filename :: <c-string>; 
end easy-setopt random-file;

define easy-setopt egdsocket
  input parameter filename :: <c-string>; 
end easy-setopt egdsocket;

define easy-setopt connecttimeout
  input parameter value :: <c-long>; 
end easy-setopt connecttimeout;

define c-function curl-easy-perform
  input parameter handle :: <curl>;
  result value :: <curl-code>;
  c-name: "curl_easy_perform";
end c-function curl-easy-perform;

define c-subtype <form-pointer> (<c-void*>) end;
define c-pointer-type <form-pointer*> => <form-pointer>;

define c-function curl-formparse
  input parameter form :: <c-string>;
  input output parameter first-item :: <form-pointer*>;
  input output parameter last-item :: <form-pointer*>;
  result value :: <curl-code>;
  c-name: "curl_formparse";
end c-function curl-formparse;

define c-function curl-formfree
  input parameter form :: <c-void*>;
  c-name: "curl_formfree";
end c-function curl-formfree;

define thread variable *header-function-callback* = #f;
define thread variable *write-function-callback* = #f;

define method dylan-header-function-callback(ptr, size, nmemb, stream) 
  *header-function-callback*(ptr, size, nmemb, stream);
end method dylan-header-function-callback;

define method dylan-write-function-callback(ptr, size, nmemb, stream) 
  *write-function-callback*(ptr, size, nmemb, stream);
end method dylan-write-function-callback;

define c-callable-wrapper c-header-function-callback of dylan-header-function-callback
  input parameter ptr :: <c-void*>;
  input parameter size :: <c-int>;
  input parameter nmemb :: <c-int>;
  input parameter stream :: <c-void*>;
  result bytes-written :: <c-int>;
end c-callable-wrapper;

define c-callable-wrapper c-write-function-callback of dylan-write-function-callback
  input parameter ptr :: <c-void*>;
  input parameter size :: <c-int>;
  input parameter nmemb :: <c-int>;
  input parameter stream :: <c-void*>;
  result bytes-written :: <c-int>;
end c-callable-wrapper;

define c-function curl-global-init
  input parameter flags :: <c-long>;
  result value :: <curl-code>;
  c-name: "curl_global_init";
end c-function curl-global-init;

define c-function curl-global-cleanup
  c-name: "curl_global_cleanup";
end c-function curl-global-cleanup;

define constant $curl-global-ssl = 1;
define constant $curl-global-all = $curl-global-ssl;

define c-function curl-escape
  input parameter url :: <c-string>;
  input parameter length :: <c-int>;
  result value :: <c-string>;
  c-name: "curl_escape";
end c-function curl-escape;

define c-function curl-unescape
  input parameter url :: <c-string>;
  input parameter length :: <c-int>;
  result value :: <c-string>;
  c-name: "curl_unescape";
end c-function curl-unescape;

define variable *lib-curl-started?* :: <boolean> = #f;

define function start-lib-curl() => ()
  unless(*lib-curl-started?*)
    start-sockets();
    curl-global-init($curl-global-all);
    *lib-curl-started?* := #t;
  end;
end function start-lib-curl;

define function stop-lib-curl() => ()
  when(*lib-curl-started?*)
    curl-global-cleanup();
    *lib-curl-started?* := #f;
  end when;    
end function stop-lib-curl;

