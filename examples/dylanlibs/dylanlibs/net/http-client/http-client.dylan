Module:    http-client
Synopsis:  HTTP client routines - for reading web pages from HTTP servers.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <http-session> (<object>)
  // The curl handle for this session.
  slot http-session-curl :: false-or(<curl>), required-init-keyword: curl:;

  // A sequence of resources that need to be held onto until
  // the session is cleaned up.
  slot http-resources :: false-or(<sequence>) = make(<stretchy-vector>);
end class <http-session>;

define method make-http-session(#key cookies? = #t, user-agent) => (r :: <http-session>)
  let session = make(<http-session>, 
                     curl: curl-easy-init());
  when(cookies?)
    curl-easy-setopt(session.http-session-curl, $curlopt-cookiefile, as(<c-string>, "nosuchfileshouldexist"));
  end when;

  when(user-agent)
    curl-easy-setopt(session.http-session-curl, $curlopt-useragent, as(<c-string>, user-agent));
  end when;

  session;
end method make-http-session;

define method cleanup-http-session(session :: <http-session>) => ()
  curl-easy-cleanup(session.http-session-curl);
  session.http-resources := #f;
  session.http-session-curl := #f;
end method cleanup-http-session;

define method add-resource(session :: <http-session>, r :: <object>) => (r :: <object>)
  session.http-resources := add!(session.http-resources, r);
  r
end method add-resource;

define macro with-http-session                              
{ with-http-session(?:name = ?:expression) ?:body end }
=> { begin
       let ?name = ?expression;
       block()
         ?body
       cleanup
         cleanup-http-session(?name);
       end block;
     end begin }
end macro with-http-session;
       

// Retrieve the data from the given url.
define method do-http-request(session :: <http-session>,
                              url :: <string>,
                              #key
                              follow-location? = #t,
                              method: http-method = #"get",
                              content,
                              query)
 => (r :: <string>)
  let curl = session.http-session-curl;
  let result = "";

  local method handle-request(ptr, size, nmemb, stream)
    ignore(stream);
    result := concatenate(result, as(<byte-string>, c-type-cast(<c-string>, ptr)));
    size * nmemb;
  end method;

  dynamic-bind(*write-function-callback* = handle-request)
    curl-easy-setopt(curl, $curlopt-url, add-resource(session, as(<c-string>, url)));
    curl-easy-setopt(curl, $curlopt-writefunction, c-write-function-callback);
    curl-easy-setopt(session.http-session-curl, $curlopt-autoreferer, 1);

    when(http-method == #"post")
      if(query)        
        content := "";
        for(pairs in query)
          unless(empty?(content))
            content := concatenate(content, "&");
          end unless;

          content := concatenate(content, pairs.head, "=", pairs.tail)
        end for;
      end if;
        
      let c-content = add-resource(session, as(<c-string>, content));
      curl-easy-setopt(curl, $curlopt-post, 1);
      curl-easy-setopt(curl, $curlopt-postfields, c-content);
      curl-easy-setopt(curl, $curlopt-postfieldsize, c-content.size);        
    end when;

    when(follow-location?)
      curl-easy-setopt(curl, $curlopt-followlocation, 1);
    end when;

    curl-easy-perform(curl);
    result;
  end dynamic-bind;    
end method do-http-request;

define function start-http-client() => ()
  start-lib-curl();
end function start-http-client;
    
define function stop-http-client() => ()
  stop-lib-curl();
end function stop-http-client;
    

