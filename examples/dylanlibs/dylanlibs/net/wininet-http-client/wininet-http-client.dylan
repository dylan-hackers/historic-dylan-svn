Module:    wininet-http-client
Synopsis:  HTTP client library that uses the WinInet as a back end.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define function start-http-client() => ()
  // Do nothing
end function start-http-client;
    
define function stop-http-client() => ()
  // Do nothing
end function stop-http-client;

define class <http-session> (<object>)
  // Lock for managing access to the session
  slot http-session-lock :: <lock> = make(<lock>);

  // The curl handle for this session.
  slot http-open-handle :: false-or(<hinternet>);

  // Table mapping server/port to an InternetConnect
  slot http-connect-table :: <string-table> = make(<string-table>);
end class <http-session>;

define method make-http-session(#key cookies? = #t, user-agent) 
 => (r :: <http-session>)
  let session = make(<http-session>);
  session.http-open-handle :=
    InternetOpen(user-agent | "Internet Explorer",
                 $internet-open-type-direct,
                 null-pointer(<lpcstr>),
                 null-pointer(<lpcstr>),
                 0);
  when(null-pointer?(session.http-open-handle))
    error("InternetOpen failed");
  end when;    
  session;
end method make-http-session;

define method cleanup-http-session(session :: <http-session>) => ()
  with-lock(session.http-session-lock)
    for(connect-handle in session.http-connect-table)
      unless(InternetCloseHandle(connect-handle))
        error("InternetCloseHandle failed");
      end unless;
    end for;

    unless(InternetCloseHandle(session.http-open-handle))
      error("InternetCloseHandle failed");
    end unless;

    session.http-open-handle := #f;
    session.http-connect-table := make(<string-table>);
  end with-lock;
end method cleanup-http-session;

define macro with-http-session                              
{ with-http-session(?:name) ?:body end }
=> { with-http-session(?name = #f)
       ?body
     end  }
{ with-http-session(?:name = ?:expression) ?:body end }
=> { begin
       let original-session = ?expression;
       let ?name = original-session | make-http-session();
       block()
         ?body
       cleanup
         unless(original-session)
           cleanup-http-session(?name);
         end unless;
       end block;
     end begin }
end macro with-http-session;

define method copy-as-char*(string :: <byte-string>)
 => (r :: <c-string>)
  let char* = make(<c-string>, element-count: string.size + 1);
  for(ch keyed-by index in string)
    char*[index] := ch;
  end for;
  char*[string.size] := as(<character>, 0);
  char*;
end method copy-as-char*;

// Retrieve the data from the given url.
define method do-http-request*(session :: <http-session>,
                               scheme :: one-of(#"https", #"http"),
                               server :: <string>,
                               port :: <integer>,
                               path :: <string>,
                               #key
                               method: http-method = #"get",
                               content,
                               query)
 => (r :: <string>) 
  with-lock(session.http-session-lock)
    let connect-key = format-to-string("%s:%s", server, port);
    let connect-handle = element(session.http-connect-table, connect-key, default: #f);
    unless(connect-handle)
      connect-handle := InternetConnect(session.http-open-handle,
                                        server,
                                        port,
                                        "",
                                        null-pointer(<lpcstr>),
                                        $internet-service-http,
                                        0,
                                        null-pointer(<LPDWORD>));
      when(null-pointer?(connect-handle))
        error("InternetConnect failed.");
      end when;

      session.http-connect-table[connect-key] := connect-handle;
    end unless;

    let http-handle = HttpOpenRequest(connect-handle,
                                      select(http-method)
                                        #"get" => "GET";
                                        #"post" => "POST";
                                        otherwise => error("Unsupported http method %s", http-method);
                                      end select,
                                      path,
                                      "HTTP/1.1",
                                      null-pointer(<lpcstr>),
                                      null-pointer(<lpcstr*>),
                                      if(scheme == #"http")
                                        logior($internet-flag-keep-connection, $internet-flag-pragma-nocache) 
                                      else
                                        logior($internet-flag-keep-connection, $internet-flag-pragma-nocache, $internet-flag-secure) 
                                      end if,
                                      null-pointer(<LPDWORD>));
    
                                        
    when(null-pointer?(http-handle))
      error("HttpOpenRequest failed.");
    end when;

    block()
      let headers = null-pointer(<lpcstr>);
      let headers-size = 0;
      let optional = null-pointer(<lpcstr>);
      let optional-size = 0;

      block()
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

          unless(empty?(content))
            optional := copy-as-char*(content);
            optional-size := content.size;
            headers := copy-as-char*("Content-Type: application/x-www-form-urlencoded");
            headers-size := headers.size;
          end unless;
        end when;
        

        let request-result = HttpSendRequest(http-handle,
                                             headers,
                                             headers.size,
                                             optional,
                                             optional.size);
        unless(request-result)
          error("HttpSendRequest failed");
        end unless;

        let string-result = "";
        block(return)
          while(#t)         
            let (query-result, bytes) = InternetQueryDataAvailable(http-handle,
                                                                   0,
                                                                   0);
            unless(query-result)
              error("InternetQueryDataAvailable failed.");
            end unless;
 
            with-stack-structure(buffer :: <c-string>, size: bytes + 1)
              let (read-result, bytes-read) = InternetReadFile(http-handle,
                                                               buffer,
                                                               buffer.size);
              unless(read-result)
                error("InternetReadFile failed");
              end unless;

              when(zero?(bytes-read))
                  return();
              end when;

              buffer[bytes-read] := as(<character>, 0);
              string-result := concatenate(string-result, buffer);
            end with-stack-structure;
          end while;
        end block;

        string-result;        
      cleanup
        unless(null-pointer?(headers))
          destroy(headers)
        end unless;
        
        unless(null-pointer?(optional))
          destroy(optional)
        end unless;
      end block;
                                          
    cleanup
      unless(InternetCloseHandle(http-handle))
        error("InternetCloseHandle failed");
      end unless;
    end block;
  end with-lock;
end method do-http-request*;


