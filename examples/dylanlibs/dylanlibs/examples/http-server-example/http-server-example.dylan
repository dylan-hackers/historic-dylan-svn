Module:    http-server-example
Synopsis:  Testing the HTTP server.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define variable *counter* :: <integer> = 0;

define method index(server :: <simple-http-server>,
                    request :: <request>,
                    stream :: <stream>)
 => ()
  with-standard-http-result(stream)
    let dom = 
      with-dom-builder()
        (html
          (head
            (title ["Test HTTP Server"])),
          (body
            (p ["Testing the Dylan HTTP Server."]),
            (p ((a href: "/formtest1") ["Form test 1"])),
            (p ((a href: "/formtest2") ["Form test 2"])),
            (p ["Counter: "], [*counter* := *counter* + 1])))
      end;
    print-html(dom, stream);
  end;
end method index;

define method formtest1(server :: <simple-http-server>,
                        request :: <request>,
                        stream :: <stream>)
 => ()
  with-standard-http-result(stream)
    if(request.request-query)
      let name = request.request-query["name"];
      print-html(
        with-dom-builder()
          (html
            (head (title ["Hi to"], [name])),
            (body
              (p ["Your name is "], (b [name]))))
        end with-dom-builder, stream);
    else
      print-html(
        with-dom-builder()
          (html
            (head
              (title ["Form Test 1"])),
            (body
              ((form action: "/formtest1")
                 ["Your name is "],
                 ((input type: "text",
                         name: "name",
                         maxlength: "20")))))
        end with-dom-builder, stream);
    end if;
  end; 
end method formtest1;

define method formtest2(server :: <simple-http-server>,
                        request :: <request>,
                        stream :: <stream>)
 => ()
  with-standard-http-result(stream)
    let body = request.request-body;
    if(body)
      let query = form-query-decode(body);
      let quotation = query["quotation"];
      print-html(
        with-dom-builder()
          (html
            (head (title ["Quotation"])),
            (body
              (p ["The quotation was "], (i [quotation]))))
        end with-dom-builder, stream);
    else
      print-html(
        with-dom-builder()
          (html
            (head
              (title ["Form Test 2"])),
            (body
              ((form action: "/formtest2",
                     method: "POST")
                 ["Enter a quote "],
                 (p
                   ((textarea 
                   name: "quotation",
                   rows: 30,
                   columns: 50)),
                  ((input type: "submit",
                          name: "submit",
                          value: "count it"))))))
        end with-dom-builder, stream);
    end if;
  end; 
end method formtest2;

define method main () => ()
  format-out("Starting...\n");
  block()
    initialize-http-server();
    let server = make(<threaded-http-server>);
    publish-dynamic-handler(server, "/quit", quit-handler);
    publish-dynamic-handler(server, "/headers", display-header-handler);
    publish-dynamic-handler(server, "/", index);
    publish-dynamic-handler(server, "/formtest1", formtest1);
    publish-dynamic-handler(server, "/formtest2", formtest2);
    start-http-server(server, port: 8000);
  cleanup
    format-out("Stopping...\n");
  end;
end method main;

begin
  main();
end;
