Module:    nntp-get-message
Synopsis:  Example of using the NNTP library
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define frame <nntp-get-message-frame> (<simple-frame>)
  pane host-pane (frame)
    make(<text-field>, value: $default-nntp-host);

  pane port-pane (frame)
    make(<text-field>, value-type: <integer>, value: $default-nntp-port);

  pane message-id-pane (frame)
    make(<text-field>);

  pane get-message-button (frame)
    make(<push-button>, label: "Get Message", activate-callback: on-get-message);

  pane message-pane (frame)
    make(<text-editor>, read-only?: #t);

  layout (frame)
    vertically(spacing: 4)
      tabling(columns: 2)
        make(<label>, label: "Host:");
        frame.host-pane;
        make(<label>, label: "Port:");
        frame.port-pane;
      end tabling;

      horizontally(spacing: 2)
        frame.message-id-pane;
        frame.get-message-button;
      end horizontally;

      frame.message-pane;
    end vertically;      

  keyword title: = "Get NNTP Message";
  keyword width: = 300;
  keyword height: = 300;
end frame <nntp-get-message-frame>;

define method on-get-message(gadget)
  let frame = gadget.sheet-frame;
  let host = frame.host-pane.gadget-value;
  let port = frame.port-pane.gadget-value;

  with-nntp-client(client = (host, port))
    let id = frame.message-id-pane.gadget-value;
    if(nntp-stat(client, id).data)
      nntp-mode(client, "reader");
      let result = "";
      for(line in nntp-article(client, id))
        result := concatenate(result, line, "\n");
      end for;
      frame.message-pane.gadget-value := result;      
    else
      notify-user(format-to-string("%s does not have %s", host, id));
    end if;
  end with-nntp-client;
end method on-get-message;

define method main () => ()
  start-frame(make(<nntp-get-message-frame>));
end method main;

begin
  start-sockets();
  main();
end;
