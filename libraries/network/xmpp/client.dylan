module: xmpp
synopsis: 
author: turbo24prg 
copyright: 

define class <xmpp-client> (<object>)
  slot jid :: <jid>,
    required-init-keyword: jid:;
  slot socket :: <tcp-socket>,
    init-keyword: socket:;
  slot state :: one-of(#"disconnected", #"connected") = #"disconnected";
  slot callbacks :: <table> = make(<table>);
  virtual slot password;
  slot lock :: <lock>;
  slot notification :: <notification>;
  slot available-stanza :: false-or(<element>) = #f;
  slot listener :: <thread>;
end class <xmpp-client>;

define method initialize (client :: <xmpp-client>, #rest rest, #key, #all-keys)
  next-method();
  client.lock := make(<lock>); 
  client.notification := make(<notification>, lock: client.lock);
end method initialize;

define method connect (client :: <xmpp-client>, #key port :: <integer> = 5222, host, stream)
 => (connected :: <boolean>);
  start-sockets();
  client.socket := make(<tcp-socket>, host: host | client.jid.domain, port: port);
  client.listener := make(<thread>, priority: $background-priority, function: curry(listen, client));
  if (~ stream)
    stream := make(<xmpp-stream>, to: client.jid.domain);
  end if;
  let answer = send(client, start-tag(stream), awaits-result?: #t);
  if (answer.name = #"stream:stream")
    client.state := #"connected";
    #t;
  else
    client.state := #"disconnected";
    #f;
  end if;
end method connect;

define method add-callback (client :: <xmpp-client>, class :: <class>, callback :: <callback>)
  unless (element(client.callbacks, class, default: #f))
    client.callbacks[class] := make(<priority-queue>, comparison-function: \>);
  end unless;
  client.callbacks[class] := add!(client.callbacks[class], callback);
end method add-callback;

define method listen (client :: <xmpp-client>)
  let current-element = #f;
  let stream-initiated? = #f;
  let parser = make(<xml-stream-parser>, stream: client.socket);

  monitor(parser, #"start-element", method (event-name, event-attributes)
    let element = make(<element>, name: event-name);
    for (attribute in event-attributes)
      add-attribute(element, attribute);
    end for;
    if (current-element)
      add-element(current-element, element);
    end if;
    current-element := element;

    if (current-element.name = #"stream:stream" & ~ stream-initiated?)
      stream-initiated? := #t;
      make(<thread>, function: curry(dispatch, client, current-element));
      current-element := #f;
    end if;
  end);

  monitor(parser, #"end-element", method (event-name)
    if (event-name = #"stream:stream" & ~ current-element)
      stream-initiated? := #f;
    else
      unless (current-element.element-parent)
        make(<thread>, function: curry(dispatch, client, current-element));
      end unless;
      current-element := current-element.element-parent;
    end if;
  end);

  monitor(parser, #"characters", method (chars)
    if (current-element & ~ every?(method(x) x = '\n' end, chars))
      current-element.node-children := concatenate(current-element.node-children, vector(make(<char-string>, text: chars)));
    end if;
  end);

  block()
    parse(parser);
  exception (e :: <condition>)
   //just catch all errors and ignore them
  end;
end method listen;

define method disconnect (client :: <xmpp-client>)
  close(client.socket);
  client.state := #"disconnected";
end method disconnect;

define method send (client :: <xmpp-client>, data :: type-union(<element>, <string>), #key awaits-result?)
  write-line(client.socket, as(<string>, data));
  force-output(client.socket);
  format-out("<<< %s\n", data);
  if (awaits-result?)
    let result = #f;
    with-lock (client.lock) 
      until (client.available-stanza) 
        wait-for(client.notification);
      end until; 
      result := client.available-stanza;
      client.available-stanza := #f;
    end with-lock;
    result;
  end if;
end method send;

define method send-with-id (client :: <xmpp-client>, data :: <element>, #key awaits-result?)
  if (~ data.id)
    data.id := "foo";
  end if;
  
  let result = send(client, data, awaits-result?: awaits-result?);
  if (awaits-result?)
    if (result.id ~= data.id)
      signal("id-missmatch");
    else
      result;
    end if;
  end if;
end method send-with-id;

define method password-setter (password, client :: <xmpp-client>)
 => (res);

  password;
end method password-setter;

define method dispatch (client :: <xmpp-client>, received-element :: <element>)
  format-out("!!! (X2) %=\n", received-element);
  let stanza = select (received-element.name)
    #"message" => as(<message>, received-element);
    #"presence" => as(<presence>, received-element);
    #"iq" => as(<iq>, received-element);
    otherwise => received-element;
  end select;
  with-lock (client.lock)
    if (~ client.available-stanza)
      release-all(client.notification);
    end if; 
    client.available-stanza := stanza;
  end with-lock;
  format-out("!!! (X2) %=\n", stanza);
  format-out("!!! (X2) %=\n", object-class(stanza));
  block (return)
    if (element(client.callbacks, object-class(stanza), default: #f))
      format-out("::: %=\n", client.callbacks[object-class(stanza)]);
      for (callback in client.callbacks[object-class(stanza)])
        if (callback.handler(client, stanza))
          return();
        end if;
      end for;
    end if;
  end block;
end method dispatch;

define generic authenticate (client :: <xmpp-client>, password, digest) => (authenticated? :: <boolean>);
define method authenticate (client :: <xmpp-client>, password, digest == #f)
 => (authenticated? :: <boolean>); 
  let possibilities = send-with-id(client, make-authentication-request(client.jid));    // , awaits-result?: #t);
/*  if (possibilities.type = #"result" &
      elements(possibilities.query, "username") &
      elements(possibilities.query, "resource") &
      elements(possibilities.query, "password"))

      ...
      
      possibilities.query.password!!!
*/ 
  let success = send-with-id(client, make-authentication(client.jid, password), awaits-result?: #t);
end method authenticate;

define method connected? (client :: <xmpp-client>)
 => (res :: <boolean>)
  client.state == #"connected"
end method connected?;

define method disconnected? (client :: <xmpp-client>)
 => (res :: <boolean>)
  client.state == #"disconnected"
end method disconnected?;
