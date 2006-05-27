module: xmpp
synopsis: 
author: 
copyright:

define constant *stanza-lock* = make(<lock>);
define constant *parsed-stanza* = make(<notification>, lock: *stanza-lock*); 
define variable *available-stanza* :: false-or(<element>) = #f;
  
define class <xmpp-client> (<object>)
  slot jid :: <jid>,
    required-init-keyword: jid:;
  slot socket :: <tcp-socket>,
    init-keyword: socket:;
  slot state :: one-of(#"disconnected", #"connected") = #"disconnected";
  slot message-callbacks :: <priority-queue> = 
    make(<priority-queue>, comparison-function: \>);
  slot presence-callbacks :: <priority-queue> =
    make(<priority-queue>, comparison-function: \>);
  slot iq-callbacks :: <priority-queue> =
    make(<priority-queue>, comparison-function: \>);
  slot xml-callbacks :: <priority-queue> =
    make(<priority-queue>, comparison-function: \>);
  virtual slot password;
end class <xmpp-client>;

define method connect (client :: <xmpp-client>, #key port :: <integer> = 5222, host, stream)
 => (connected :: <boolean>);
  start-sockets();
  client.socket := make(<tcp-socket>, host: host | client.jid.domain, port: port);
  make(<thread>, priority: $background-priority, function: curry(listen, client));
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

define method listen (client :: <xmpp-client>)

block ()
  let stream-running? = #f;
  let parsing-tag? = #f;
  let tag = "";
  let buffer = "";
  let current-element = #f;
  let tag-queue = make(<deque>);

  while (~ stream-at-end?(client.socket))
    let received = read-element(client.socket);
  
    block(read-next)
        if (parsing-tag? = #f)
          if (received = '<')
            parsing-tag? := #t;
            if (size(buffer) > 0 & ~ every?(method(x) x = '\n' end, buffer) & current-element)
              //let xml-text = make(<char-string>, text: buffer);
              format-out("||| %=          %=\n", current-element, buffer);
              format-out("||| %=\n", current-element.node-children);         
              current-element.node-children := concatenate(current-element.node-children, vector(make(<char-string>, text: buffer)));
              format-out("||| %=          %=\n", current-element, buffer);
              buffer := "";
            end if;
            tag := add!(tag, received);
            read-next();
          elseif (~ stream-running? & received ~= '\n')
            //!!! error: not well-formed xml: chars not contained in root element
            format-out("!!! error: not well-formed xml: chars not contained in root element\n");
          elseif (stream-running? & current-element)
            //!!! collect chars into text of current-element!!!
            buffer := add(buffer, received);
            read-next();
          end if;
        else
          if (received = '>')
            // seems as we got an element
            tag := add!(tag, received);
            format-out(">>> %s\n", tag);

            // could be the start tag of an element
            let (index, start-tag, attributes, opened-element?) = scan-start-tag(tag);
            if (start-tag & opened-element?)
              format-out("!!! (start)  %s\n", start-tag);
              // should be closed later
              push-last(tag-queue, start-tag);
              format-out("!!! now at depth: %d\n", size(tag-queue));
              // dispatch  
              let element = make(<element>, name: as(<string>, start-tag));
              for (attribute in attributes)
                add-attribute(element, attribute);
              end for;
              if (current-element)
                add-element(current-element, element);
              end if;
              current-element := element;
              format-out("!!! (current element) %=\n", current-element);
              if (current-element.name = #"stream:stream" & ~ stream-running?)
                stream-running? := #t;
                //!!! do something
                format-out("!!! (X) %=\n", current-element);
                make(<thread>, function: curry(dispatch, client, current-element));
                current-element := #f;
              end if;
              // cleanup
              tag := "";
              parsing-tag? := #f;
              read-next();
            elseif (start-tag & ~ opened-element?)
              format-out("!!! (empty)  %s\n", start-tag);
              // dispatch
              let element = make(<element>, name: as(<string>, start-tag));
              for (attribute in attributes)
                add-attribute(element, attribute);
              end for;
              // empty stanza
              if (size(tag-queue) < 2)
                format-out("!!! (X) %=\n", element);
                make(<thread>, function: curry(dispatch, client, element));
              else
                add-element(current-element, element);
              end if;
              // cleanup
              tag := "";
              parsing-tag? := #f;
              read-next();
            end if;
            
            // could be the end tag of an element
            let (index, end-tag, opened-element?) = scan-end-tag(tag);
            if (end-tag)
              format-out("!!! (end)  %s\n", end-tag);
              // should close the last started tag
              if (as(<symbol>, end-tag) = last(tag-queue))
                format-out("!!! (successful end)  %s\n", end-tag);
                pop-last(tag-queue);  
                format-out("!!! now at depth: %d\n", size(tag-queue));
                // dispatch
                format-out("!!! (-) %=\n", current-element);
                format-out("!!! (+) %=\n", current-element.element-parent);
                if (size(tag-queue) < 2)
                  format-out("!!! (X) %=\n", current-element);
                  if (end-tag = "stream:stream" & ~ current-element)
                    stream-running? := #f;
                    //!!! what do do here? thread?!
                  else
                    make(<thread>, function: curry(dispatch, client, current-element));
                  end if;
                end if;
                current-element := current-element.element-parent;
                // cleanup
                tag := "";
                parsing-tag? := #f;
                read-next();
              else
                //!!! error: not-well formed xml: start/end tag mismatch
                format-out("!!! (WANTED end)  %s (%s)\n", last(tag-queue), real-name(last(tag-queue)));
              end if;
            end if;
            
            // could be a xml declaration
            let (index, processing-instruction) = scan-xml-decl(tag);
            if (processing-instruction)
              format-out("!!! %=: %s\n", object-class(processing-instruction), processing-instruction.name);
              tag := "";
              parsing-tag? := #f;
              read-next();
            end if;
    
          else
            //XXX we allow everything in a tag
            if (received ~= '\n')
              tag := add!(tag, received);
            end if;
            read-next();
          end if;
        end if;
    end block;
   
  end while;
  format-out("!!! OOOOHHHH! NOOOOO!");
exception (condition :: <condition>)
  disconnect(client);
  format-out("client: listen: Error: %=", condition);
end block;

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
    with-lock (*stanza-lock*) 
      until (*available-stanza*) 
        wait-for(*parsed-stanza*);
      end until; 
      result := *available-stanza*;
      *available-stanza* := #f;
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

define method dispatch (client :: <xmpp-client>, element :: <element>)
//  let stanza = element;
  format-out("!!! (X2) %=\n", element);
  let stanza = select (element.name)
    #"message" => as(<message>, element);
    #"presence" => as(<presence>, element);
    #"iq" => as(<iq>, element);
    otherwise => element;
  end select;
  with-lock (*stanza-lock*)
    if (~ *available-stanza*)
      release-all(*parsed-stanza*);
    end if; 
    *available-stanza* := stanza;
  end with-lock;
  format-out("!!! (X2) %=\n", stanza);
  format-out("!!! (X2) %=\n", object-class(stanza));
  let callbacks = select (stanza by instance?)
    <message> => client.message-callbacks;
    <presence> => client.presence-callbacks;
    <iq> => client.iq-callbacks;
    otherwise => client.xml-callbacks;
  end select;
  block (return)
    for (callback in callbacks)
      if (callback.handler(client, stanza))
        return();
      end if;
    end for;
  end block;
end method dispatch;

define method authenticate (client :: <xmpp-client>, password, #key digest = #t)
  let authentication = #f;
  let authentication-request = #f;
  if (digest)
    //!!!
  else
    authentication-request := make-authentication-request(client.jid);
    authentication := make-authentication(client.jid, password);
  end if;
  //!!!
  send-with-id(client, authentication-request, awaits-result?: #t);
  ///!!! verify!!!
  send-with-id(client, authentication, awaits-result?: #t);
end method authenticate;

define method connected? (client :: <xmpp-client>)
 => (res :: <boolean>)
  client.state = #"connected"
end method connected?;

define method disconnected? (client :: <xmpp-client>)
 => (res :: <boolean>)
  client.state = #"disconnected"
end method disconnected?;
