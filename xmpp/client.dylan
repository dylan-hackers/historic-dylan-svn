module: xmpp
synopsis: 
author: 
copyright:

define variable *parser-depth* = 0;

define class <xmpp-client> (<object>)
  slot jid :: <jid>,
    required-init-keyword: jid:;
  slot socket :: <tcp-socket>,
    init-keyword: socket:;
  slot state :: one-of(#"disconnected", #"authenticating", #"connected");
  virtual slot password;
end class <xmpp-client>;

define method connect (client :: <xmpp-client>, #key port :: <integer> = 5222)
  start-sockets();
  client.socket := make(<tcp-socket>, host: client.jid.domain, port: port);
  client.state := #"connected";
  make(<thread>, function: curry(listen, client));
end method connect;

define method listen (client :: <xmpp-client>)
block ()
  let stanza-complete? = #f;
  while (#t)
    let (received, found?) = read-to(client.socket, '>');
    received := concatenate(received, ">");
    format-out(">>> %=\n", received);
//XXX should strip spaces before first element!
    if (found? & (received[0] = '<'))
//check xml-decl
      let (index, processing-instruction) = scan-xml-decl(received);
      if (processing-instruction)
        format-out("!!! %=: %=\n", object-class(processing-instruction), processing-instruction.name);
      end if;  
// check if start
      format-out("!!! %=\n", "Check for element start");

      let (index, name) = scan-start-of-tag("<foo"); // scan-start-tag(received);
      if (name)
        format-out("!!! (start) %=\n", name);
        *parser-depth* := *parser-depth* + 1;
      else
        format-out("!!! no start!\n");
      end if;
      format-out("%=\n", received);
    end if;
//dispatch(received);
  end while;
exception (condition :: <condition>)
  disconnect(client);
  format-out("client: listen: Error: %=", condition);
end block;
end method listen;

define method disconnect (client :: <xmpp-client>)
  close(client.socket);
  client.state := #"disconnected";
end method disconnect;

define method send (client :: <xmpp-client>, data)
  write-line(client.socket, as(<string>, data));
  force-output(client.socket);
  format-out("<<< %=\n", data);
end method send;

define method password-setter (password, client :: <xmpp-client>)
 => (res);

  password;
end method password-setter;

define meta start-of-tag(elt-name, sym-name, attribs, s)
  => (sym-name, attribs)
  "<", scan-name(elt-name), scan-s?(s), scan-xml-attributes(attribs),
//  (push(*tag-name-with-proper-capitalization*, elt-name)),
  set!(sym-name, as(<symbol>, elt-name))
end meta start-of-tag;

define meta start-tag
//(name, s, attribs) => (name)
  "<", ">"
//, scan-name(name), scan-s?(s), scan-xml-attributes(attribs), ">"
end meta start-tag;

define meta end-tag (name, s) => (name)
  "</", scan-name(name), scan-s?(s), ">"
end meta end-tag;
                              
/*
define method valid-xmpp-data? (data :: <string>)
 => (res :: <boolean>);
  if (parse-document(data))
    #t;
  else
    #f;
  end if;
end method valid-xmpp-data?;
*/
//define method dispatch (
