module: xmpp
synopsis: 
author: 
copyright:

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
  make(<thread>, priority: $background-priority, function: curry(listen, client));
end method connect;

define method listen (client :: <xmpp-client>)
block ()
  let stanza-complete? = #f;
  let parser-depth = 0;
  while (#t)
    let (received, found?) = read-through(client.socket, '>');
    format-out(">>> %s\n", received);
//XXX should strip spaces before first element!
    if (found? & (received[0] = '<'))
//check xml-decl
      let (index, processing-instruction) = scan-xml-decl(received);
      if (processing-instruction)
        format-out("!!! %=: %s\n", object-class(processing-instruction), processing-instruction.name);
      end if;
// check if start
      let (index, name, opened-element?) = scan-start-tag(received);
      format-out("!!! %= %= %=\n", index, name, opened-element?);
/*      if (name)
//format-out("!!! (start) %s - (current depth) %d\n", real-name(name), *parser-depth* + 1);
//        *parser-depth* := *parser-depth* + 1;
        format-out("!!! started element: %=\n", name);
        parser-depth := parser-depth + 1;
      else
        format-out("!!! no start element started\n");
      end if;
*/
      format-out("!!! depth: %=\n", parser-depth);
    else
      format-out("!!! not found!");
    end if;
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
  format-out("<<< %s\n", data);
end method send;

define method password-setter (password, client :: <xmpp-client>)
 => (res);

  password;
end method password-setter;

/*
define meta start-of-tag(elt-name, sym-name, attribs, s) => (elt-name, attribs)
  "<", scan-name(elt-name), scan-s?(s), scan-xml-attributes(attribs), ">"
//  (push(*tag-name-with-proper-capitalization*, elt-name)),
//set!(sym-name, as(<symbol>, elt-name))
end meta start-of-tag;

define meta start-tag
//(name, s, attribs) => (name)
  "<", ">"
//, scan-name(name), scan-s?(s), scan-xml-attributes(attribs), ">"
end meta start-tag;

define meta end-tag (name, s) => (name)
  "</", scan-name(name), scan-s?(s), ">"
end meta end-tag;
*/             
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

/*
define meta start-tag (elt-name, sym-name, attribs, s) => (elt-name, atts)
  "<", scan-name(elt-name), scan-s?(s), scan-xml-attributes(attribs), ">"
end meta start-tag;
*/


define collector maybe-elements (c) => (c) 
  loop([do(collect()])
end collector maybe-element;

define collector maybe-element (c) => (c)
 "<", loop({[">", do(collect('>')), finish()], [accept(c), do(collect(c))]})
end collector elements;
