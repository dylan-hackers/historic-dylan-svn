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
  format-out("TEST: %s\n", real-name("foo:bar"));
  make(<thread>, priority: $background-priority, function: curry(listen, client));
end method connect;

define method listen (client :: <xmpp-client>)

block ()
//  let parser-depth = 0;
//  let stream-initiated? = #f;
  let parsing-tag? = #f;
  let parser-buffer = "";
  
  // keep watching that start tags match end tags
  let tag-queue = make(<deque>);
  
  while (~ stream-at-end?(client.socket))
    let received = read-element(client.socket);

    block(read-next)
        if (parsing-tag? = #f)
          if (received = '<')
            parsing-tag? := #t;
            parser-buffer := add!(parser-buffer, received);
            read-next();
          elseif (size(tag-queue) = 0 & received ~= '\n')
            //!!! error: not well-formed xml: chars not contained in root element
            format-out("!!! error: not well-formed xml: chars not contained in root element\n");
          end if;
        else
          if (received = '>')
            // seems as we got an element
            parser-buffer := add!(parser-buffer, received);
            format-out(">>> %s\n", parser-buffer);

            // could be the start tag of an element
            let (index, start-tag, opened-element?) = scan-start-tag(parser-buffer);
            if (start-tag & opened-element?)
              format-out("!!! (start)  %s (%s)\n", start-tag, real-name(start-tag));
              // should be closed later
              push-last(tag-queue, start-tag);
              format-out("!!! now at depth: %d\n", size(tag-queue));
              // dispatch();
              parser-buffer := "";
              parsing-tag? := #f;
              read-next();
            elseif (start-tag & ~ opened-element?)
              format-out("!!! (empty)  %s (%s)\n", start-tag, real-name(start-tag));
              // dispatch();
              parser-buffer := "";
              parsing-tag? := #f;
              read-next();
            end if;
            
            // could be the end tag of an element
            let (index, end-tag, opened-element?) = scan-end-tag(parser-buffer);
            if (end-tag)
              format-out("!!! (end)  %s (%s)\n", end-tag, real-name(end-tag));
              // should close the last started tag
              if (as(<symbol>, end-tag) = last(tag-queue))
                format-out("!!! (successful end)  %s (%s)\n", end-tag, real-name(end-tag));
                pop-last(tag-queue);  
                format-out("!!! now at depth: %d\n", size(tag-queue));
                // dispatch();
                parser-buffer := "";
                parsing-tag? := #f;
                read-next();
              else
                //!!! error: not-well formed xml: start/end tag mismatch
                format-out("!!! (WANTED end)  %s (%s)\n", last(tag-queue), real-name(last(tag-queue)));
              end if;
            end if;
            
            // could be a xml declaration
            let (index, processing-instruction) = scan-xml-decl(parser-buffer);
            if (processing-instruction)
              format-out("!!! %=: %s\n", object-class(processing-instruction), processing-instruction.name);
              parser-buffer := "";
              parsing-tag? := #f;
              read-next();
            end if;
    
          else
            //XXX we allow everything in a tag
            if (received ~= '\n')
              parser-buffer := add!(parser-buffer, received);
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

/*
define collector maybe-elements (c) => (c) 
  loop([scan-maybe-element(c), do(collect(c))])
end collector maybe-element;

define collector maybe-element (c) => (c)
 "<", loop({[">", do(collect('>')), finish()], [accept(c), do(collect(c))]})
end collector elements;
*/
