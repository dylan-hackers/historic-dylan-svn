module: xmpp-test
synopsis: 
author: 
copyright: 

define function main(name :: <string>, #rest strings)

/*
  let jid = make(<jid>, domain: "foo");
  format-out("%=, %=, %=\n", jid.node, jid.domain, jid.resource);

  jid := make(<jid>, node: "a", domain: "b", resource: "c");
  format-out("%=, %=, %=\n", jid.node, jid.domain, jid.resource);

  jid := as(<jid>, "a@b/c");
  format-out("%=, %=, %=\n", jid.node, jid.domain, jid.resource);

  jid := as(<jid>, "a@b");
  format-out("%=, %=, %=\n", jid.node, jid.domain, jid.resource);
    
  jid := as(<jid>, "b");
  format-out("%=, %=, %=\n", jid.node, jid.domain, jid.resource);  
    
  format-out("%=\n", as(<string>, make(<jid>, domain: "b")));
  format-out("%=\n", as(<string>, make(<jid>, node: "a", domain: "b")));
  format-out("%=\n", as(<string>, make(<jid>, node: "a", domain: "b", resource: "c")));
  format-out("%=\n", as(<string>, make(<jid>, domain: "b", resource: "c")));
  
  format-out("%=\n", begin
                        as(<jid>, "b") = make(<jid>, domain: "b");
                      end);
  format-out("%=\n", begin
                        as(<jid>, "a@b") = make(<jid>, node: "a", domain: "b");
                      end);
  format-out("%=\n", begin
                        as(<jid>, "a@b/c") = make(<jid>, node: "a", domain: "b", resource: "c");
                      end);
  format-out("%=\n", begin
                        as(<jid>, "b/c") = make(<jid>, domain: "b", resource: "c");
                      end);

  let presence = make(<presence>);
  format-out("%=\n", presence);

  presence.type := #"foobar";
  format-out("%=\n", presence);
  
  presence.type := #"error";
  format-out("%=\n", presence);
  format-out("%=\n", presence.type);
  format-out("%=\n", presence.x);
  
  presence.show := "foobar";
  format-out("%=\n", presence);

  presence.show := "chat";
  format-out("%=\n", presence);

  presence.show := "dnd";
  format-out("%=\n", presence);
  format-out("%=\n", presence.show);

  
  format-out("%=\n", presence.status);
  presence.status := "Foo-Bar <3";
  format-out("%=\n", presence);
  format-out("%=\n", presence.status);

  presence.priority := 1;
  format-out("%=\n", presence); 
  format-out("%=\n", presence.priority);
  
  presence.to := make(<jid>, node: "alice", domain: "jabber.ccc.de", resource: "dylan");
  presence.from := "bob";  
  presence.id := "2342";

  let answer = answer(presence);
  format-out("%s %s %s\n", presence.to, presence.from, presence.id);
  format-out("%s %s %s\n", answer.to, answer.from, answer.id); 
  format-out("%=\n", presence);
  format-out("%=\n", answer);
    
  let foo = make(<element>, name: "foo1");
  let foo2 = make(<element>, name: "foo1");
  add-element(foo2, make(<element>, name: "foo2"));
  add-attribute(foo2, make(<attribute>, name: "a", value: "1"));
  add-attribute(foo2, make(<attribute>, name: "b", value: "2"));
  add-attribute(foo2, make(<attribute>, name: "c", value: "3"));
  add-attribute(foo, make(<attribute>, name: "d", value: "4"));
  add-attribute(foo, make(<attribute>, name: "a", value: "0"));
  format-out("%=\n", foo);
  format-out("%=\n", foo2);
  import-element(foo, foo2);
  format-out("%=\n", foo);

  let message = make(<message>, to: presence.to, body: "Hello, World"); 
  format-out("%=\n", message);

  let presence = make(<presence>);
  let presence-x = make(<x>);
  add-element(presence, presence-x);
  format-out("%=\n", presence.x);
  format-out("%=\n", object-class(presence.x));
  
  let presence = make(<presence>);
  let presence-x = make(<element>, name: "x");
  add-element(presence, presence-x);
  format-out("%=\n", presence.x);
  format-out("%=\n", object-class(presence.x));

  add-namespace(presence, "jabber:foo"); 
  format-out("%=\n", presence);

  let foobar = make(<element>, name: "foobar");
  add-attribute(foobar, make(<attribute>, name: "a", value: "1"));
  add-attribute(foobar, make(<attribute>, name: "a", value: "2"));
  format-out("%=\n", foobar);
  add-element(foobar, make(<element>, name: "foo"));
  add-element(foobar, make(<element>, name: "foo2"));
  add-element(foobar, make(<element>, name: "foo2"));
  add-element(foobar, make(<element>, name: "foo"));
  remove-element(foobar, "foo");
  format-out("%=\n", foobar);
  remove-element(foobar, "foo2", count: 1);
  format-out("%=\n", foobar);

  let iq = make(<iq>);
  format-out("%=\n", iq);
  iq.vcard := make(<element>, name: "vCard");
  format-out("%=\n", object-class(iq.vcard));
  iq.vcard := make(<vcard>);
  format-out("%=\n", object-class(iq.vcard));
  iq.query := make(<element>, name: "query");
  format-out("%=\n", object-class(iq.query));
  iq.query := make(<query>);
  format-out("%=\n", object-class(iq.query));
                    
  let iq2 = make(<iq>, vcard: make(<vcard>));
  format-out("%=\n", iq);

  let m = make(<message>);
  add-element(m, make(<element>, name: "foo1"));
  add-element(m, make(<element>, name: "foo2"));
  m.body := "Whooha!";
  m.subject := "Simple Test";
  format-out("%=\n", m);
  normalize(m);
  format-out("%=\n", m);

  let foo-stanza = make(<message>);
  let xxx = make(<element>, name: "x");
  add-element(foo-stanza, xxx);
  format-out("%=\n", object-class(first(foo-stanza.node-children)));
  format-out("%=\n", object-class(first(elements(foo-stanza, "x"))));

  let test-table = make(<table>);
  test-table[#"x"] := <x>;
  format-out("%=\n", test-table[#"x"]);
  let foox = #"x";
  format-out("%=\n", object-class(make(test-table[foox])));

  let superfluous-jid = make(<jid>, node: "turbo24prg", domain: "jabber.ccc.de", resource: "marvin");
  let cool-query = make-query(type: #"get", to: superfluous-jid);
  format-out("%=\n", cool-query);

  format-out("%=\n", make-authentication(make(<jid>, node: "turbo24prg", domain: "jabber.ccc.de"), "1337"));
  format-out("%=\n", make-authentication(superfluous-jid, "1337"));
  format-out("%=\n", make-registration(username: "turbo24prg", password: "foobar"));
  format-out("%=\n", make-registration(username: "turbo24prg"));
  format-out("%=\n", make-vcard(to: superfluous-jid));

  let ns-test = make(<element>, name: "foo:bar");
  add-attribute(ns-test, make(<attribute>, name: "xml:lang", value: "blah"));
  format-out("%=\n", ns-test);
 
  let stream = make(<xmpp-stream>);
  
  stream.version := "1.0";
  format-out("%=\n", stream);
  stream.version := "0.9";
  format-out("%=\n", stream);
  stream.version := make(<version>, major: 1, minor: 0);
  format-out("%=\n", stream);
  
  stream.version := "1.0";
  format-out("%= %= %=\n", stream.version, stream.version.major, stream.version.minor);
  stream.version := "0.9";
  format-out("%= %= %=\n", stream.version, stream.version.major, stream.version.minor);
  stream.version := "1";
  format-out("%= %= %=\n", stream.version, stream.version.major, stream.version.minor);

  let stream-error = make(<xmpp-stream-error>);
  stream-error.description := "Uff! Don't know?!";
  stream-error.condition := #"server-shutdown";
  format-out("%=\n", stream-error); 
  stream-error.condition := #"system-shutdown";
  format-out("%=\n", stream-error);
  stream-error.condition := #"policy-violation";
  stream-error.description := #f;
  format-out("%=\n", stream-error);

  let stanza-error = make(<xmpp-stanza-error>);
  stanza-error.description := "Boo!";
  stanza-error.type := #"continue"; 
  stanza-error.condition := #"unexpected-request";
  format-out("%=\n", stanza-error);

  let intl-message = make(<message>, body: "Dies ist ein Test.", to: superfluous-jid, language: "de");
  add-body(intl-message, "This is a test.", language: "en");
  add-body(intl-message, "C'est une test.", language: "fr");
  format-out("%=\n", intl-message);

  let test-document :: <document> = parse-document("<foo>Blub</foo>");
  format-out("%=\n", object-class(test-document.root));
  format-out("%=\n", test-document.node-children);

  let test-document :: <document> = parse-document("<foo>Blub</foo><foo2>Blob</foo2>");
  format-out("%=\n", object-class(test-document.root));
  format-out("%=\n", test-document.node-children);
  
*/
format-out("%=\n", parse-document("<stream:stream xmlns:stream='http://etherx.jabber.org/streams' id='4463ACD5' xmlns='jabber:client' from='192.168.0.1'></stream:stream>"));

//start-sockets();
  let client = make(<xmpp-client>, jid: make(<jid>, node: "foo", domain: "192.168.0.1"));
  let stream = make(<xmpp-stream>, to: client.jid.domain);
  
//  connect(client);
//  send(client, stream);
//  listen(client);
//  let response = read-line(client.socket, on-end-of-stream: #f);
//  if (response) write-line(*standard-output*, response); end if;
//  disconnect(client);
  
/*
//  format-out("%=\n", object-class(parse-document("<foo>Blub</foo>")));
  block ()
    format-out("%=\n", object-class(parse-document("<foo>Blub")));
//    format-out("%=\n", object-class(parse-document("<foo>Blub</fo")));
//    format-out("%=\n", object-class(parse-document("<foo t='>Blub</foo>")));
//    format-out("%=\n", object-class(parse-document("<foo>Blub</blub>")));
  exception (condition :: <condition>)
    format-out("Error: %=\n", condition);
    format-out("%=\n", object-class(condition));
  end block;
*/
  block()
    connect(client);
    format-out("Connected to xmpp server at %s port: %d\n", 
      client.socket.remote-host.host-name,
      client.socket.remote-port);
    send(client, stream);
    while (#t)
//foo
    end while;

//close(client-socket);
    disconnect(client);
    format-out("Connection closed.  Bye\n");
  exception (condition :: <condition>)
    disconnect(client);
    format-out("xmpp-test: Error: %=\n", condition);
  end block;

/*
  block () 
    format-out("%=\n", scan-xml-decl("<?xml version='1.0'?>"));
    format-out("%=\n", scan-xml-decl("<?xml version=''?>"));
//format-out("%=\n", scan-xml-decl("<?xml?>"));
    format-out("%=\n", scan-xml-decl("<?xml '"));
  exception (condition :: <condition>)
    format-out("Error: %=: %=\n", condition);
  end block;
*/
/*
  block ()
    let (index, element) = scan-maybe-element-data("<foo123'%>");
    format-out("%=\n", element);
    let (index, element) = scan-maybe-element-data("<foo123'%><foo");
    format-out("%=\n", element);
    let (index, element) = scan-maybe-element-data("<foo123'%><foo>");
    format-out("%=\n", element);
    let (index, element) = scan-maybe-element-data("a<foo123'%><foo>", start: 1);
    format-out("%=\n", element);
    let (index, positions) = scan-positions("1 2 3 4");
    format-out("%=\n", positions);
    
  exception (condition :: <condition>)
    format-out("Error: %=: %=\n", condition);
  end block;
*/ 
  exit-application(0);
end function main;
/*
define meta positions (space) => (space)
  "1", scan-s(space), "2", scan-s(space), "3", scan-s(space), "4"
end meta positions;
*/
// Invoke our main() function.
main(application-name(), application-arguments());
