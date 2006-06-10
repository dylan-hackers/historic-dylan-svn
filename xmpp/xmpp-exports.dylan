module: dylan-user

define library xmpp
  use common-dylan;
  use io;
  use network;
  use xml-parser;
  use meta;
  use priority-queue;
  
  export xmpp;
end library;

define module xmpp
  use common-dylan;
  use threads;
  use common-extensions;
  use format, exclude: { format-to-string };
  use sockets;
  use streams;
  use xml-parser;
  use xml-stream-parser;
  use simple-xml;
  use priority-queue;
  
  //XXX
  use standard-io;
  use format-out;
  use %productions;
  
  export <jid>,
    node, node-setter,
    domain, domain-setter,
    resource, resource-setter,
    strip, strip!;

  export <stanza>,
    answer;

  export <presence>,
    show, show-setter,
    status, status-setter, add-status,
    priority, priority-setter;
  
  export <message>,
    body, body-setter, add-body,
    subject, subject-setter, add-subject,
    thread, thread-setter;
    
  export <x>,
    x;

  export <iq>,
    query, query-setter,
    vcard, vcard-setter,
    make-query, make-vcard,
    make-authentication,
    make-registration;

  export <query>;

  export <vcard>;

  export <xmpp-stream>,
    language, language-setter,
    version, version-setter;

  export <version>,
    major, major-setter,
    minor, minor-setter;
 
  export <xmpp-stream-error>,
    <xmpp-stanza-error>,
    condition, condition-setter,
    description, description-setter; 

  export <xmpp-client>,
    jid, jid-setter,
    socket, socket-setter,
    state, state-setter,
    callbacks,
    callbacks-setter,
    listener,
    add-callback,
    connect, disconnect,
    send, authenticate,
    connected?, disconnected?;
   
  export <callback>;

  export normalize,
    id, id-setter,
    from, from-setter,
    to, to-setter,
    language, language-setter,
    type, type-setter,
    *default-language*,
    print-object;

  export xmpp-element-definer;
  export <foo-stanza>,
    foo-id, foo-id-setter,
    foo-from, foo-from-setter,
    foo-to, foo-to-setter,
    foo-type, foo-type-setter, 
    foo-language, foo-language-setter;
end module;
