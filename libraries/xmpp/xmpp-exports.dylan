module: dylan-user

define library xmpp
  use common-dylan;
  use io;
  use network;
  use xml-parser;
  use meta;
  
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
  use simple-xml;
 
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
    jid, socket, state,
    connect, disconnect,
    send, authenticate;
    
  export normalize,
    id, id-setter,
    from, from-setter,
    to, to-setter,
    language, language-setter,
    type, type-setter,
    *default-language*,
    print-object;
    
end module;
