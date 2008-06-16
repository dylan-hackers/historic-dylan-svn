Module:    dylan-user
Author:    Hannes Mehnert
Copyright: (C) 2006,  All rights reserved.

define module xmpp-bot
  use common-dylan,
    exclude: { format-out };
  use threads;
  use format;
  use format-out;
  use standard-io;
  use xmpp;
  use simple-xml;
  use xml-parser;

  // Add binding exports here.
  export <xmpp-bot>, broadcast-message, unicast-message, ping, online-users;
end module xmpp-bot;
