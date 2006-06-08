Module:    dylan-user
Author:    Hannes Mehnert
Copyright: (C) 2006,  All rights reserved.

define module xmpp-bot
  use common-dylan;
  use threads;
  use format;
  use format-out;
  use standard-io;
  use xmpp;
  use simple-xml;

  // Add binding exports here.
  export <xmpp-bot>, broadcast-message;
end module xmpp-bot;
