Module:    dylan-user
Author:    Hannes Mehnert
Copyright: (C) 2006,  All rights reserved.

define library xmpp-bot
  use common-dylan;
  use io;
  use system;
  use xmpp;
  use xml-parser, import: { simple-xml, xml-parser };

  // Add any more module exports here.
  export xmpp-bot;
end library xmpp-bot;
