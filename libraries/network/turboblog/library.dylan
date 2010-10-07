module: dylan-user
author: turbo24prg

define library turboblog
  use dylan;
  use common-dylan,
    import: { common-extensions };
  use io;
  use system,
    import: { locators, threads, date, file-system };
  use network;
  use string-extensions;
  use regular-expressions;
  use dsp;
  use koala;
  use web-framework;
  use xml-parser;
  use xml-rpc-client;
  use uri;
end;


define module turboblog
  use dylan;
  use threads;
  use common-extensions,
    exclude: { split, format-to-string };
  use locators;
  use file-system;
  use date;
  use format;
  use format-out;
  use sockets;
  use streams;
  use character-type;
  use substring-search;
  use regular-expressions;
  use dsp, exclude: { join };
  use koala;
  use web-framework, exclude: { slot-type };
  use users;
  use permission;
  use storage;
  use changes;
  use xml-parser, 
    rename: { <comment> => <xml-comment>, 
              name => xml-name, 
              name-setter => xml-name-setter };
  use simple-xml;
  use xml-rpc-client;
  use uri;
end;
