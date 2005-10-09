module: dylan-user
author: Hannes Mehnert <hannes@mehnert.org>

define library buddha
  use common-dylan;
  use io;
  use koala, import: { koala };
  use dood;
  use string-extensions, import: { character-type };
  use regular-expressions;
  use network;
  use system, import: { file-system };
  use xml-rpc-common;
  use xml-parser;
  use dylan;
  export buddha;
end;

define module web-macro
  use dylan;

  export <slot>,
    slot-name,
    slot-type,
    slot-getter-method,
    slot-setter-method,
    slot-global-list;
 
  export list-reference-slots,
    reference-slots,
    data-slots;

  export \web-class-definer;
end;

define module xml
  use dylan;
  use xml-parser;
  
  export \with-xml,
    \with-xml-builder,
    escape-html;
end;

define module buddha
  use common-dylan;
  use dylan-extensions, exclude: { slot-type };
  use threads;
  use format-out;
  use format, import: { format };
  use print, import: { print-object };
  use koala, exclude: { print-object };
  use streams;
  use standard-io;
  use character-type, import: { hex-digit? };
  use dood;
  use regular-expressions;
  use sockets, import: { <tcp-socket>, <internet-address> };
  use file-system;
  use xml-rpc-common, import: { base64-encode, base64-decode };
  use xml;
  use web-macro;
end;
