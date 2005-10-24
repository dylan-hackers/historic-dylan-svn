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
  use common-dylan;

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
  use common-dylan;
  use xml-parser;
  
  export \with-xml,
    \with-xml-builder,
    escape-html;
end;

define module object-table
  use common-dylan;
  use dylan-extensions, import: { address-of,
                                  <string-table> };

  export get-reference,
    get-object;
end;

define module utils
  use common-dylan;
  use dylan-extensions, import: { debug-name };
  use regular-expressions;
  export exclude,
    regexp-match,
    get-url-from-type,
    <wrapper-sequence>,
    <mutable-wrapper-sequence>,
    data;
end;

/*
define module class-editor
  use common-dylan;
  use xml;
  use web-macro;
  use object-table;
  use utils;
  export edit-form,
    remove-form,
    add-form,
    check;
end;
*/

define module class-browser
  use common-dylan;
  use xml;
  use web-macro;
  use object-table;
  use format-out;
  use utils;
//  use class-editor;
  export browse-list,
    browse-table,
    remove-form; //this shouldn't be here
end;

define module buddha
  use common-dylan;
  use dylan-extensions, exclude: { slot-type };
  use threads;
  use format-out;
  use format, import: { format };
  use print, import: { print-object };

  use streams;
  use standard-io;
  use character-type, import: { hex-digit? };

  use koala, exclude: { print-object };
  use sockets, import: { <tcp-socket>, <internet-address> };

  use dood;
  use file-system;
  use xml-rpc-common, import: { base64-encode, base64-decode };

  use xml;
  use web-macro;
  use object-table;
  use class-browser;
//  use class-editor;
  use utils;
end;
