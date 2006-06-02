module: dylan-user
author: Hannes Mehnert <hannes@mehnert.org>

define library web-framework
  use common-dylan;
  use dylan;
  use io;
  use koala, import: { koala, dsp };
  use xml-parser, import: { simple-xml };
  use system, import: { file-system, date };
  use dood;

  export object-table,
    web-framework,
    storage,
    users,
    changes;
end;

define module object-table
  use common-dylan;
  use dylan-extensions,
    import: { address-of, <string-table> };

  export get-reference,
    get-object;
end;

define module storage
  use common-dylan;
  use dood;
  use file-system;
  use threads;
  use format-out;
  use koala;

  export storage,
    dump-data,
    dumper,
    save,
    restore,
    restore-newest,
    version,
    storage-type;
end;

define module web-framework-macro
  use common-dylan;
  use dylan-extensions, import: { debug-name };

  export list-reference-slots,
    reference-slots,
    data-slots;

  export \web-class-definer;

  export <reference-object>,
    visible?,
    visible?-setter;

  export <slot>,
    slot-name,
    slot-type,
    slot-getter-method,
    slot-setter-method,
    default,
    default-function,
    default-help-text;

  export check,
    key,
    show,
    get-url-from-type;

  export <triple>,
    slot-name,
    old-value,
    new-value;

  export <web-form-warning>,
    <web-success>,
    <web-error>,
    error-string;
end;

define module users
  use common-dylan;
  use dylan;
  use dsp, import: { set-attribute, get-attribute };
  use koala;
  use storage;
  use web-framework-macro;

  //user stuff
  export <user>,
    username,
    password,
    email,
    access,
    current-user,
    login,
    logged-in?;
end;

define module changes
  use common-dylan;
  use dylan;
  use date;
  use simple-xml;

  use object-table;
  use storage;
  use web-framework-macro;
  use users;

  //changes
  export <change>,
    author,
    date,
    command,
    undo,
    redo,
    print-xml;

  //commands
  export <add-command>,
    <remove-command>,
    <edit-command>;
end;


define module web-framework
  use common-dylan;
  use object-table;
  use simple-xml;
  use format-out;
  use koala;
  
  use web-framework-macro, export: all;
  use storage;
  use changes;

  export respond-to-get,
    respond-to-post;

  export edit-form,
    remove-form,
    add-form,
    list-forms;

  export browse-list,
    browse-table,
    remove-form,
    browse,
    to-table-header,
    to-table;
end;

