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
    storage;
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

  export storage,
    dump-data,
    dumper,
    save,
    restore,
    restore-newest,
    version;
end;

define module web-framework
  use common-dylan;
  use object-table;
  use simple-xml;
  use format-out;
  use koala;
  use date;
  use dsp, import: { set-attribute, get-attribute };
  use dylan-extensions, import: { debug-name };

  use storage;

  export <reference-object>,
    visible?,
    visible?-setter;

  export <web-form-warning>,
    <web-success>,
    <web-error>,
    error-string;

  export respond-to-get,
    respond-to-post;

  export check;

  export edit-form,
    remove-form,
    add-form,
    list-forms;

  export browse-list,
    browse-table,
    remove-form,
    show,
    browse,
    to-table-header,
    to-table;

  export list-reference-slots,
    reference-slots,
    data-slots;

  export \web-class-definer;

  //commands
  export <add-command>,
    <remove-command>,
    <edit-command>;

  //changes
  export <change>,
    author,
    date,
    command,
    undo,
    redo,
    print-xml;


  //user stuff
  export <user>,
    username,
    password,
    email,
    admin?,
    current-user,
    valid-user?,
    login,
    logged-in,
    *users*;

end;


/*
define module changes
  use common-dylan;
  use xml;

  export <entry>;
end;
*/

