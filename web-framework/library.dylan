module: dylan-user
author: Hannes Mehnert <hannes@mehnert.org>

define library web-framework
  use common-dylan;
  use dylan;
  use io;
  use koala, import: { koala, dsp };
  use xml-parser, import: { simple-xml };
  use system, import: { date };

  export object-table,
    web-framework;
end;

define module object-table
  use common-dylan;
  use dylan-extensions,
    import: { address-of, <string-table> };

  export get-reference,
    get-object;
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
    print-xml,
    get-all-changes,
    set-changes,
    add-change;


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

