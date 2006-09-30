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
    \with-storage,
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
    password-setter,
    email,
    email-setter,
    access,
    access-setter,
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

  //exports
  export generate-rss;
  
  //feed
  export <feed>,
    authors, authors-setter,
    categories, categories-setter,
    contributors, contributors-setter,
    generator, generato-setter,
    icon, icon-setter,
    identifier, identifier-setter,
    links, links-setter,
    logo, logo-setter,
    rights, rights-setter,
    subtitle, subtitle-setter,
    title, title-setter,
    updated, updated-setter,
    entries, entries-setter,
    language, language-setter,
    description, description-setter,
    published, published-setter;
  
  //entry
  export <entry>,
    authors, authors-setter,
    categories, categories-setter,
    content, content-setter,
    contributors, contributors-setter,
    identifier, identifier-setter,
    links, links-setter,
    published, published-setter,
    rights, rights-setter,
    source, source-setter,
    summary, summary-setter,
    title, title-setter,
    updated, updated-setter,
    comments, comments-setter,
    comments-count;

  export <category>,
    term, term-setter,
    scheme, scheme-setter,
    label, label-setter,
    description, description-setter; 

  export <generator>,
    uri, uri-setter,
    system-version, system-version,
    text, text-setter;
    
  export <content>,
    <raw-content>,
    <textile-content>,
    <xhtml-content>;
    
  //commands
  export <add-command>,
    <remove-command>,
    <edit-command>;

  export <comment>,
    name, name-setter,
    website, website-setter,
    body, body-setter,
    published, published-setter;
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

