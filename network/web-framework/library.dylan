module: dylan-user
author: Hannes Mehnert <hannes@mehnert.org>

define library web-framework
  use common-dylan;
  use dylan;
  use io;
  use koala, import: { koala, dsp };
  use xml-parser;
  use system, import: { file-system, date, locators };
  use dood;
  use uri;

  export object-table,
    web-framework,
    storage,
    users,
    changes,
    change,
    permission;
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
  use koala;
  use format-out;
  use locators;

  export storage,
    \with-storage,
    \storage-class-definer,
    dump-data,
    dumper,
    query-dump,
    do-dump,
    save,
    setup,
    restore,
    restore-newest,
    //version,
    storage-type,
    key;
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
  //export <access-level>;

  export <user>,
    username,
    username-setter,
    password,
    password-setter,
    email,
    email-setter,
    additional-information,
    additional-information-setter,
    authenticated-user,
    find-user,
    authenticate,
    login,
    logout;
    //valid-user?;
end;

define module change
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
    print-xml,
    print-change;

  //commands
  export <add-command>,
    <remove-command>,
    <edit-command>;

end;
define module changes
  use common-dylan;
  use dylan;
  use date;
  use xml-parser,
    import: { node-children, node-children-setter,
              parse-document, root, <element>,
              name-setter => xml-name-setter};
  use simple-xml;
  use koala;
  use object-table;
  use storage;
  use web-framework-macro;
  use users;
  use change, export: all;
  //exports
  export generate-rss, generate-atom;
  
  //feed
  export <feed>,
    authors, authors-setter,
    categories, categories-setter,
    contributors, contributors-setter,
    generator, generator-setter,
    icon, icon-setter,
    identifier, identifier-setter,
    links, links-setter,
    logo, logo-setter,
    rights, rights-setter,
    subtitle, subtitle-setter,
    title, title-setter,
    updated, updated-setter,
    entries, entries-setter,
    languages, languages-setter,
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
    <markup-content>,
    <textile-content>,
    <xhtml-content>,
    type;
    
  export <comment>,
    name, name-setter,
    website, website-setter,
    content, content-setter,
    published, published-setter;

  export <pingback>,
    pingback-source, pingback-source-setter,
    content, content-setter,
    published, published-setter;

  export <uri>;
  
  export <link>,
    href, href-setter;
end;

define module permission
  use common-dylan;

  export with-permission,
    <permission-error>,
    <authentication-error>,
    permitted?,
    permission-error,
    authentication-error;
end;

define module web-framework
  use common-dylan,
    exclude: { format-to-string };
  use object-table;
  use simple-xml;
  use xml-parser,
    rename: { <element> => <xml-element> };
  use koala;
  use dsp;
  use format;
  use uri;

  use web-framework-macro, export: all;
  use storage;
  use change;
  use users;
  use permission;

  export *errors*,
    *action*,
    *form*;

  export action-test-definer,
    action-tests-definer,
    object-test-definer,
    object-tests-definer,
    error-test-definer,
    error-tests-definer;

end;
