Module: dylan-user
Synopsis: Code shared by HTTP client and server

define library http-common
  use common-dylan,
    import: { dylan, common-extensions, threads, simple-random };
  use functional-dylan,
    import: { dylan-extensions };
  use io,
    import: { format, standard-io, streams };
  use logging;
  use system,
    import: { date, file-system, locators, operating-system };
  use strings;
  use uncommon-dylan;
  export http-common;
end library http-common;

define module http-common
  use common-extensions,
    exclude: { format-to-string };
  use date;
  use dylan;
  use dylan-extensions,
    import: { element-no-bounds-check,
              element-no-bounds-check-setter,
              element-range-check,
              element-range-error,
              // make-symbol,
              // case-insensitive-equal,
              // case-insensitive-string-hash
              <format-string-condition>
              };
  use file-system,
    import: { with-open-file,
              <file-does-not-exist-error>,
              <pathname> };
  use format;
  use locators;
  use logging;
  use standard-io;
  use streams;
  use strings;
  use threads;
  use uncommon-dylan;

  export
    // General one-off utilities
    wrapping-inc!,
    file-contents,
    pset,                // multiple-value-setq
    parent-directory,
    quote-html,          // Change < to &lt; etc

    <string-trie>, 
    find-object, 
    add-object, 
    remove-object,
    trie-children,
    trie-object,
    <trie-error>,

    <expiring-mixin>,
    expired?,
    date-modified,
    date-modified-setter,

    // Attributes
    <attributes-mixin>,
    get-attribute,
    set-attribute,
    remove-attribute;

  // Parsing
  create
    token-end-position;

end module http-common;
