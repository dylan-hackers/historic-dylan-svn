module: dylan-user

define module common
   use common-imports, export: all;
   use conditions, import: { file-error, file-not-found };
   
   // from system
   use file-system,
      import: { <file-system-error>, <file-does-not-exist-error>, <file-error>,
                file-error-locator, do-directory, delete-file,
                delete-directory => sys-delete-directory };
   use locators, import: { merge-locators };
   
   export *verbose?*, verbose-log, log, log-object;
   export with-file-error-handlers, with-open-file;
   export read-expected, read-lines-to-end, delete-directory;
   export merge-file-source-locations, $unknown-source-location;
   export item-string-list, group-elements, any-element;
   export add-row;
   export <case-insensitive-skip-list>, case-insensitive-less?;
end module;   
