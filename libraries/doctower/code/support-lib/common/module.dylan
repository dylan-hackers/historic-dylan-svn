module: dylan-user

define module common
   use common-imports, export: all;
   use conditions, import: { file-error, file-not-found };
   
   // from system
   use file-system, import: { <file-system-error>, <file-does-not-exist-error> };
   
   export *verbose?*, verbose-log, log, log-object;
   export with-open-file, read-expected, read-lines-to-end;
   export merge-file-source-locations, $unknown-source-location;
   export item-string-list, group-elements, any-element;
   export add-row;
   export <case-insensitive-skip-list>, case-insensitive-less?;
end module;   
