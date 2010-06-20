module: dylan-user

define module common
   use common-imports, export: all;
   use conditions, import: { file-error, file-not-found };
   
   // from system
   use file-system, import: { <file-system-error>, <file-does-not-exist-error> };
   
   export $verbose?, verbose-log, log, log-object,
          slot-visitor-definer,
          with-open-file, read-expected, read-lines-to-end,
          merge-file-source-locations, $unknown-source-location,
          item-string-list, group-elements, any-element,
          add-row,
          <case-insensitive-skip-list>, case-insensitive-less?;
end module;   
