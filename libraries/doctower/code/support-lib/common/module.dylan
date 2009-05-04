module: dylan-user

define module common
   use common-imports, export: all;
   use conditions, import: { file-error, file-not-found };
   
   // from system
   use file-system, import: { <file-system-error>, <file-does-not-exist-error> };
   
   export $verbose?, verbose-log, log, log-object, slot-visitor-definer,
          merge-file-source-locations, item-string-list, with-open-file,
          group-elements, any-element, <case-insensitive-skip-list>,
          case-insensitive-less?, read-expected, read-lines-to-end;
end module;   
