module: dylan-user

define module common
   use common-imports, export: all;
   use conditions, import: { file-error, file-not-found };
   
   // from system
   use file-system, import: { <file-system-error>, <file-does-not-exist-error> };
   
   export log, log-object, slot-visitor-definer, merge-file-source-locations,
          item-string-list, with-open-file;
end module;   
