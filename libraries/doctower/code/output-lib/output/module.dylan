module: dylan-user

define module output
   use common;
   use conditions, import: { file-error };
   use configs, import: { *template-directory*, *output-directory*, *package-title* };
   use ordered-tree;
   use markup-rep;
   use template-files;
   // from system
   use locators;
   use file-system, import: { ensure-directories-exist, copy-file, file-system-separator };
   // from template-engine
   use template-engine;
   // from transcendental
   use transcendental, import: { log => math-log };
   // from string-extensions
   use character-type;

   export
      topic-link-map, target-navigation-ids, output-file-info,
      target-link-info, write-output-file, output-templates,
      sanitized-xml;
end module;
