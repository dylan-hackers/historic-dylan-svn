module: dylan-user

define module parser-common
   use common, exclude: { source-location, file-locator };
   use configs, import: { $tab-size };

   // from peg-parser
   use peg-parser;
   
   export canonical-text-stream, line-col-position-func;
   
   export <updatable-source-location-mixin>, <source-location-token>,
          source-location, source-location-setter,
          <file-parse-context>, <internal-parse-context>,
          source-location-from-stream-positions,
          file-locator, line-col-position, span-token-positions,
          note-source-location, note-combined-source-location;
end module;