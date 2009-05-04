module: dylan-user

define module parser-common
   use common, exclude: { source-location, file-locator, line-col-position };

   // from peg-parser
   use peg-parser;
   
   export <updatable-source-location-mixin>, <source-location-token>,
          source-location, source-location-setter,
          <file-parse-context>, source-location-from-stream-positions,
          file-locator, line-col-position, span-token-positions,
          note-source-location, note-combined-source-location;
end module;