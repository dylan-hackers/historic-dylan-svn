module: dylan-topics


define method formatted-paragraph (text :: <string>, #rest vals)
=> (paragraph :: <paragraph>)
   let para-text = apply(format-to-string, text, vals);
   make(<paragraph>, source-location: $generated-source-location,
        content: markup-seq(para-text));
end method;


define method location-paragraph (location :: <file-source-location>)
=> (paragraph :: <paragraph>)
   formatted-paragraph("Defined in %s line %d.",
         location.source-file, location.source-start-line);
end method;


define method list-of-directive (type :: <symbol>)
=> (directive :: <api-list-placeholder>)
   // TODO: What about scope? Can be deleted? Needs to be set later?
   make(<api-list-placeholder>, source-location: $generated-source-location,
        type: type)
end method;