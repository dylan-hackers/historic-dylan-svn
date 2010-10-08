module: dylan-user

define module configs
   use common;
   
   export
      *api-list-file*, *generated-topics-directory*, *topic-file-extension*,
      *contents-file-extension*, *config-file-extension*, *package-title*,
      *output-directory*, *output-types*, *template-directory*;
      
   export
      $ascii-line-chars, $bullet-chars, $open-quote-chars, $close-quote-chars,
      $default-markup-quote-specs, $default-list-quote-specs,
      $default-title-quote-specs, $tab-size;

end module;
