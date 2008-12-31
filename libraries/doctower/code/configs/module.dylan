module: dylan-user

define module configs
   use common;
   use internal-rep, import: { <topic-level-style> }, export: all;
   
   export
      $ascii-line-chars, $bullet-chars, $open-quote-chars, $close-quote-chars,
      $default-markup-quote-specs, $default-list-quote-specs,
      $default-title-quote-specs, $section-style, $tab-size;

end module;
