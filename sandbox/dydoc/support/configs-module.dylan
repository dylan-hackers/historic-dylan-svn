module: dylan-user

define module configs
   use common;
   use internal-rep, import: { <topic-level-style> };
   
   export
      $ascii-line-chars, $bullet-chars, $open-quote-chars, $close-quote-chars,
      $default-quote-spec, $default-list-quote-spec, $section-style, $tab-size;

end module;
