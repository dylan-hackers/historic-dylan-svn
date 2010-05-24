module: dylan-user

define module configs
   use common;
   
   export
      $ascii-line-chars, $bullet-chars, $open-quote-chars, $close-quote-chars,
      $default-markup-quote-specs, $default-list-quote-specs,
      $default-title-quote-specs, $tab-size;

end module;
