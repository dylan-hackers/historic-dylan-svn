Module:    dylan-user
Synopsis:  String manipulation functions
Author:    Carl Gay
Copyright: This code is in the public domain.


define library strings
  use common-dylan;
  use functional-dylan,
    import: { dylan-extensions };
  use io,
    import: { streams, format-out };
  use string-extensions,
    import: { string-hacking };
  use regular-expressions;
  export strings;
end;

// Interface module
//
define module strings

  // Possible addtions...
  // translate
  // make-translation-table
  // center
  // justify
  // fill-paragraph
  // substring
  // starts-with?
  // ends-with?

  // String predicates
  create
    byte-string?;

  // Character predicates
  create
    alphabetic?,
    digit?,
    alphanumeric?,
    whitespace?,
    uppercase?,
    lowercase?,
    graphic?,
    printable?,
    control?;

  // Comparison/searching
  create
    equal?,
    less?,
    greater?,
    case-insensitive-equal?,
    case-insensitive-less?,
    case-insensitive-greater?,
    index-of,
    count-matches;

  // Creation/modification/conversion
  create
    trim,
    uppercase,
    uppercase!,
    lowercase,
    lowercase!,
    capitalize,
    capitalize!,
    pluralize,
    a-or-an,
    digit-to-integer,
    integer-to-digit;
    /* Should have all these basic conversion functions in common-dylan 
    character-to-string,
    string-to-integer, integer-to-string,
    string-to-float, float-to-string,
    */

  // Substrings
  create
    <substring>,
    <small-substring>,
    substring,
    substring-start,
    substring-end;

  // Fast byte string API
  create
    $cr,
    $lf,
    char-position,
    char-position-from-end,
    char-position-if,
    whitespace?,
    whitespace-position,
    skip-whitespace,
    trim-whitespace,
    looking-at?,
    key-match,
    string-match,
    string-position,
    string-equal?,
    digit-weight;

  // Non-copying substring
  create
    <substring>,
    substring,
    substring-base,
    substring-start,
    string-extent;
    
end module strings;


// Implementation module
//
define module strings-implementation
  use strings;            // Use API module
  use common-dylan;
  use format-out;
  use streams,
    import: { \with-output-to-string,
              write,
              write-element };
  use string-hacking,
    import: { // predecessor,
              // successor,
              // add-last,
              <character-set>,
              <case-sensitive-character-set>,
              <case-insensitive-character-set>,
              <byte-character-table> };
  use dylan-extensions,
    import: { element-no-bounds-check,
              element-no-bounds-check-setter,
              element-range-check,
              element-range-error,
              // make-symbol,
              // case-insensitive-equal,
              // case-insensitive-string-hash
              <format-string-condition>
              };
end module strings-implementation;

