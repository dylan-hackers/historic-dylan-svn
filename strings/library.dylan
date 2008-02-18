Module:    dylan-user
Synopsis:  String manipulation functions
Author:    Carl Gay
Copyright: This code is in the public domain.


define library strings
  use common-dylan;
  use io,
    import: { streams };
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
    substring,
    trim,
    replace,
    replace!,
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

end module strings;


// Implementation module
//
define module strings-implementation
  use strings;            // Use API module
  use common-dylan;
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
end module strings-implementation;
