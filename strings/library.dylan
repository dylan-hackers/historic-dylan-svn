Module:    dylan-user
Synopsis:  String manipulation functions
Author:    Carl Gay
Copyright: This code is in the public domain.


define library strings
  use common-dylan;
  use io, import: { streams };
  export strings;
end;

define module strings
  use common-dylan, exclude: { split };
  use streams, import: { with-output-to-string, write, write-element };

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
    count-occurrances;

  // Creation/modification:
  create
    join,
    trim,  // or strip?
    split,
    replace,
    //replace!,  is it worth having this?  old/new must be same size
    upcase,
    upcase!,
    downcase,
    downcase!,
    capitalize,
    capitalize!,
    pluralize,
    a-or-an;

  // Conversion
  create
    //as(<string>, <character>)
    //as(<integer>, <string>)
    //as(<string>, <integer>)
    //as(<float>, <string>)
    //as(<string>, <float>)
    //as(<double-float>, <string>)
    //as(<string>, <double-float>)
    digit-to-integer,              // base:
    integer-to-digit;              // base:

end module strings;

