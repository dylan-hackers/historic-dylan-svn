module: dylan-user
author: David Lichteblau (david.lichteblau@snafu.de)
copyright: Copyright (c) 1999 David Lichteblau

define library meta
  use common-dylan;
  use streams;
  export meta;
end library;

define module meta-base
  use common-dylan;
  use streams;
  export \with-meta-syntax, \with-collector;
  export \meta-parse-aux, \process-meta, \call-meta-subroutine;
end module meta-base;

define module meta-syntax
  use common-dylan;
  use meta-base;
  export \meta-definer, \collector-definer;

// internal-macro exports for gwydion d2c
  export \scan-helper, \scanner-builder, \meta-builder;
end module meta-syntax;

define module meta-types
  use common-dylan;
  export <space>, <digit>, <letter>, <num-char>, <any-char>, <graphic-char>;
end module meta-types;

define module meta
  use common-dylan;
  use meta-base, export: all;
  use meta-syntax, export: all;
  use meta-types, export: all;

  export scan-s, scan-word, scan-int, scan-number;
end module meta;

