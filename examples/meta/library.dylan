library: meta
module: dylan-user
author: David Lichteblau (david.lichteblau@snafu.de)
copyright: Copyright (c) 1999 David Lichteblau

define library meta
  use common-dylan;
  use streams;
  // use collection-extensions;

  export meta;
end library;

define module meta
  use common-dylan;
  // use extensions;
  use streams;
  // use subseq;

  export with-meta-syntax, with-collector;
  export \meta-parse-aux, \process-meta, \call-meta-subroutine;
end module;
