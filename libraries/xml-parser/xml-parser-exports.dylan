module: dylan-user
author: Andreas Bogk <andreas@andreas.org>
copyright: (c) 2000 Gwydion Dylan Maintainers. Licensed under LGPL

define library xml-parser
  use common-dylan;
  use io;
  use melange-support;

  export xml-parser;
end library;

define module xml-parser
  use common-dylan;
  use format-out;
  use standard-io;
  use streams;
  use melange-support, exclude: { subclass };

end module;
