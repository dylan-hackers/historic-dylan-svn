Module: dylan-user
RCS-Header: $Header: /scm/cvs/src/demos/html2txt/html2txt-exports.dylan,v 1.1.1.1.22.1 2004/07/22 16:36:23 housel Exp $

define library html
  use dylan;
  use io;
  use system;
  use collection-extensions;
  use string-extensions;
end library html;

define module html
  use dylan;
  
  // A few basic definitions not present in the Dylan spec
  use extensions, import: {main, %main};
  
  // Additional collection classes and operations from "collection-extensions"
  use subseq;
  use self-organizing-list;

  // From string-extensions:
  use substring-search;
  
  // I/O support from the "streams" and "standard-io" libraries
  use streams;
  use standard-io;
  use file-system;
  
  export html2text;
end module html;
