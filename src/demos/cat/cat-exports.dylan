module: dylan-user
rcs-header: $Header: /scm/cvs/src/demos/cat/cat-exports.dylan,v 1.1.1.1.26.1 2003/10/23 20:32:45 housel Exp $

define library cat
  use Dylan;
  use IO;
  use System;
end;

define module cat
  use Dylan;
  use Extensions;
  use streams;
  use Standard-IO;
  use File-System;
end;
