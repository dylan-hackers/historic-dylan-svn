Module:   dylan-user
Synopsis: Dylan utilities I can't live without
Author:   Carl Gay

define library dylan-basics
  use common-dylan;
  use io;
  export dylan-basics;
end;

define module dylan-basics
  use dylan;
  use common-extensions, exclude: { split };
  use streams, import: { write, with-output-to-string };

  export
    \bind,              // like LET* in Common Lisp
    \iff,               // more concise IF
    \with-restart,
    \with-simple-restart,
    <singleton-object>,
    \inc!,              // like ++foo
    \dec!,              // like --foo
    string-to-float,
    // Wasn't sure whether to include this, since FunDev already has
    // float-to-string, but decided to keep it with a different name.
    // --cgay
    float-to-formatted-string,
    join,
    split,
    remove-keys,        // For removing keywords from #rest arglists.
    raise,
    ignore-errors,
    table-keys,
    table-values;
end;


