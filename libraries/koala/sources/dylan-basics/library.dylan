Module:   dylan-user
Synopsis: Dylan utilities I can't live without
Author:   Carl Gay

define library dylan-basics
  use common-dylan;
  export dylan-basics;
end;

define module dylan-basics
  use dylan;
  use common-extensions;

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
    remove-keys,        // For removing keywords from #rest arglists.
    throw;
end;


