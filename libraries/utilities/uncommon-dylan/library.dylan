Module:   dylan-user
Synopsis: Some definitions of general use that could be considered for
          inclusion in common-dylan if they stand the test of time.
Author:   Carl Gay

define library uncommon-dylan
  use common-dylan;
  use io;
  export uncommon-dylan;
end;

define module uncommon-dylan
  use dylan;
  use common-extensions;
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
    remove-keys,        // For removing keywords from #rest arglists.
    raise,
    ignore-errors,
    value-sequence,
    has-key?,
    count,

    wrapping-inc!,
    pset,                // multiple-value-setq

    <string-trie>,
    find-object,
    add-object,
    remove-object,
    trie-children,
    trie-object,
    <trie-error>,

    <nonnegative-integer>,
    <positive-integer>;

end module uncommon-dylan;

