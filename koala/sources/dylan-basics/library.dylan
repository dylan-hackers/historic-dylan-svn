Module:   dylan-user
Synopsis: Dylan utilities I can't live without
Author:   Carl Gay

define library dylan-basics
  use common-dylan;
  export dylan-basics;
end;

define module dylan-basics
  use common-dylan;

  export
    \iff,
    \with-restart,
    \with-simple-restart,
    <singleton-object>,
    \inc!,
    \dec!,
    string-to-float,
    // Wasn't sure whether to include this, since FunDev already has
    // float-to-string, but decided to keep it with a different name.
    // --cgay
    float-to-formatted-string;
end;


