Module:   dylan-user
Synopsis: Dylan Survival Kit -- Basic dylan utilities
Author:   Carl Gay

define library dysuki
  use common-dylan;
  export dysuki;
end;

define module dysuki
  use common-dylan;

  export
    \iff,
    \with-restart,
    \with-simple-restart,
    <sealed-constructor>,
    <singleton-object>,
    \inc!,
    \dec!,
    string-to-float,
    // Wasn't sure whether to include this, since FunDev already has
    // float-to-string, but decided to keep it with a different name.
    // --cgay
    float-to-formatted-string;
end;


