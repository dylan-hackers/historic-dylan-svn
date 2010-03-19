Module: dylan-user
Synopsis: MIME tools
Author: Carl Gay

define library mime
  use common-dylan;
  use io,
    import: { streams };
  use strings;
  use system,
    import: { file-system };
  export mime;
end;

define module mime
  create
    <mime-type>,
    mime-type,
    mime-subtype,
    mime-name;

  // Comparisons
  // =

  // Mappings
  create
    <mime-type-map>,
    load-mime-types,
    $default-mime-type-map,
    extension-to-mime-type,
    extension-to-mime-type-setter;
end module mime;

define module mime-internal
  use common-dylan;
  use file-system;
  use mime;
  use streams;
  use strings,
    import: { trim };
end module mime-internal;
