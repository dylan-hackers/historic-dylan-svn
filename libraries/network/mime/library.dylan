Module: dylan-user
Synopsis: MIME tools
Author: Carl Gay

define library mime
  use common-dylan;
  use dylan,
    import: { dylan-extensions };
  use io,
    import: { format, streams };
  use strings;
  use system,
    import: { file-system };
  export mime;
  export mime-internal;
end library mime;

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
  use dylan-extensions,
    import: { string-hash };
  use file-system,
    import: { <pathname>, with-open-file };
  use format,
    import: { format };
  use mime;
  use streams,
    import: { with-open-file, read-line, with-output-to-string };
  use strings,
    import: { trim };
end module mime-internal;
