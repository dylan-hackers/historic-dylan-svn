Module:    dylan-user
Synopsis:  Routines for reading Creatures 2 .gno files.
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.
License:   See License.txt

define module gno-file
  use common-dylan;
  use streams;

  // Add binding exports here.
  export gno-file-version,
    <gno-documentation>,
    gno-type,
    gno-type-setter,
    gno-subtype,
    gno-subtype-setter,
    gno-number,
    gno-number-setter,
    gno-description,
    gno-description-setter,
    gno-comment,
    gno-comment-setter,
    <gno-extra-documentation>,
    gno-comments,
    gno-comments-setter,
    gno-sequence,
    gno-sequence-setter,
    save-gno-file,
    load-gno-file;    
end module gno-file;
