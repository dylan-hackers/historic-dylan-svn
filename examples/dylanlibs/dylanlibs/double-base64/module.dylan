Module:    dylan-user
Synopsis:  Base64 encoding/decoding routines
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt


define module base64
  use common-dylan;

  // Add binding exports here.
  export 
    <base64>, 
    base64-string,
    base64-encode,
    base64-decode-as,
    <base64-byte>,
    <base64-vector>;
end module base64;
