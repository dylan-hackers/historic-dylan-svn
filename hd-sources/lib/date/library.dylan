Module:    dylan-user
Author:    Jonathan Bachrach, Gary Palter
Synopsis:  The Date library API
Copyright: 1996 The Harlequin Group Limited.  All rights reserved.

define library date
  use harlequin-dylan;
  use operating-system;
  export date;
end library date;

define module date
  use harlequin-dylan;
  use dylan-direct-c-ffi;
  use operating-system;
  use simple-format;
  export
    <date>,
    encode-date, decode-date,
    date-year,
    date-month,
    date-day,
    date-hours,
    date-minutes,
    date-seconds,
    date-microseconds,
    date-time-zone-offset, date-time-zone-offset-setter,
    <day-of-week>,
    date-day-of-week,
    as-iso8601-string,
    current-date,
    current-timestamp,
    local-time-zone-offset,
    local-time-zone-name,
    //---*** Other time zone tools?  (I.e., conversions from/to zone name to/from offset)
    local-daylight-savings-time?,
    <duration>,
    <year/month-duration>, <day/time-duration>,
    encode-year/month-duration, encode-day/time-duration,
    decode-duration,
    <date-arithmetic-error>, <date-arithmetic-invalid-day>,
    <date-arithmetic-error-restart>, <date-arithmetic-use-last-valid-day>;
end module date;

