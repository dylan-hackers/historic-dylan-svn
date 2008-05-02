Module: koala-test-suite

define constant fmt = format-to-string;

define suite koala-test-suite ()
    suite header-test-suite;
end suite koala-test-suite;

define suite header-test-suite ()
  test test-date-header-parsing;
end suite header-test-suite;

define test test-date-header-parsing ()
  // RFC 2616 - 3.3.1
  // HTTP/1.1 clients and servers that parse the date value MUST accept
  // all three formats (for compatibility with HTTP/1.0), though they MUST
  // only generate the RFC 1123 format for representing HTTP-date values
  // in header fields. See section 19.3 for further information.
  //    Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
  //    Sunday, 06-Nov-94 08:49:37 GMT ; RFC 850, obsoleted by RFC 1036
  //    Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format
  let date = encode-date(1994, 11, 06, 08, 49, 37, time-zone-offset: 0);
  let test-dates = #(
    "Tue, 15 Nov 1994 12:45:26 GMT",  // rfc1123
    "Sun, 06 Nov 1994 08:49:37 GMT",  // rfc1123
    "Sunday, 06-Nov-94 08:49:37 GMT", // rfc850
    "Sun Nov  6 08:49:37 1994"        // ANSI C asctime (GMT)
    );
  for (test-date in test-dates)
    check-equal(fmt("Date %s parses correctly", test-date),
                date,
                parse-http-date(test-date, 0, test-date.size));
  end;
end test test-date-header-parsing;

define function main ()
  run-test-application(koala-test-suite);
end;

begin
  main();
end;


