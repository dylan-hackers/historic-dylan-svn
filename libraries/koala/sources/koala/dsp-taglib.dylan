Module:    dsp
Author:    Carl Gay
Synopsis:  Tags in the "dsp" taglib
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


//// Tags

//// Conditional tags

define thread variable *if-tag-test-result* = #"unbound";

// <dsp:if> exists so that if the test function is expensive, it need only be executed
// once, whereas using <dsp:when> and <dsp:unless> it would have to be executed twice
// (or cached).
//
// <dsp:if test="foo">
//   <dsp:then>foo then</dsp:then>
//   <dsp:else>foo else</dsp:else>
// </dsp:if>
//
define body tag \if in dsp
    (page :: <dylan-server-page>, response :: <response>, do-body :: <function>)
    (test :: <named-method>)
  dynamic-bind (*if-tag-test-result* = test(page, get-request(response)))
    // always process the body since there may be HTML outside the dsp:then
    // or dsp:else tags.
    do-body();
  end;
end;

define body tag \then in dsp
    (page :: <dylan-server-page>, response :: <response>, do-body :: <function>)
    ()
  when (*if-tag-test-result* & (*if-tag-test-result* ~= #"unbound"))
    do-body();
  end;
end;

define body tag \else in dsp
    (page :: <dylan-server-page>, response :: <response>, do-body :: <function>)
    ()
  unless (*if-tag-test-result*)
    do-body();
  end;
end;

// <dsp:when test="foo">
//   ...body...
// </dsp:when>
//
define body tag \when in dsp
    (page :: <dylan-server-page>, response :: <response>, do-body :: <function>)
    (test :: <named-method>)
  when (test(page, get-request(response)))
    do-body();
  end;
end;

// <dsp:unless test="foo">
//   ...body...
// </dsp:unless>
//
define body tag \unless in dsp
    (page :: <dylan-server-page>, response :: <response>, do-body :: <function>)
    (test :: <named-method>)
  unless (test(page, get-request(response)))
    do-body();
  end;
end;


//// Iteration tags

define thread variable *table-has-rows?* :: <boolean> = #f;
define thread variable *table-first-row?* :: <boolean> = #f;
define thread variable *table-row-data* :: <object> = #f;
define thread variable *table-row-number* :: <integer> = -1;

define function current-row () *table-row-data* end;
define function current-row-number () *table-row-number* end;

define body tag table in dsp
    (page :: <dylan-server-page>, response :: <response>, do-body :: <function>)
    (generator :: <named-method>)
  let stream = output-stream(response);
  write(stream, "<table");
  show-tag-call-attributes(stream, exclude: #[#"generator"]);
  write(stream, ">\n");
  let (rows, start-index, row-count) = generator(page);
  let len = size(rows);
  if (len == 0 | row-count == 0)
    dynamic-bind(*table-has-rows?* = #f,
                 *table-first-row?* = #t)  // so that dsp:hrow will execute
      do-body();
    end;
  else
    let start :: <integer> = start-index | 0;
    for (i from start below start + (row-count | len),
         first-row? = #t then #f,
         while: i < len)
      dynamic-bind (*table-has-rows?* = #t,
                    *table-row-data* = rows[i],
                    *table-row-number* = i,
                    *table-first-row?* = first-row?)
        do-body();
      end;
    end;
  end if;
  write(stream, "</table>");
end;

define body tag hrow in dsp
    (page :: <dylan-server-page>, response :: <response>, do-body :: <function>)
    ()
  when (*table-first-row?*)
    show-table-element(output-stream(response), "tr", do-body);
  end;
end;

define body tag row in dsp
    (page :: <dylan-server-page>, response :: <response>, do-body :: <function>)
    ()
  when (*table-has-rows?*)
    show-table-element(output-stream(response), "tr", do-body);
  end;
end;

define body tag hcell in dsp
    (page :: <dylan-server-page>, response :: <response>, do-body :: <function>)
    ()
  show-table-element(output-stream(response), "td", do-body);
end;

define body tag cell in dsp
    (page :: <dylan-server-page>, response :: <response>, do-body :: <function>)
    ()
  show-table-element(output-stream(response), "td", do-body);
end;

define body tag no-rows in dsp
    (page :: <dylan-server-page>, response :: <response>, do-body :: <function>)
    ()
  when (~ *table-has-rows?*)
    show-table-element(output-stream(response), "tr", do-body);
  end;
end;

define function show-table-element
    (stream, element-name :: <string>, do-body :: <function>)
  format(stream, "<%s", element-name);
  show-tag-call-attributes(stream);
  write(stream, ">");
  do-body();
  format(stream, "</%s>", element-name);
end;

define tag row-number in dsp
    (page :: <dylan-server-page>, response :: <response>)
    ()
  when (*table-row-number* >= 0)
    format(output-stream(response), "%d", *table-row-number* + 1);
  end;
end;


//// Date Tags

// Display a date.  If a key is given, it will be looked up in the given scope
// and should be a <date>, which will then be displayed according to timezone
// and style.
// @see parse-tag-arg(<string>, <date>)
//
define tag show-date in dsp
    (page :: <dylan-server-page>, response :: <response>)
    (timezone, style, date :: <date> = current-date(), key, scope)
  //---TODO: Finish this.  For now it can only show the current date.
  date-to-stream(output-stream(response), date);
end;

//// Internationalization tags

//// XML tags


