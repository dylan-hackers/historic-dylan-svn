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
    (page :: <dylan-server-page>, do-body :: <function>)
    (test :: <named-method>)
  dynamic-bind (*if-tag-test-result* = test(page))
    // always process the body since there may be HTML outside the dsp:then
    // or dsp:else tags.
    do-body();
  end;
end;

define body tag \then in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  when (*if-tag-test-result* & (*if-tag-test-result* ~= #"unbound"))
    do-body();
  end;
end;

define body tag \else in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
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
    (page :: <dylan-server-page>, do-body :: <function>)
    (test :: <named-method>)
  when (test(page))
    do-body();
  end;
end;

// <dsp:unless test="foo">
//   ...body...
// </dsp:unless>
//
define body tag \unless in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    (test :: <named-method>)
  unless (test(page))
    do-body();
  end;
end;



// ---TODO: Define a tag to replace the HTML <input> tag, that will automatically take
//          care of defaulting the value correctly if the form is redisplayed due to
//          error, and will allow CSS to display the input tag in a unique way.
//
define tag show-query-value in dsp (page :: <dylan-server-page>)
 (name :: <string>)
  let qv = get-query-value(name);
  qv & write(current-response().output-stream, qv);
end;


//// Date Tags

// Display a date.  If a key is given, it will be looked up in the given scope
// and should be a <date>, which will then be displayed according to timezone
// and style.
// @see parse-tag-arg(<string>, <date>)
//
define tag show-date in dsp (page :: <dylan-server-page>)
 (timezone, style, date :: <date> = current-date(), key, scope)
  //---TODO: Finish this.  For now it can only show the current date.
  date-to-stream(current-response().output-stream, date);
end;

//// HTTP Header Tags

define tag show-referer in dsp (page :: <dylan-server-page>)
 ()
  format(current-response().output-stream, "%s",
         header-value(#"Referer"));
end;

//// Internationalization tags

//// XML tags
