Module:    dsp
Author:    Carl Gay
Synopsis:  Tags in the "dsp" taglib
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


//// Tags

// This is for comments that you don't want to be seen by the user,
// whereas HTML comments (<!-- ... -->) will be seen.  
define body tag comment in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  // don't call do-body
end;


// <dsp:get name="foo" context="page|request|header|session"/>
//
define tag get in dsp
    (page :: <dylan-server-page>)
    (name :: <string>, context :: <string>)
  let value = select (as(<symbol>, context))
                #"page" => get-attribute(page-context(), name);
                #"request" => get-query-value(name);
                #"header" => get-header(current-request(), name);
                #"session" => get-attribute(get-session(current-request()), name);
                // #"any" => ...
                // any other context?
                otherwise =>
                  // todo -- what error class?
                  error("Bad context specified in <dsp:get> tag: %s", context)
              end;
  output("%s", value | "");     // don't show #f
end tag get;


//// Conditional tags

define thread variable *if-tag-test-result* = #"unbound";

// <dsp:if> exists so that if the test function is expensive, it need
// only be executed once, whereas using <dsp:when> and <dsp:unless>
// it would have to be executed twice (or cached).
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


//// Iteration tags

// loop
// 
// This stores the current iteration value in the page context because
// it's convenient to be able to use <dsp:get> to access it rather than
// defining a new tag such as, for example, <dsp:loop-value>.
//
// I can imagine adding more parameters for this tag, such as start,
// end, limit, etc.

/* Example, from the wiki:

      <h2><wiki:show-group-name/></h2>
      <dsp:loop over="group-member-names" var="user-name"
                header="<ul>" footer="</ul>">
        <li><dsp:get name="user-name" context="page"/></li>
      </dsp:loop>
*/
define thread variable *loop-value* = #f;
define thread variable *loop-index* :: false-or(<integer>) = #f;
define thread variable *loop-first?* :: <boolean> = #f;
define thread variable *loop-last?* :: <boolean> = #f;

define body tag loop in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    (over :: <named-method>, var, header, footer)
  let items :: <collection> = over(page);
  for (item in items,
       i from 1)
    dynamic-bind (*loop-value* = item,
                  *loop-index* = i,
                  *loop-first?* = (i = 1),
                  *loop-last?* = (i = items.size))
      if (var)
        set-attribute(page-context(), var, *loop-value*);
      end;
      if (header & *loop-first?*)
        output("%s", header);
      end;

      do-body();

      if (footer & *loop-last?*)
        output("%s", footer);
      end;
    end;
  end for;
  // Scope the loop variable to the loop, not the entire page.
  remove-attribute(page-context(), var);
end tag loop;

define tag loop-index in dsp
    (page :: <dylan-server-page>)
    ()
  // *loop-index* starts at 1.
  if (*loop-index*)
    output("%d", *loop-index*);
  end;
end;

define named-method loop-first? in dsp
    (page :: <dylan-server-page>)
  *loop-first?*
end;

define named-method loop-last? in dsp
    (page :: <dylan-server-page>)
  *loop-last?*
end;


define thread variable *table-has-rows?* :: <boolean> = #f;
define thread variable *table-first-row?* :: <boolean> = #f;
define thread variable *table-row-data* :: <object> = #f;
define thread variable *table-row-number* :: <integer> = -1;

define function current-row () *table-row-data* end;
define function current-row-number () *table-row-number* end;

define body tag table in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    (generator :: <named-method>)
  let response = current-response();
  let stream = output-stream(response);
  write(stream, "<table");
  show-tag-call-attributes(stream, exclude: #[#"generator"]);
  write(stream, ">\n");
  // Generator functions must return rows, but start-index and row-count
  // are optional.
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
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  when (*table-first-row?*)
    let response = current-response();
    show-table-element(output-stream(response), "tr", do-body);
  end;
end;

define body tag row in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  when (*table-has-rows?*)
    let response = current-response();
    show-table-element(output-stream(response), "tr", do-body);
  end;
end;

define body tag hcell in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  let response = current-response();
  show-table-element(output-stream(response), "td", do-body);
end;

define body tag cell in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  let response = current-response();
  show-table-element(output-stream(response), "td", do-body);
end;

define body tag no-rows in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  when (~ *table-has-rows?*)
    let response = current-response();
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
    (page :: <dylan-server-page>)
    ()
  when (*table-row-number* >= 0)
    let response = current-response();
    output("%d", *table-row-number* + 1);
  end;
end;
 

// ---TODO: Define a tag to replace the HTML <input> tag, that will
//          automatically take care of defaulting the value correctly
//          if the form is redisplayed due to error, and will allow
//          CSS to display the input tag in a unique way.
//
define tag show-query-value in dsp (page :: <dylan-server-page>)
 (name :: <string>)
  let qv = get-query-value(name);
  qv & write(output-stream(current-response()), qv);
end;


//// Date Tags

// Display a date.  If a key is given, it will be looked up in the
// given scope and should be a <date>, which will then be displayed
// according to the given format (a la strftime).
//
// @see parse-tag-arg(<string>, <date>)
//
define tag show-date in dsp
    (page :: <dylan-server-page>)
    (date :: <date> = current-date(), format, key, scope)
  //---TODO: Finish this.  For now it can only show the current date.
  date-to-stream(output-stream(current-response()), date);
end;

//// HTTP Header Tags

// This can be replaced by <dsp:get name="Referer" context="header/> now.
//
define tag show-referer in dsp
    (page :: <dylan-server-page>)
    ()
  output("%s", get-header(current-request(), "Referer"));
end;


//// Internationalization tags

// Nothing yet, I guess.


//// XML tags

// Nothing yet, I guess.


// A simple error reporting mechanism.  Store errors in the page context
// so they can be displayed when the next page is generated.  The idea is
// that pages should use the <dsp:show-form-notes/> tag if they can be
// the target of a GET or POST that might generate errors.

define abstract class <form-note> (<object>)
  constant slot format-string :: <string>,
    required-init-keyword: #"format-string";
  constant slot format-arguments :: <sequence>,
    required-init-keyword: #"format-arguments";
end;

define class <form-error> (<form-note>)
  constant slot form-field-name :: false-or(<string>) = #f,
    init-keyword: #"form-field-name";
end;

define class <form-message> (<form-note>)
end;

define method note-form-error
    (cond :: <condition>,
     #key field-name, format-arguments :: <sequence> = #())
  note-form-error(format-to-string("%s", cond),
                  field-name: field-name,
                  format-arguments: format-arguments);
end;

define method note-form-error
    (message :: <string>,
     #key field-name, format-arguments :: <sequence> = #())
  add-form-note(make(<form-error>,
                     format-string: message,
                     format-arguments: format-arguments,
                     form-field-name: field-name))
end;

// todo -- needs field-name parameter
// todo -- this and note-form-error need to handle multiple messages for
//         the same field name
define method note-form-message
    (message :: <string>, #rest args)
  add-form-note(make(<form-message>,
                     format-string: message,
                     format-arguments: copy-sequence(args)));
end;

define constant $form-notes-key = "dsp:form-notes";

// This shows the use of <page-context> to store the form errors since they
// only need to be accessible during the processing of one page.
//
define method add-form-note
    (note :: <form-note>)
  let context :: <page-context> = page-context();
  let notes = get-attribute(context, $form-notes-key) | make(<stretchy-vector>);
  add!(notes, note);
  set-attribute(context, $form-notes-key, notes);
end;

define method display-form-note
    (out :: <stream>, note :: <form-error>)
  write(out, "<li>");
  // Should I call quote-html on this output?
  apply(format, out, format-string(note), format-arguments(note));
  write(out, "</li>\n");
end;

define method display-form-note
    (out :: <stream>, note :: <form-message>)
  write(out, "<p>");
  // Should I call quote-html on this output?
  apply(format, out, format-string(note), format-arguments(note));
  write(out, "</p>\n");
end;
  
define tag show-form-notes in dsp
    (page :: <dylan-server-page>)
    ()
  let notes = get-attribute(page-context(), $form-notes-key);
  when (notes)
    let messages = choose(rcurry(instance?, <form-message>), notes);
    let errors = choose(rcurry(instance?, <form-error>), notes);
    let out = output-stream(current-response());
    write(out, "<div class=\"form-notes\">\n");
    unless(empty?(messages))
      write(out, "<div class=\"form-note-message\">\n");
      do(curry(display-form-note, out), messages);
      write(out, "</div>\n");
    end;
    unless(empty?(errors))
      format(out, "<div class=\"form-note-errors\">Please fix the following errors:\n<ul>\n");
      do(curry(display-form-note, out), errors);
      format(out, "</ul></div>\n");
    end;
    write(out, "</div>\n");
  end;
end;

