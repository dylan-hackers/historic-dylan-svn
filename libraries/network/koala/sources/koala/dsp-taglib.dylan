Module:    dsp
Author:    Carl Gay
Synopsis:  Tags in the "dsp" taglib
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


//// Utils

// Used to generate XHTML elements using the current tag call's attributes.
//
define method show-element
    (stream :: <stream>, element-name :: <string>,
     #key exclude :: <sequence> = #(),
          body :: false-or(<function>))
  local method show-attribute (attr :: <symbol>, value)
          let attr-name = as(<string>, attr);  // for now...
          write(stream, " ");
          write(stream, attr-name);
          // This supports old-style attributes with no value.  For example,
          // IIRC <option name="foo" selected> used to be valid.  Not sure there's
          // much point in supporting this anymore, but it doesn't seem to hurt.
          if (value)
            let value = as(<string>, trim(value));
            if (~empty?(value))
              format(stream, "=\"%s\"", quote-html(value));
            end;
          end;
        end;
  format(stream, "<%s", element-name);
  map-tag-call-attributes(show-attribute, exclude: exclude);
  if (body)
    write(stream, ">");
    body();
    format(stream, "</%s>", element-name);
  else
    write(stream, "/>");
  end;
end;


//// Tags

// This is for comments that you don't want to be seen by the user,
// whereas HTML comments (<!-- ... -->) will be seen.  
define body tag comment in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  // don't call do-body
end;


// <dsp:get name="foo" context="c1,c2,..."/>
// where c1, c2 are form-notes|page|request|header|session.
// context defaults to "page".
// raw="true" means don't escape HTML characters.
//
define tag get in dsp
    (page :: <dylan-server-page>)
    (name :: <string>, context, tag, raw :: <boolean>)
  let value = get-context-value(name, context, tag: tag);
  if (value)
    let string = as(<string>, value);
    output("%s", iff(raw, string, quote-html(string)));
  end;
end tag get;

define method get-context-value
    (name :: <string>, context :: false-or(<string>), #key tag)
 => (value :: <object>)
  block (return)
    // Search contexts in order to find a value.  First one is displayed.
    for (context in split(context | "page", ','))
      let value = select (as(<symbol>, context))
                    page:    => get-attribute(page-context(), name);
                    request: => get-query-value(name);
                    headers: => get-header(current-request(), name);
                    session: => get-attribute(get-session(current-request()), name);
                    field-errors: => format-field-errors(get-field-errors(name), tag);
                    /* todo
                    otherwise =>
                      if (...debugging template tags?...)
                        signal(make(<koala-api-error>,
                                    format-string: "Bad context specified in "
                                      "<dsp:get> tag: %s",
                                    format-arguments: list(context)));
                      end;
                    */
                  end;
      if (value)
        return(value);
      end;
    end for;
  end block;
end method get-context-value;



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

// For use with <dsp:if>, <dsp:when>, and <dsp:unless>.  It grabs the
// 'name' and 'context' attributes out of those tag calls and uses them
// to see if the name exists in the given context.
//
define named-method exists? in dsp
    (page :: <dylan-server-page>)
  let name = get-tag-call-attribute(#"name");
  if (name)
    let context = get-tag-call-attribute(#"context");
    let value = get-context-value(name, context);
    value ~= #f
  end
end named-method exists?;



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

/* todo -- If one wants to do something fairly complicated in the
first loop iteration only, then it's not desirable/possible to use
"header=" in the loop element itself.  But once you move anything
inside a <dsp:when test="loop-start?"> then the "header=" becomes
almost useless because it must be output BEFORE whatever's in the
dsp:when element.  e.g.

   <dsp:loop over="user-group-names" var="group-name" footer="</ul>">
     <dsp:when test="loop-start?">
       <h3><wiki:show-user-username/> is a member of:</h3>
       <ul>
     </dsp:when>
     ...
   </dsp:loop>

Here you end up with XML in which the <ul> and </ul> aren't properly
nested in the template source, which makes editing tools less useful.
Is there any way around this?

I supposed one can use an outer dsp:when to decide whether to enter
the loop at all, but that likely means computing the loop collection
twice, which is unfortunate, and also means either writing a new test
(e.g., test="does-user-belong-to-any-groups?" for the above example).

It occurs to me that we could avoid having to write that extra named-method
by supporting a syntax like test="user-group-names.empty?.not", where
"empty?" and "not" and a few others are explicitly supported.  This
introduces a new mini-language, which is annoying, but it provides more
expressivity. The set of supported functions, like "not", could be defined
in a way similar to the way named-methods work, so that the user could
extend the mini-language to some degree.  But at some point simple 
chaining with "." isn't expressive enough and we'll want more.  Does
that way lie madness?
*/

define thread variable *loop-value* = #f;
define thread variable *loop-index* :: false-or(<integer>) = #f;
define thread variable *loop-start?* :: <boolean> = #f;
define thread variable *loop-end?* :: <boolean> = #f;

define method loop-index ()
  *loop-index*
end;

define method loop-value ()
  *loop-value*
end;

define body tag loop in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    (over :: <named-method>, var, header, footer)
  let items :: <collection> = over(page);
  for (item in items,
       i from 1)
    dynamic-bind (*loop-value* = item,
                  *loop-index* = i,
                  *loop-start?* = (i = 1),
                  *loop-end?* = (i = items.size))
      if (var)
        set-attribute(page-context(), var, *loop-value*);
      end;
      if (header & *loop-start?*)
        output("%s", header);
      end;

      do-body();

      if (footer & *loop-end?*)
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

define named-method loop-start? in dsp
    (page :: <dylan-server-page>)
  *loop-start?*
end;

define named-method loop-end? in dsp
    (page :: <dylan-server-page>)
  *loop-end?*
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
    show-element(output-stream(response), "tr", body: do-body);
  end;
end;

define body tag row in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  when (*table-has-rows?*)
    let response = current-response();
    show-element(output-stream(response), "tr", body: do-body);
  end;
end;

define body tag hcell in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  let response = current-response();
  show-element(output-stream(response), "td", body: do-body);
end;

define body tag cell in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  let response = current-response();
  show-element(output-stream(response), "td", body: do-body);
end;

define body tag no-rows in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    ()
  when (~ *table-has-rows?*)
    let response = current-response();
    show-element(output-stream(response), "tr", body: do-body);
  end;
end;

define tag row-number in dsp
    (page :: <dylan-server-page>)
    ()
  when (*table-row-number* >= 0)
    let response = current-response();
    output("%d", *table-row-number* + 1);
  end;
end;
 

//// Form field tags

// These tags are designed to directly replace HTML <input>, <textarea> and
// other widget tags.  They aid in correlating input errors with their
// corresponding widgets when a page is redisplayed.
/*
define tag input in dsp
    (page :: <dylan-server-page>)
    (name :: <string>, class, value)
  // Append "invalid-input" to the class so that it can be manipulated via CSS.
  if (get-form-error(name))
    class := iff(class,
                 concatenate(class, " invalid-input"),
                 "invalid-input");
  end;
  // Output a normal <input> element, but fill in the "value" attribute
  // if not explicitly provided in the tag.
  show-element(output-stream(current-response()), "input",
               attributes: make-table(value: => value | get-query-value(name),
                                      class: => class));
end;
*/


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

//// Form Field Errors

// Provides a mechanism for associating error messages with specific
// form fields.  Multiple errors can be associated with one key/field-name.

define abstract class <note> (<object>)
  constant slot note-text :: <string>,
    required-init-keyword: text:;
end;

define class <form-field-error> (<note>)
end;

// Errors are stored in a <string-table> in the <page-context> under this key.
//
define constant $field-errors-key = "dsp:field-errors";

// This uses <page-context> to store the form errors since they
// only need to be accessible during the processing of one page.
//
define method add-field-error
    (field-name :: <string>, message :: <string>,
     #rest format-arguments)
  let all-errors = get-attribute(page-context(), $field-errors-key)
                     | make(<string-table>);
  let error = make(<form-field-error>,
                   text: apply(format-to-string, message, format-arguments));
  let errors-for-field = element(all-errors, field-name, default: #());
  all-errors[field-name] := add-new!(errors-for-field, error,
                                     test: method (n1, n2)
                                             n1.note-text = n2.note-text
                                           end);
  set-attribute(page-context(), $field-errors-key, all-errors);
end method add-field-error;

define method add-field-error
    (field-name :: <string>, error :: <serious-condition>,
     #rest format-arguments)
  ignore(format-arguments);
  add-field-error(field-name, format-to-string("%s", error));
end;

// Get all the <field-error>s associated with a given field-name.
// This is a named-method so that it can be used with <dsp:loop>.
//
define named-method get-field-errors
    (field-name :: <string>)
 => (messages :: <sequence>)
  let message-table = get-attribute(page-context(), $field-errors-key);
  if (message-table)
    element(message-table, field-name, default: #[])
  else
    #[]
  end;
end;

// <dsp:show-field-errors field-name="field1,field2,..." tag="span"/>
//
define tag show-field-errors in dsp
    (page :: <dylan-server-page>)
    (field-name :: <string>, tag = "div")
  let error-table = get-attribute(page-context(), $field-errors-key);
  when (error-table)
    let field-errors = apply(concatenate,
                             map(get-field-errors, split(field-name, ',')));
    let field-errors = remove-duplicates!(field-errors,
                                          test: method (n1, n2)
                                                  n1.note-text = n2.note-text
                                                end);
    output("%s", format-field-errors(field-errors, tag));
  end;
end tag show-field-errors;

define body tag if-error in dsp
    (page :: <dylan-server-page>, do-body :: <function>)
    (field-name :: <string>, text :: false-or(<string>))
  let field-errors = get-field-errors(field-name);
  if (~empty?(field-errors))
    if (text)
      output("%s", text);
    end;
    do-body();
  end;
end tag if-error;

define method format-field-errors
    (errors :: <sequence>, tag :: false-or(<string>))
 => (string :: <string>)
  let tag = tag | "div";
  if (empty?(errors))
    ""
  else
    let messages = map(method (error :: <form-field-error>)
                         format-to-string("<%s class=\"field-error\">%s</%s>",
                                          tag, error.note-text, tag)
                       end,
                       errors);
    format-to-string("<%s class=\"field-errors\">%s</%s>\n",
                     tag, join(messages, "\n"), tag)
  end
end method format-field-errors;


//// Page notes and errors

// For errors and notes unrelated to specific form fields.  e.g., confirmation
// messages such as "successfully saved".

define class <page-note> (<note>)
end;

define constant $page-notes-key = "dsp:page-notes";

define constant $page-errors-key = "dsp:page-errors";

define method add-page-note
    (format-string :: <string>, #rest format-arguments)
  apply(add-page-note-internal, $page-notes-key, format-string, format-arguments);
end;

define method add-page-error
    (format-string :: <string>, #rest format-arguments)
  apply(add-page-note-internal, $page-errors-key, format-string, format-arguments);
end;

define method add-page-note-internal
    (key :: <string>, format-string :: <string>, #rest format-arguments)
  let notes = get-attribute(page-context(), key) | make(<stretchy-vector>);
  let text = apply(format-to-string, format-string, format-arguments);
  let note = make(<page-note>, text: text);
  add-new!(notes, note, test: method (n1, n2)
                                n1.note-text = n2.note-text
                              end);
  set-attribute(page-context(), key, notes);
end method add-page-note-internal;

define named-method page-errors?
    (page :: <dylan-server-page>)
  get-attribute(page-context(), $page-errors-key)
end;

define named-method page-notes?
    (page :: <dylan-server-page>)
  get-attribute(page-context(), $page-notes-key)
end;

define tag show-page-notes in dsp
    (page :: <dylan-server-page>)
    ()
  show-page-notes-internal($page-notes-key, "page-note");
end;

define tag show-page-errors in dsp
    (page :: <dylan-server-page>)
    ()
  show-page-notes-internal($page-errors-key, "page-error");
end;

define method show-page-notes-internal
    (key :: <string>, css-class :: <string>)
  let notes = get-attribute(page-context(), key);
  if (notes)
    // Note the 's' added to the end of the class name in the outer div.  :-/
    output("<div class=\"%ss\">", css-class);
    for (note in notes)
      output("<div class=\"%s\">%s</div>", css-class, note.note-text);
    end;
    output("</div>")
  end;
end method show-page-notes-internal;

// So that responder methods can decide whether to redisplay the same
// page with errors highlighted or commit the changes, without having
// to manually track whether they called add-{page/field}-error.
//
define method page-has-errors?
    () => (errors? :: <boolean>)
  let page-errors = get-attribute(page-context(), $page-errors-key);
  let field-errors = get-attribute(page-context(), $field-errors-key);
  (page-errors & page-errors.size > 0)
    | (field-errors & field-errors.size > 0)
end;

//// Debug tags

define tag show-query-values in dsp
    (page :: <dylan-server-page>)
    ()
  output("<ul>\n");
  for (value keyed-by name in request-query-values(current-request()))
    output("<li>%s: %s</li>\n", name, quote-html(value));
  end;
  output("</ul>\n");
end tag show-query-values;


