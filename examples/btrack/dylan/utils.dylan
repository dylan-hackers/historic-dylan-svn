Module: btrack
Author: Carl Gay


// Seems like this should be in dylan or common-dylan.
//
define method as
    (type == <integer>, value :: <string>) => (v :: <integer>)
  string-to-integer(value)
end;


define class <btrack-exception> (<simple-error>)
end;

// Thrown when a field in a web form fails to validate.
//
define class <invalid-form-field-exception> (<btrack-exception>)
end;


// I use this a lot, so a shorter name is useful.
//
define constant sformat :: <function> = format-to-string;


// Used to display a set of choices in a web form.
// e.g., for a <select> input field.  If the 'names' slot is
// #f then the values are displayed however Dylan prints them.
//
define class <option-menu> (<object>)
  slot value-generator :: <function>,
    required-init-keyword: #"generator";
  slot name-key :: <function> = method (x) sformat("%s", x) end,
    init-keyword: #"name-key";
  slot value-key :: <function> = identity,
    init-keyword: #"value-key";
  slot test-function :: <function> = \=,
    init-keyword: #"test";
end;

define function btrack-option-menu
    (generator, #key name-key, value-key, test) => (menu :: <option-menu>)
  make(<option-menu>,
       generator: iff(instance?(generator, <sequence>),
                      method () generator end,
                      generator),
       name-key: name-key | name,
       value-key: value-key | record-id,
       test: test | \=)
end;

define method display
    (stream :: <stream>, menu :: <option-menu>, current-selection)
  let name-key = name-key(menu);
  let val-key = value-key(menu);
  let test = test-function(menu);
  for (v in value-generator(menu)())
    format(stream, "<option value='%s'%s>%s</option>\n",
           val-key(v),
           iff(test(v, current-selection), " selected", ""),
           name-key(v));
  end;
end;

