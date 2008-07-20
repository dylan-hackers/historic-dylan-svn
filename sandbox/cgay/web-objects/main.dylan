Module: web-objects-example

define variable *warning* :: false-or(<string>) = #f;

// A modifiable record that stores one slot, a message.
//
define primary record <message-record> (<modifiable-record>)
  type-name: "message";
  pretty-name: "message";
  table-name: "tbl_message";
  database slot message :: <string>,
    init-value: " ",
    init-keyword: #"message",
    column-name: "message",
    column-number: 4;
end;

define taglib msg-taglib ()
end;

define class <edit-message-page> (<edit-record-page>)
end;

define variable $edit-message-page
  = make(<edit-message-page>, source: "edit-message.dsp");

define method respond-to-post (page :: <edit-message-page>)
  if (get-query-value("new-record") = "true")
    let msg = get-query-value("message");
    msg & (msg := trim(msg));
    if (msg & msg ~= "")
      *warning* := #f;
      create-new-message-record(msg);
    else
      *warning* := "You must enter a message.";
    end;
  end;
  next-method();          // process the DSP template
end;

define tag show-message in msg-taglib
    (page :: <edit-message-page>)
    ()
  output(message(*record*));
end;

define url-map /* for $http-server */
  url "/web-objects/edit-message"
    action GET () => $edit-message-page;
end;

// This is just used during development, since it's easier to debug a project
// when run as an executable rather than as a dynamically loaded Koala module.
// Don't call this (below) when linking as a DLL.
define method main () => ()
  start-server(make(<http-server>));
end;

begin
  main();
end;

