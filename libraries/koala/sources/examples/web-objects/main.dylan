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

define page edit-message-page (<edit-record-page>)
    (url: "/edit-message.dsp",
     source: document-location("edit-message.dsp"))
end;

define method respond-to-post (page :: <edit-message-page>,
                               request :: <request>,
                               response :: <response>)
  if (get-form-value("new-record") = "true")
    let msg = get-form-value("message");
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
    (page :: <edit-message-page>, response :: <response>)
    ()
  log-debug("writing message output");
  write(output-stream(response), message(*record*));
end;

// This is just used during development, since it's easier to debug a project
// when run as an executable rather than as a dynamically loaded Koala module.
// Don't call this (below) when linking as a DLL.
define method main () => ()
  start-server();
end;

main();

