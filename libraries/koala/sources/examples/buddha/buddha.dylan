module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define variable *config* = make(<config>,
                                config-name: "config");

//list containing recent changes
define variable *changes* = #();

define variable *version* = 0;

define class <buddha> (<object>)
  constant slot config :: <config> = *config*;
  constant slot version :: <integer> = *version*;
  constant slot changes = *changes*;
end;

define variable *directory* = "www/buddha/";

define sideways method process-config-element
    (node :: <xml-element>, name == #"buddha")
  let cdir = get-attr(node, #"content-directory");
  if (~cdir)
    log-warning("Buddha - No content-directory specified!");
  else
    *directory* := cdir;
  end;
  log-info("Buddha content directory = %s", *directory*);
end;

define generic respond-to-get (page,
                               request :: <request>,
                               response :: <response>,
                               #key errors);

define macro page-definer
  { define page ?:name end }
    => { define responder ?name ## "-responder" ("/" ## ?"name")
           (request, response)
           if (request.request-method = #"get")
             respond-to-get(as(<symbol>, ?"name"), request, response)
           elseif (request.request-method = #"post")
             respond-to-post(as(<symbol>, ?"name"), request, response)
           end;
         end; }
end;

define responder default-responder ("/")
  (request, response)
  respond-to-get(#"network", request, response);
end;

define page network end;
define page subnet end;
define page vlan end;
define page host end;
define page zone end;
define page user end;
define page save end;
define page restore end;
define page browse end;
define page edit end;
define page changes end;

define macro with-buddha-template
  { with-buddha-template(?stream:variable, ?title:expression)
      ?body:*
    end }
    => { begin
           let page = with-xml-builder()
html(xmlns => "http://www.w3.org/1999/xhtml") {
  head {
    title(concatenate("Buddha - ", ?title)),
    link(rel => "stylesheet", href => "/buddha.css")
  },
  body {
    div(id => "header") {
      div(id => "navbar") {
        a("Network", href => "/network"),
        a("Subnet", href => "/subnet"),
        a("VLAN", href => "/vlan"),
        a("Host", href => "/host"),
        a("Zone", href => "/zone"),
        a("User interface", href => "/user"),
        a("Save", href => "/save"),
        a("Restore", href => "/restore"),
        a("Class browser", href => "/browse"),
        a("Edit", href => "/edit"),
        a("Recent Changes", href => "/changes")
      }
    },
    do(?body)
  }
}
end;
           format(?stream, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n");
           format(?stream, "%=", page);
         end; }
end;

define class <buddha-form-warning> (<condition>)
  constant slot error-string :: <string>, required-init-keyword: warning:;
end;

define class <buddha-form-error> (<error>)
  constant slot error-string :: <string>, required-init-keyword: error:;
end;

define method respond-to-get (page == #"changes",
                              request :: <request>,
                              response :: <response>,
                              #key errors = #())
  let out = output-stream(response);
  let action = get-query-value("do");
  let errors = errors;
  if (action)
    block(return)
      let change = get-object(get-query-value("change"));
      if (action = "undo")
        undo(change)
      elseif (action = "redo")
        redo(change)
      end
    exception (e :: <buddha-form-error>)
      errors := add!(errors, e);
      return();
    end;
  end;
  with-buddha-template(out, "Recent Changes")
    collect(show-errors(errors));
    collect(with-xml()
      ul {
        do(for (change in *changes*)
             collect(with-xml()
                       li {
                         do(print-xml(change)),
                         text(" "),
                         a("Undo",
                           href => concatenate("/changes?do=undo&change=",
                                               get-reference(change))) /* ,
                         text(" "),
                         a("Redo",
                           href => concatenate("/changes?do=redo&change=",
                                               get-reference(change))) */
                       }
                     end)
           end)
      }
    end);
  end;
end;

define method respond-to-get
    (page == #"edit",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  let obj-string = get-query-value("obj");
  let obj = get-object(obj-string);
  unless (obj)
    obj := *config*;
  end;
  with-buddha-template(out, "Edit")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                do(edit-form(obj)),
                do(list-forms(obj))
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"browse",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  let obj-string = get-query-value("obj-id");
  let obj = get-object(obj-string);
  unless (obj)
    obj := *config*;
  end;
  with-buddha-template(out, "Browse")
    with-xml()
      div(id => "content") {
        do(browse-list(obj))
      }
    end;
  end;
end;

define method show-errors (errors)
  with-xml()
    div(id => "error")
    {
      do(if (errors & (errors.size > 0))
           with-xml()
             ul
             {
               do(for(error in errors)
                    collect(with-xml()
                              li(error.error-string)
                            end);
                  end)
             }
           end;
         end)
    }
  end;
end;

define method respond-to-get
    (page == #"save",
     request :: <request>,
     response :: <response>,
     #key errors)
  let filename = concatenate("buddha-", integer-to-string(*version*));
  let dood = make(<dood>,
                  locator: concatenate(*directory*, base64-encode(filename)),
                  direction: #"output",
                  if-exists: #"replace");
  dood-root(dood) := make(<buddha>);
  dood-commit(dood);
  dood-close(dood);
  *version* := *version* + 1;
  format(output-stream(response), "Saved %S\n", filename);
  respond-to-get(#"network",
                 request,
                 response);
                              
end;

define method respond-to-get
    (page == #"restore",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "Restore Database")
    collect(show-errors(errors));
    collect(with-xml()
      div(id => "content")
        { form(action => "/restore", \method => "post")
          { \select(name => "filename")
            {
              do(do-directory(method(directory :: <pathname>,
                                     name :: <string>,
                                     type :: <file-type>)
                                  if (type == #"file")
                                    let b64-name = base64-decode(name);
                                    collect(with-xml()
                                              option(b64-name,
                                                     value => b64-name)
                                            end);
                                  end if;
                              end, *directory*))
            },
            input(type => "submit",
                  name => "restore-button",
                  value => "Restore")
          }
        }
            end);
  end;
end;

define method respond-to-post
    (page == #"restore", request :: <request>, response :: <response>)
  let file = get-query-value("filename");
  let b64file = base64-encode(file);
  let dood = make(<dood>,
                  locator: concatenate(*directory*, b64file),
                  direction: #"input");
  let buddha = dood-root(dood);
  dood-close(dood);
  *config* := buddha.config;
  *changes* := buddha.changes;
  if (buddha.version > *version*)
    *version* := buddha.version;
  end;
  format(output-stream(response), "Restored %s\n", file);
  respond-to-get(page,
                 request,
                 response);
end;

define method respond-to-get
    (page == #"network",
     request :: <request>,
     response :: <response>,
     #key errors)
  //TODO: gen dhcp config
  //      remove/edit network forms
  let out = output-stream(response);
  with-buddha-template (out, "Networks")
    collect(show-errors(errors));
    collect(with-xml ()
              div(id => "content")
              {
                do(browse-table(<network>, *config*.networks)),
                do(add-form(<network>,
                            "Networks",
                            *config*.networks,
                            fill-from-request: errors))
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"subnet",
     request :: <request>,
     response :: <response>,
     #key errors)
  //TODO: remove/edit subnet forms
  let out = output-stream(response);
  with-buddha-template(out, "Subnets")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                do(browse-table(<subnet>, *config*.subnets)),
                do(add-form(<subnet>, "Subnets", *config*.subnets, fill-from-request: errors))
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"vlan",
     request :: <request>,
     response :: <response>,
     #key errors)
  //TODO: remove/edit vlan forms
  let out = output-stream(response);
  with-buddha-template(out, "VLAN")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                do(browse-table(<vlan>, *config*.vlans)),
                do(add-form(<vlan>, "Vlans", *config*.vlans, fill-from-request: errors))
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"host",
     request :: <request>,
     response :: <response>,
     #key errors)
  //TODO won't work this way..., needs a context
  let out = output-stream(response);
  with-buddha-template(out, "Hosts")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                do(browse-table(<host>, *config*.hosts)),
                do(add-form(<host>, "Hosts", *config*.hosts, fill-from-request: errors))
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"zone",
     request :: <request>,
     response :: <response>,
     #key errors)
  //TODO: edit/remove forms
  //      strip more infos from table
  //      generate tinydns/bind config file
  let out = output-stream(response);
  with-buddha-template(out, "Zones")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                do(browse-table(<zone>, *config*.zones)),
                do(add-form(<zone>, "Zones", *config*.zones, fill-from-request: errors))
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"user",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "User Interface")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                //do(show-host-info()),
                form(action => "/user", action => "post")
                {
                  div(class => "edit")
                  {
                    text("Hostname"),
                    input(type => "text", name => "hostname"),
                    text("MAC"),
                    input(type => "text", name => "mac"),
                    input(type => "submit",
                          name => "add-host-button",
                          value => "Add Hostname")
                  }
                }
              }
            end);
  end;
end;

define method do-action (action == #"gen-dhcpd", response :: <response>)
 => (show-get? :: <boolean>)
  let network = get-query-value("network");
  network := *config*.networks[string-to-integer(network)];
  set-content-type(response, "text/plain");
  print-isc-dhcpd-file(network, output-stream(response));
  #f; //we don't want the default page!
end;

define function main () => ()
  block()
    start-server();
  exception (e :: <condition>)
    format-out("error: %=\n", e);
  end
end;

define function main2()
  let cisco = make(<cisco-ios-device>,
                   ipv4-address: "23.23.23.23",
                   login-password: "xxx",
                   enable-password: "xxx");

  let control = connect-to-cisco(cisco);
  control.run;

  send-command(control, "terminal length 0");
  let result = send-command(control, "show running");
  format-out("%s\n", result);
end;

begin
  main();
end;
