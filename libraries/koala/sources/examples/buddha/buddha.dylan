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
  constant slot users = *users*;
end;

define variable *directory* = "www/buddha/";

define thread variable *user* = #f;

define variable *nameserver* = list(make(<nameserver>,
                                         ns-name: "auth-int.congress.ccc.de"),
                                    make(<nameserver>,
                                         ns-name: "auth-ext.congress.ccc.de"));

define sideways method process-config-element
    (node :: <xml-element>, name == #"buddha")
  let cdir = get-attr(node, #"content-directory");
  if (~cdir)
    log-warning("Buddha - No content-directory specified!");
  else
    *directory* := cdir;
  end;
  log-info("Buddha content directory = %s", *directory*);
  restore-newest-database();
end;

define method split-file (file :: <string>) => (version :: <integer>)
  let elements = split(file, '-');
  if (elements.size = 2)
    string-to-integer(elements[1]);
  else
    0
  end;
end;

define method restore-newest-database () => ();
  let file = "";
  let latest-version = 0;
  do-directory(method(directory :: <pathname>,
                      name :: <string>,
                      type :: <file-type>)
                   if (type == #"file")
                     if (split-file(name) > latest-version)
                       file := name;
                     end;
                   end;
               end, *directory*);
  restore-database(file);
end;

define method restore-database (file :: <string>)
  let dood = make(<dood>,
                  locator: concatenate(*directory*, file),
                  direction: #"input");
  format-out("restored %= %=\n", *directory*, file);
  let buddha = dood-root(dood);
  dood-close(dood);
  *config* := buddha.config;
  *changes* := buddha.changes;
  *users* := buddha.users;
  if (buddha.version > *version*)
    *version* := buddha.version + 1;
  end;
  do(method(x) if (x.dns-only?) x.subnet := $bottom-subnet end end,
     *config*.hosts);
end;

define method initial-responder (request :: <request>, response :: <response>)
  dynamic-bind(*user* = make(<user>,
                             username: "admin",
                             password: "foo",
                             email: "buddhaadmin@local",
                             admin: #t))
    block(return)
      if (request.request-method = #"post")
        respond-to-post(#"edit", request, response);
        return();
      end;
      let stream = output-stream(response);
      let page = with-xml-builder()
html(xmlns => "http://www.w3.org/1999/xhtml") {
  head {
    title("Buddha - Please create initial user!"),
    link(rel => "stylesheet", href => "/buddha.css")
  },
  body {
        h1("Welcome to buddha, please create an initial admin-user!"),
        div(id => "content") { 
          do(add-form(<user>, "Users", *users*, refer: "login")),
          b("Please set the admin flag!")
        }
  }
}
end;
      format(stream, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n");
      format(stream, "%=", page);
    end;
  end;
end;

define generic respond-to-get (page,
                               request :: <request>,
                               response :: <response>,
                               #key errors);

define macro page-definer
  { define page ?:name end }
    => { define responder ?name ## "-responder" ("/" ## ?"name")
           (request, response)
           block(return)
             if (*users*.size = 0)
                 initial-responder(request, response);
                 return();
             end;
             //dns, dhcp shouldn't need a valid user
             //(to get it working with wget and stuff, without needing cookies)
             unless (logged-in(request))
               login(request);
               unless (logged-in(request))
                 //error
                 respond-to-get(#"login", request, response,
                                errors: list(make(<buddha-form-error>,
                                                  error: "No valid user supplied\n")));
                 return();
               end;
             end;
             dynamic-bind(*user* = *users*[logged-in(request)])
               if (request.request-method = #"get")
                 respond-to-get(as(<symbol>, ?"name"), request, response)
               elseif (request.request-method = #"post")
                 respond-to-post(as(<symbol>, ?"name"), request, response)
               end;
             end;
           end;
         end; }
end;

define responder default-responder ("/")
  (request, response)
  if (*users*.size = 0)
    initial-responder(request, response);
  else
    respond-to-get(#"login", request, response);
  end;
end;

/*
define responder dood-responder ("/dood")
  (request, response)
  let dood = make(<dood>,
                  locator: concatenate(*directory*, base64-encode(filename)),
                  direction: #"output",
                  if-exists: #"replace");
  dood-root(dood) := make(<buddha>);
  dood-commit(dood);
  dood-close(dood);
  
end;
*/

define page network end;
define page network-detail end;
define page subnet end;
define page subnet-detail end;
define page vlan end;
define page vlan-detail end;
define page host end;
define page host-detail end;
define page zone end;
define page zone-detail end;
define page user end;
define page save end;
define page restore end;
define page browse end;
define page edit end;
define page changes end;
define page adduser end;
define page logout end;
define page add end;
define page admin end;

define responder dhcp-responder ("/dhcp")
    (request, response)
    respond-to-get(#"dhcp", request, response);
end;

define responder tinydns-responder ("/tinydns")
    (request, response)
    respond-to-get(#"tinydns", request, response);
end;

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
        a("Vlans", href => "/vlan"),
        a("Zones", href => "/zone"),
        a("Hosts", href => "/host"),
        a("Networks", href => "/network"),
        a("Subnets", href => "/subnet"),
        a("Changes", href => "/changes"),
        br, br, text("Add"),
        a("vlan", href => concatenate("/add?object-type=",
                                      get-reference(<vlan>),
                                      "&parent-object=",
                                      get-reference(*config*.vlans))),
        a("zone", href => concatenate("/add?object-type=",
                                      get-reference(<zone>),
                                      "&parent-object=",
                                      get-reference(*config*.zones))),
        a("host", href => concatenate("/add?object-type=",
                                      get-reference(<host>),
                                      "&parent-object=",
                                      get-reference(*config*.hosts))),
        a("network", href => concatenate("/add?object-type=",
                                         get-reference(<network>),
                                         "&parent-object=",
                                         get-reference(*config*.networks))),
        a("subnet", href => concatenate("/add?object-type=",
                                        get-reference(<subnet>),
                                        "&parent-object=",
                                        get-reference(*config*.subnets)))
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

define class <buddha-success> (<buddha-form-warning>)
end;

define class <buddha-form-error> (<error>)
  constant slot error-string :: <string>, required-init-keyword: error:;
end;

define method respond-to-get (page == #"admin",
                              request :: <request>,
                              response :: <response>,
                              #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "Admin")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              { ul {
                  li { a("User management", href => "/adduser") },
                  li { a("Save database", href => "/save") },
                  li { a("Restore database", href => "/restore") },
                  li { a("Generic browser", href => "/browse") },
                  li { a("Generic editor", href => "/edit") }
                }
              }
            end);
  end;
end;

define method respond-to-get (page == #"adduser",
                              request :: <request>,
                              response :: <response>,
                              #key errors = #())
  let username = logged-in(request);
  let user = element(*users*, username, default: #f);
  if (user & user.admin?)
    let out = output-stream(response);
    with-buddha-template(out, "User management")
      collect(show-errors(errors));
      collect(with-xml()
                div(id => "content")
                  {
                   do(browse-table(<user>, *users*)),
                   do(add-form(<user>, "Users", *users*, fill-from-request: errors, refer: "adduser"))
                     }
              end)
    end;
  else
    errors := add!(errors, make(<buddha-form-error>, error: "Permission denied"));
    respond-to-get (#"network", request, response, errors: errors);
  end;
end;

define method respond-to-get (page == #"logout",
                              request :: <request>,
                              response :: <response>,
                              #key errors = #());
  clear-session(request);
  respond-to-get(#"login", request, response);
end;

define method respond-to-get (page == #"login",
                              request :: <request>,
                              response :: <response>,
                              #key errors = #())
  let out = output-stream(response);
  format(out, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n");
  let page = with-xml-builder()
html(xmlns => "http://www.w3.org/1999/xhtml") {
  head {
    title("Buddha - Login"),
    link(rel => "stylesheet", href => "/buddha.css")
  },
  body {
    do(show-errors(errors)),
    h1("Please login to buddha"),
    form(action => "/network", \method => "get") {
      div(class => "edit") {
        text("Username "),
        input(type => "text",
              name => "username"),
        br,
        text("Password "),
        input(type => "password",
              name => "password"),
        br,
        input(type => "submit",
              value => "Login")
      }
    }
  }
}
  end;
  format(out, "%=", page);
end;

                    
define method respond-to-get (page == #"add",
                              request :: <request>,
                              response :: <response>,
                              #key errors = #())
  let real-type = get-object(get-query-value("object-type"));
  let parent-object = get-object(get-query-value("parent-object"));
  let out = output-stream(response);
  with-buddha-template(out, concatenate("Add ", get-url-from-type(real-type)))
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                h1(concatenate("Add ", get-url-from-type(real-type))),
                do(add-form(real-type, #f, parent-object, fill-from-request: errors))
              }
            end);
  end;
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
                  locator: concatenate(*directory*, filename),
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
                                    collect(with-xml()
                                              option(name,
                                                     value => name)
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
  restore-database(file);
  format(output-stream(response), "Restored %s\n", file);
  respond-to-get(page,
                 request,
                 response);
end;

define method respond-to-get
    (page == #"dhcp",
     request :: <request>,
     response :: <response>,
     #key errors)
  let network = get-object(get-query-value("network"));
  unless (network)
    network := *config*;
  end;
  set-content-type(response, "text/plain");
  print-isc-dhcpd-file(network, output-stream(response));
end;

define method respond-to-get
    (page == #"tinydns",
     request :: <request>,
     response :: <response>,
     #key errors)
  set-content-type(response, "text/plain");
  print-tinydns-zone-file(*config*, output-stream(response));
end;


define method respond-to-get
    (page == #"network",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template (out, "Networks")
    collect(show-errors(errors));
    collect(with-xml ()
              div(id => "content")
              {
                table {
                  tr { th("CIDR"), th("dhcp?"), th("dhcp.conf") },
                  do(map(method(x) with-xml()
                                     tr { td { a(show(x.cidr),
                                                 href => concatenate("/network-detail?network=",
                                                                     get-reference(x))) },
                                          td(show(x.dhcp?)),
                                          td { a("dhcpd.conf",
                                                 href => concatenate("/dhcp?network=",
                                                                     get-reference(x))) }
                                         }
                                   end
                         end, *config*.networks))
/*                                         map(method(y) with-xml()
                                     tr { td { a(show(y.cidr),
                                                 href => concatenate("/subnet-detail?subnet=",
                                                                     get-reference(y))) },
                                          td(show(y.dhcp?)),
                                          td("&nbsp;") }
                                                       end;
                                             end, choose(method(z) z.network = x end, *config*.subnets)))
                         end, *config*.networks))) */
                }
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"network-detail",
     request :: <request>,
     response :: <response>,
     #key errors)
  let dnetwork = get-object(get-query-value("network"));
  let out = output-stream(response);
  with-buddha-template(out, concatenate("Network ", show(dnetwork), " detail"))
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                h1(concatenate("Network ", show(dnetwork))),
                do(edit-form(dnetwork)),
                do(remove-form(dnetwork, *config*.networks)),
                //dhcp options add|edit|remove
                h2(concatenate("DHCP options for subnet ", show(dnetwork))),
                do(if (dnetwork.dhcp-options.size > 0)
                     with-xml()
                       ul { do(map(method(x)
                                       with-xml()
                                         li { text(x), do(remove-form(x, dnetwork, url: "network-detail")) }
                                          end
                                      end, dnetwork.dhcp-options)) }
                     end;
                   end),
                do(add-form(<string>, "dhcp options", dnetwork.dhcp-options)),
                //add subnet with filled-in network?!
                h2(concatenate("Subnets in network ", show(dnetwork))),
                table { tr { th("CIDR"), th("dhcp?") },
                        do(map(method(x) with-xml()
                                           tr { td {a(show(x),
                                                      href => concatenate("/subnet-detail?subnet=",
                                                                          get-reference(x))) },
                                                td(show(x.dhcp?)) }
                                         end
                               end, choose(method(y) y.network = dnetwork end, *config*.subnets))) }
              }
            end);
  end;
end;


define method respond-to-get
    (page == #"subnet",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "Subnets")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                table {
                  tr { th("CIDR"), th("dhcp?"), th("VLAN") },
                  do(map(method(x) with-xml()
                                     tr { td { a(show(x.cidr),
                                                 href => concatenate("/subnet-detail?subnet=",
                                                                     get-reference(x))) },
                                          td(show(x.dhcp?)),
                                          td { a(show(x.vlan),
                                                 href => concatenate("/vlan-detail?vlan=",
                                                                     get-reference(x.vlan))) }
                                         }
                                   end
                         end, *config*.subnets))
                }
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"subnet-detail",
     request :: <request>,
     response :: <response>,
     #key errors)
  let dsubnet = get-object(get-query-value("subnet"));
  let out = output-stream(response);
  with-buddha-template(out, concatenate("Subnet ", show(dsubnet), " detail"))
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                h1(concatenate("Subnet ", show(dsubnet))),
                do(edit-form(dsubnet)),
                do(remove-form(dsubnet, *config*.subnets)),
                ul { li { text("VLAN "), a(show(dsubnet.vlan),
                                          href => concatenate("/vlan-detail?vlan=",
                                                              get-reference(dsubnet.vlan))) },
                     li { text("Network "), a(show(dsubnet.network),
                                              href => concatenate("/network-detail?network=",
                                                                  get-reference(dsubnet.network))) }
                },
                //dhcp options edit|remove
                h2(concatenate("DHCP options for subnet ", show(dsubnet))),
                do(if (dsubnet.dhcp-options.size > 0)
                     with-xml()
                       ul { do(map(method(x) with-xml()
                                               li(x)
                                             end
                                   end, dsubnet.dhcp-options)) }
                     end
                   end),
                do(add-form(<string>, "dhcp options", dsubnet.dhcp-options)),
                h2(concatenate("Hosts in subnet ", show(dsubnet))),
                table { tr { th("Hostname"), th("IP"), th("Mac")},
                        do(map(method(x) with-xml()
                                           tr { td {a(x.host-name,
                                                      href => concatenate("/host-detail?host=",
                                                                          get-reference(x))) },
                                                td(show(x.ipv4-address)),
                                                td(show(x.mac-address)) }
                                         end
                               end, choose(method(y) y.subnet = dsubnet end, *config*.hosts))) }
                //add host with predefined subnet (cause we have the context)?
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"vlan",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "VLAN")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                table
                {
                  tr { th("ID"), th("Name"), th("Description") },
                  do(map(method(x) with-xml()
                                     tr { td { a(show(x.number),
                                               href => concatenate("/vlan-detail?vlan=",
                                                                   get-reference(x))) },
                                          td(show(x.vlan-name)),
                                          td(show(x.description)) }
                                   end
                         end, *config*.vlans))
                }
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"vlan-detail",
     request :: <request>,
     response :: <response>,
     #key errors)
  let dvlan = get-object(get-query-value("vlan"));
  let out = output-stream(response);
  with-buddha-template(out, concatenate("VLAN ", show(dvlan.number), " detail"))
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                h1(concatenate("VLAN ", show(dvlan.number), ", Name ", dvlan.vlan-name)),
                do(edit-form(dvlan)),
                do(remove-form(dvlan, *config*.vlans)),
                h2(concatenate("Subnets in VLAN ", show(dvlan.number))),
                table {
                  tr { th("CIDR"), th("dhcp?") },
                  do(map(method(x) with-xml()
                                     tr { td { a(show(x.cidr),
                                                 href => concatenate("/subnet-detail?subnet=",
                                                                     get-reference(x))) },
                                          td(show(x.dhcp?)) }
                                   end
                         end, choose(method(x) x.vlan = dvlan end, *config*.subnets)))
                }
                //add subnet with pre-filled vlan?
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"host",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "Hosts")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                table
                {
                  tr { th("Hostname"), th("IP-Address"), th("Subnet"), th("Zone") },
                  do(map(method(x) with-xml()
                                     tr { td { a(x.host-name,
                                                 href => concatenate("/host-detail?host=",
                                                                     get-reference(x))) },
                                          td (show(x.ipv4-address)),
                                          td { a(show(x.subnet),
                                                 href => concatenate("/subnet-detail?subnet=",
                                                                     get-reference(x.subnet))) },
                                          td { a(show(x.zone),
                                                 href => concatenate("/zone-detail?zone=",
                                                                     get-reference(x.zone))) }
                                     }
                                   end
                         end, *config*.hosts))
                }
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"host-detail",
     request :: <request>,
     response :: <response>,
     #key errors)
  let host = get-object(get-query-value("host"));
  let out = output-stream(response);
  with-buddha-template(out, concatenate("Host ", host.host-name, " detail"))
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                h1(concatenate("Host ", host.host-name, " ", show(host.ipv4-address))),
                do(edit-form(host)),
                do(remove-form(host, *config*.hosts)),
                ul { li { text("Subnet "), a(show(host.subnet),
                                             href => concatenate("/subnet-detail?subnet=",
                                                                 get-reference(host.subnet))) },
                     li { text("Zone "), a(show(host.zone),
                                           href => concatenate("/zone-detail?zone=",
                                                               get-reference(host.zone))) }
                }
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"zone",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "Zones")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                a("generate tinydns.conf", href => "/tinydns"),
                table
                {
                  tr { th("Zone name") },
                  do(map(method(x) with-xml()
                                     tr { td { a(x.zone-name,
                                           href => concatenate("/zone-detail?zone=",
                                                               get-reference(x))) } }
                                   end
                         end, *config*.zones))
                }
              }
            end);
  end;
end;

define method respond-to-get
    (page == #"zone-detail",
     request :: <request>,
     response :: <response>,
     #key errors)
  let dzone = get-object(get-query-value("zone"));
  let out = output-stream(response);
  with-buddha-template(out, concatenate("Zone ", dzone.zone-name, " detail"))
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                h1(concatenate("Zone ", dzone.zone-name)),
                do(edit-form(dzone)),
                do(remove-form(dzone, *config*.zones)),
                //edit|remove ns, mx, cname, forms, add host form?!
                h2("Nameserver entries"),
                do(if (dzone.nameservers.size > 0)
                     with-xml()
                       ul { do(map(method(x) with-xml()
                                               li(x.ns-name)
                                             end
                                   end, dzone.nameservers)) }
                     end
                   end),
                do(add-form(<nameserver>, #f, dzone.nameservers)),
                h2("Mail exchange entries"),
                do(if (dzone.mail-exchanges.size > 0)
                     with-xml()
                       table { tr { th("Name"), th("Priority") },
                              do(map(method(x) with-xml()
                                                 tr { td(x.mx-name),
                                                     td(show(x.priority)) }
                                               end
                                     end, dzone.mail-exchanges)) }
                     end
                   end),
                do(add-form(<mail-exchange>, #f, dzone.mail-exchanges)),
                h2("Cname records"),
                do(if (dzone.cnames.size > 0)
                     with-xml()
                       table { tr { th("Source"), th("Target") },
                              do(map(method(x) with-xml()
                                                 tr { td(x.source),
                                                     td(x.target) }
                                               end
                                     end, dzone.cnames)) }
                     end
                   end),
                do(add-form(<cname>, #f, dzone.cnames)),
                h2("Hosts"),
                table { tr { th("Hostname"), th("TTL") },
                        do(map(method(x) with-xml()
                                           tr { td { a(x.host-name,
                                                       href => concatenate("/host-detail?host=",
                                                                           get-reference(x))) },
                                                td(show(x.time-to-live)) }
                                         end
                               end, choose(method(y) y.zone = dzone end, *config*.hosts))) }
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

define function main () => ()
  let dumper
  = make(<thread>,
         function: method()
                       sleep(23);
                       while(#t)
                         let filename
                           = concatenate("buddha-", integer-to-string(*version*));
                         let dood
                           = make(<dood>,
                                  locator: concatenate(*directory*,
                                                       filename),
                                  direction: #"output",
                                  if-exists: #"replace");
                         dood-root(dood) := make(<buddha>);
                         dood-commit(dood);
                         dood-close(dood);
                         *version* := *version* + 1;
                         sleep(300);
                       end;
                   end);
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
