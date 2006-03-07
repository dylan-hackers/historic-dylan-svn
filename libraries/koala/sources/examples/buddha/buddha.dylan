module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define variable *directory* = "www/buddha/";

define thread variable *user* = #f;

define variable *nameserver* = list(make(<nameserver>,
                                         ns-name: "auth-int.congress.ccc.de"),
                                    make(<nameserver>,
                                         ns-name: "auth-ext.congress.ccc.de"));

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
          do(add-form(<user>, "Users", storage(<user>), refer: "login")),
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


define macro page-definer
  { define page ?:name end }
    => { define responder ?name ## "-responder" ("/" ## ?"name")
           (request, response)
           block(return)
             if (storage(<user>).size = 0)
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
                                errors: list(make(<web-error>,
                                                  error: "No valid user supplied\n")));
                 return();
               end;
             end;
             dynamic-bind(*user* = current-user())
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
  if (storage(<user>).size = 0)
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
    div(id => "buddha-menu") {
      div(id => "buddha-title") { h1 { span(concatenate("Buddha - ", ?title)) } },
      div(id => "buddha-view") {
        ul {
          li { a("Vlans", href => "/vlan") },
          li { a("Zones", href => "/zone") },
          li { a("Hosts", href => "/host") },
          li { a("Networks", href => "/network") },
          li { a("Subnets", href => "/subnet") },
          li { a("Changes", href => "/changes") }
        }
      },
      div (id => "buddha-edit") {
        ul {
          li("Add:"),
          li { a("vlan", href => concatenate("/add?object-type=",
                                             get-reference(<vlan>),
                                             "&parent-object=",
                                             get-reference(storage(<vlan>)))) },
          li { a("zone", href => concatenate("/add?object-type=",
                                             get-reference(<zone>),
                                             "&parent-object=",
                                              get-reference(storage(<zone>)))) },
          li { a("host", href => concatenate("/add?object-type=",
                                             get-reference(<host>),
                                             "&parent-object=",
                                             get-reference(storage(<host>)))) },
          li { a("network", href => concatenate("/add?object-type=",
                                                get-reference(<network>),
                                                "&parent-object=",
                                                get-reference(storage(<network>)))) },
          li { a("subnet", href => concatenate("/add?object-type=",
                                               get-reference(<subnet>),
                                               "&parent-object=",
                                               get-reference(storage(<subnet>)))) }
        },
        ul { li{ text("Logged in as "),
                 strong(*user*.username) } }
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

define method respond-to-get (page == #"admin",
                              request :: <request>,
                              response :: <response>,
                              #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "Admin")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              { h2("Welcome, stranger"),
                ul {
                  li(concatenate("Database version is ", show(version()))),
                  li(concatenate("There were ", show(size(storage(<change>))), " changes")),
                  li(concatenate("There are ", show(size(storage(<user>))), " users")),
                  li{ a("User stats", href => "/koala/user-agents") }
                },
                ul {
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
    errors := add!(errors, make(<web-error>, error: "Permission denied"));
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
    form(action => "/network", \method => "post") {
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
                do(add-form(real-type,
                            #f,
                            parent-object,
                            xml: if (real-type = <network>)
                                   with-xml()
                                     div { text("generate reverse zone"),
                                           input(type => "checkbox",
                                                 name => "reverse-dns?",
                                                 value => "reverse-dns?")
                                          }
                                   end
                                 else
                                   #f
                                 end,
                            fill-from-request: errors))
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
    exception (e :: <web-error>)
      errors := add!(errors, e);
      return();
    end;
  end;
  let count = get-query-value("count");
  if (count & (count ~= ""))
    if (count = "all")
      count := size(storage(<change>))
    else
      count := integer-to-string(count)
    end
  else
    count := 30;
  end;
  with-buddha-template(out, concatenate("Recent Changes - Last ", integer-to-string(count), " Changes"))
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content") {
                a(concatenate("View all ", integer-to-string(size(storage(<change>))), " changes"),
                  href => "/changes?count=all"),
                ul {
                  do(for (change in storage(<change>),
                          i from 0 to count)
                       block(ret)
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
                       exception (e :: <error>)
                         collect(with-xml()
                                   li("error parsing change, sorry")
                                 end);
                         ret()
                       end
                     end)
                  }
                }
            end);
  end;
end;

define constant $colors = #("color1", "color2");
define constant color-table = make(<string-table>);

define method reset-color (object :: <object>)
  color-table[get-reference(object)] := 0;
end;

define method next-color (object :: <object>)
 => (color :: <string>)
  let ref = get-reference(object);
  let result = element(color-table, ref, default: 0);
  color-table[ref] := modulo(result + 1, $colors.size);
  $colors[result];
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
                              li(error.error-string,
                                 class => if(instance?(error, <web-form-warning>))
                                            "green"
                                          else
                                            "red"
                                          end)
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
  dump-data();
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
  restore(file);
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
    network := storage(<network>);
  end;
  set-content-type(response, "text/plain");
  print-isc-dhcpd-file(network, output-stream(response));
end;

define method respond-to-get
    (page == #"tinydns",
     request :: <request>,
     response :: <response>,
     #key errors)
  let zone = get-object(get-query-value("zone"));
  unless (zone)
    zone := storage(<zone>);
  end;
  set-content-type(response, "text/plain");
  print-tinydns-zone-file(zone, output-stream(response));
end;


define method respond-to-post
    (page,
     request :: <request>,
     response :: <response>)
  respond-to-get(page, request, response);
end;

define method respond-to-get
    (page == #"network",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  reset-color(storage(<network>));
  with-buddha-template (out, "Networks")
    collect(show-errors(errors));
    collect(with-xml ()
              div(id => "content")
              {
                table {
                  tr { th("CIDR"), th("dhcp?"), th("VLAN"), th },
                  do(let res = make(<stretchy-vector>);
                     do(method(x)
                            res := add!(res, with-xml()
                                               tr(class => next-color(storage(<network>)))
                                                 { td { a(show(x.cidr),
                                                           href => concatenate("/network-detail?network=",
                                                                               get-reference(x))) },
                                                   td(show(x.dhcp?)),
                                                   td,
                                                   td { do(if(x.dhcp?)
                                                            with-xml()
                                                              a("dhcpd.conf",
                                                              href => concatenate("/dhcp?network=",
                                                                                  get-reference(x)))
                                                            end
                                                          end) }
                                                     }
                                             end);
                            reset-color(storage(<subnet>));
                            res := concatenate(res,
                                               map(method(y)
                                                       with-xml()
                                                         tr(class => concatenate("foo-", next-color(storage(<subnet>)))) { 
                                                           td { a(show(y.cidr),
                                                                  href => concatenate("/subnet-detail?subnet=",
                                                                                      get-reference(y))) },
                                                           td(show(y.dhcp?)),
                                                           td { a(show(y.vlan.number),
                                                                  href => concatenate("/vlan-detail?vlan=",
                                                                                      get-reference(y.vlan))) },
                                                             td }
                                                       end;
                                                   end,
                                                   choose(method(z)
                                                              z.network = x
                                                          end, storage(<subnet>))));
                        end, storage(<network>));
                     res)
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
                do(edit-form(dnetwork,
                             refer: "network-detail",
                             xml: with-xml()
                                    input(type => "hidden",
                                          name => "network",
                                          value => get-reference(dnetwork))
                                  end)),
                do(remove-form(dnetwork, storage(<network>), url: "network")),
                //dhcp options add|edit|remove
                h2(concatenate("DHCP options for subnet ", show(dnetwork))),
                do(if (dnetwork.dhcp-options.size > 0)
                     with-xml()
                       ul { do(map(method(x)
                                       with-xml()
                                         li { text(x), do(remove-form(x, dnetwork.dhcp-options,
                                                                      url: "network-detail",
                                                                      xml: with-xml()
                                                                             input(type => "hidden",
                                                                                   name => "network",
                                                                                   value => get-reference(dnetwork))
                                                                           end)) }
                                          end
                                      end, dnetwork.dhcp-options)) }
                     end;
                   end),
                do(add-form(<string>, "dhcp options", dnetwork.dhcp-options,
                            refer: "network-detail",
                            xml: with-xml()
                                   input(type => "hidden",
                                         name => "network",
                                         value => get-reference(dnetwork))
                                 end)),
                //add subnet with filled-in network?!
                h2(concatenate("Subnets in network ", show(dnetwork))),
                table { tr { th("CIDR"), th("dhcp?") },
                        do(reset-color(storage(<subnet>));
                           map(method(x) with-xml()
                                           tr(class => next-color(storage(<subnet>)))
                                              { td {a(show(x),
                                                      href => concatenate("/subnet-detail?subnet=",
                                                                          get-reference(x))) },
                                                td(show(x.dhcp?)) }
                                         end
                               end, choose(method(y) y.network = dnetwork end, storage(<subnet>)))) }
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
                  do(reset-color(storage(<subnet>));
                     map(method(x) with-xml()
                                     tr(class => next-color(storage(<subnet>)))
                                       { td { a(show(x.cidr),
                                                 href => concatenate("/subnet-detail?subnet=",
                                                                     get-reference(x))) },
                                          td(show(x.dhcp?)),
                                          td { a(show(x.vlan),
                                                 href => concatenate("/vlan-detail?vlan=",
                                                                     get-reference(x.vlan))) }
                                         }
                                   end
                         end, storage(<subnet>)))
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
                do(edit-form(dsubnet,
                             refer: "subnet-detail",
                             xml: with-xml()
                                    input(type => "hidden",
                                          name => "subnet",
                                          value => get-reference(dsubnet))
                                  end)),
                do(remove-form(dsubnet, storage(<subnet>), url: "subnet")),
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
                                               li { text(x),
                                                    do(remove-form(x, dsubnet.dhcp-options,
                                                                   url: "subnet-detail",
                                                                   xml: with-xml()
                                                                          input(type => "hidden",
                                                                                name => "subnet",
                                                                                value => get-reference(dsubnet))
                                                                        end)) }
                                             end
                                   end, dsubnet.dhcp-options)) }
                     end
                   end),
                do(add-form(<string>, "dhcp options", dsubnet.dhcp-options,
                            refer: "subnet-detail",
                            xml: with-xml()
                                   input(type => "hidden",
                                         name => "subnet",
                                         value => get-reference(dsubnet))
                                 end)),
                h2(concatenate("Hosts in subnet ", show(dsubnet))),
                table { tr { th("Hostname"), th("IP"), th("Mac")},
                        do(reset-color(storage(<host>));
                           map(method(x) with-xml()
                                           tr(class => next-color(storage(<host>)))
                                             { td {a(x.host-name,
                                                      href => concatenate("/host-detail?host=",
                                                                          get-reference(x))) },
                                                td(show(x.ipv4-address)),
                                                td(show(x.mac-address)) }
                                         end
                               end, choose(method(y) y.subnet = dsubnet end, storage(<host>)))) }
                //add host with predefined subnet (cause we have the context)?
              }
            end);
  end;
end;

define method insert-br (list :: <collection>) => (res :: <collection>)
  let res = make(<list>);
  do(method(x)
         res := add!(res, x);
         res := add!(res, with-xml() br end);
     end, list);
  //remove last br
  reverse!(tail(res));
end;

define method respond-to-get
    (page == #"vlan",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "Vlans")
    collect(show-errors(errors));
    collect(with-xml()
              div(id => "content")
              {
                table
                {
                  tr { th("ID"), th("Name"), th("Subnets"), th("Description") },
                  do(reset-color(storage(<vlan>));
                     map(method(x) with-xml()
                                     tr(class => next-color(storage(<vlan>)))
                                       { td { a(show(x.number),
                                               href => concatenate("/vlan-detail?vlan=",
                                                                   get-reference(x))) },
                                          td(show(x.vlan-name)),
                                          td { do(insert-br(map(method(y)
                                                                    with-xml()
                                                                      a(show(y.cidr),
                                                                        href => concatenate("/subnet-detail?subnet=",
                                                                                            get-reference(y)))
                                                                    end
                                                                end, choose(method(z) z.vlan = x end,
                                                                            storage(<subnet>)))))
                                             },
                                          td(show(x.description)) }
                                   end
                         end, storage(<vlan>)))
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
                do(edit-form(dvlan,
                             refer: "vlan-detail",
                             xml: with-xml()
                                    input(type => "hidden",
                                          name => "vlan",
                                          value => get-reference(dvlan))
                                  end)),
                do(remove-form(dvlan, storage(<vlan>), url: "vlan")),
                h2(concatenate("Subnets in VLAN ", show(dvlan.number))),
                table {
                  tr { th("CIDR"), th("dhcp?") },
                  do(reset-color(storage(<subnet>));
                     map(method(x) with-xml()
                                     tr (class => next-color(storage(<subnet>)))
                                       { td { a(show(x.cidr),
                                                 href => concatenate("/subnet-detail?subnet=",
                                                                     get-reference(x))) },
                                          td(show(x.dhcp?)) }
                                   end
                         end, choose(method(x) x.vlan = dvlan end, storage(<subnet>))))
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
                  do(reset-color(storage(<host>));
                     map(method(x) with-xml()
                                     tr(class => next-color(storage(<host>)))
                                       { td { a(x.host-name,
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
                         end, storage(<host>)))
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
                do(edit-form(host,
                             refer: "host-detail",
                             xml: with-xml()
                                    input(type => "hidden",
                                          name => "host",
                                          value => get-reference(host))
                                  end)),
                do(remove-form(host, storage(<host>), url: "host")),
                ul { li { text("Subnet "), a(show(host.subnet),
                                             href => concatenate("/subnet-detail?subnet=",
                                                                 get-reference(host.subnet))) },
                     li { text("Zone "), a(show(host.zone),
                                           href => concatenate("/zone-detail?zone=",
                                                               get-reference(host.zone))) }
                },
/*                h2("Add CNAME entry"),
                do(add-form(<cname>, #f, host.zone.cnames,
                            fill-from-request: list(concatenate("source=", host.host-name)),
                            refer: "host-detail",
                            xml: with-xml()
                                   input(type => "hidden",
                                         name => "host",
                                         value => get-reference(host))
                                 end)) */
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
                a("generate complete tinydns.conf", href => "/tinydns"),
                table
                {
                  tr { th("Zone name"), th },
                  do(reset-color(storage(<zone>));
                     map(method(x) with-xml()
                                     tr(class => next-color(storage(<zone>)))
                                       { td { a(x.zone-name,
                                           href => concatenate("/zone-detail?zone=",
                                                               get-reference(x))) },
                                          td { a("tinydns",
                                                 href => concatenate("/tinydns?zone=",
                                                                     get-reference(x))) }
                                      }
                                   end
                         end, storage(<zone>)))
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
                do(edit-form(dzone,
                             refer: "zone-detail",
                             xml: with-xml()
                                    input(type => "hidden",
                                          name => "zone",
                                          value => get-reference(dzone))
                                  end)),
                do(remove-form(dzone, storage(<zone>), url: "zone")),
                //edit|remove ns, mx, cname, forms, add host form?!
                h2("Nameserver entries"),
                do(if (dzone.nameservers.size > 0)
                     with-xml()
                       ul { do(map(method(x) with-xml()
                                               li { text(x.ns-name),
                                                    do(remove-form(x, dzone.nameservers,
                                                                   url: "zone-detail",
                                                                   xml: with-xml()
                                                                          input(type => "hidden",
                                                                                name => "zone",
                                                                                value => get-reference(dzone))
                                                                        end)) }
                                              end
                                   end, dzone.nameservers)) }
                     end
                   end),
                do(add-form(<nameserver>, #f, dzone.nameservers,
                            refer: "zone-detail",
                            xml: with-xml()
                                   input(type => "hidden",
                                         name => "zone",
                                         value => get-reference(dzone))
                                 end)),
                do(unless(dzone.reverse?)
                     with-xml() div {
                h2("Mail exchange entries"),
                do(if (dzone.mail-exchanges.size > 0)
                     with-xml()
                       table { tr { th("Name"), th("Priority"), th("Remove") },
                              do(map(method(x) with-xml()
                                                 tr { td(x.mx-name),
                                                     td(show(x.priority)),
                                                     td { do(remove-form(x, dzone.mail-exchanges,
                                                                         url: "zone-detail",
                                                                         xml: with-xml()
                                                                           input(type => "hidden",
                                                                                 name => "zone",
                                                                                 value => get-reference(dzone))
                                                                         end)) } }
                                               end
                                     end, dzone.mail-exchanges)) }
                     end
                   end),
                do(add-form(<mail-exchange>, #f, dzone.mail-exchanges,
                            refer: "zone-detail",
                            xml: with-xml()
                                   input(type => "hidden",
                                         name => "zone",
                                         value => get-reference(dzone))
                                 end)),
                h2("Cname records"),
                do(if (dzone.cnames.size > 0)
                     with-xml()
                       table { tr { th("Source"), th("Target"), th("Remove") },
                              do(reset-color(dzone.cnames);
                                 map(method(x) with-xml()
                                                 tr(class => next-color(dzone.cnames))
                                                   { td(x.source),
                                                     td(x.target),
                                                     td { do(remove-form(x, dzone.cnames,
                                                                         url: "zone-detail",
                                                                         xml: with-xml()
                                                                           input(type => "hidden",
                                                                                 name => "zone",
                                                                                 value => get-reference(dzone))
                                                                         end)) } }
                                               end
                                     end, dzone.cnames)) }
                     end
                   end),
                do(add-form(<cname>, #f, dzone.cnames,
                            refer: "zone-detail",
                            xml: with-xml()
                                    input(type => "hidden",
                                          name => "zone",
                                          value => get-reference(dzone))
                                  end)),
                h2("A-records"),
                do(if (dzone.a-records.size > 0)
                     with-xml()
                       table { tr { th("Hostname"), th("IP"), th("TTL"), th("Remove") },
                              do(reset-color(dzone.a-records);
                                 map(method(x) with-xml()
                                                 tr(class => next-color(dzone.a-records))
                                                   {
                                                    td(x.host-name),
                                                    td(show(x.ipv4-address)),
                                                    td(show(x.time-to-live)),
                                                    td { do(remove-form(x, dzone.a-records,
                                                                         url: "zone-detail",
                                                                         xml: with-xml()
                                                                           input(type => "hidden",
                                                                                 name => "zone",
                                                                                 value => get-reference(dzone))
                                                                         end)) } }
                                               end
                                     end, dzone.a-records)) }
                     end
                   end),
                do(add-form(<a-record>, #f, dzone.a-records,
                            refer: "zone-detail",
                            xml: with-xml()
                                    input(type => "hidden",
                                          name => "zone",
                                          value => get-reference(dzone))
                                  end)),
                h2("Hosts"),
                table { tr { th("Hostname"), th("IP"), th("TTL") },
                        do(reset-color(storage(<host>));
                           map(method(x) with-xml()
                                           tr(class => next-color(storage(<host>)))
                                             { td { a(x.host-name,
                                                       href => concatenate("/host-detail?host=",
                                                                           get-reference(x))) },
                                                td(show(x.ipv4-address)),
                                                td(show(x.time-to-live)) }
                                         end
                               end, choose(method(y) y.zone = dzone end, storage(<host>)))) } }
                     end
                   end)
              }
            end);
  end;
end;

/*
define constant $yourname-users = make(<string-table>);

define class <yourname-user> (<object>)
  slot password :: <string>, required-init-keyword: password:;
  slot host :: false-or(<host>) = #f;
end;

define method respond-to-post
    (page == #"user",
     request :: <request>,
     response :: <response>)
  let remote-ip = get-remote-address(request);
  let entered-password = get-query-value("password");
  let hostname = get-query-value("hostname");
  let entered-mac-address = get-query-value("mac-address");
  let user = element($yourname-users, remote-ip, default: #f);
  let errs = #();
  format-out("ip %= pass %= host %= mac %= user %=\n",
             remote-ip, entered-password, hostname, entered-mac-address, user);
end;
  block(ret)
    if (user)
      if (user.password = entered-password)
        if (user.host)
          let changes = #();
          if (user.host.mac-address ~= entered-mac-address)
            let triple = make(<triple>,
                              old-value: user.host.mac-address,
                              new-value: entered-mac-address,
                              slot-name: "mac-address");
            changes := add!(changes, triple);
          end;
          if (user.host.host-name ~= hostname)
            let triple = make(<triple>,
                              old-value: user.host.host-name,
                              new-value: hostname,
                              slot-name: "host-name");
            changes := add!(changes, triple);
          end;
          if (changes.size > 0)
            //we have to do something
            let command = make(<edit-command>,
                               arguments: list(user.host, changes));
            redo(command);
            let change = make(<change>,
                              command: command);
            save(change);
            let slot-names = apply(concatenate, map(method(x)
                                                        concatenate(x.slot-name, " to ",
                                                                    show(x.new-value), "  ")
                                                    end, changes));
            signal(make(<web-success>,
                        warning: concatenate("Saved Host ",
                                             show(user.host),
                                             " changed slots: ",
                                             slot-names)));
          end;
        else
          let ip = as(<ip-address>, remote-ip);
          //create new host
          let new-host = make(<host>,
                              host-name: hostname,
                              ipv4-address: ip,
                              time-to-live: 300,
                              mac-address: entered-mac-address,
                              subnet: choose(method(x)
                                                 ip-in-net?(x, ip)
                                             end, storage(<subnet>))[0],
                              zone: choose(method(x)
                                               x.zone-name = "congress.ccc.de";
                                           end, storage(<zone>))[0]);
          //add new host
          let command = make(<add-command>,
                             arguments: list(new-host, storage(<host>)));
          redo(command);
          let change = make(<change>,
                            command: command);
          save(change);
          signal(make(<web-success>,
                      warning: concatenate("Added host: ", show(new-host))));
        end;
      else
        //wrong password
        signal(make(<web-error>,
                    error: "Invalid user/password"));
      end;
      //post before get
      signal(make(<web-error>,
                  error: "POST before GET, go away"));
    end;
  exception (e :: <condition>)
    errs := add!(errs, e);
    ret()
  exception (e :: <web-error>)
    errs := add!(errs, e);
    ret()
  exception (e :: <error>)
    errs := add!(errs, e);
    ret()
  end;
  respond-to-get(page, request, response, errors: errs);
end; */

define method respond-to-get
    (page == #"user",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  let page = with-xml-builder()
html(xmlns => "http://www.w3.org/1999/xhtml") {
  head {
    title("Buddha - Yourname service!"),
    link(rel => "stylesheet", href => "/buddha.css")
  },
  body {
    div(id => "content") {
      h1("Welcome to Buddha"),
      do(collect(show-errors(errors))),
      form(action => "/user", \method => "post")
      {
        div(class => "edit")
        {
          text("Hostname"),
          input(type => "text", name => "hostname"),
          text(".congress.ccc.de"), br,
          text(" MAC-address"),
          input(type => "text", name => "mac-address"), br,
          text("Password"),
          input(type => "password", name => "password"), br,
          input(type => "submit",
                name => "add-host-button",
                value => "Add Hostname")
        }
      }
    }
  }
}
  end;
  format(out, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n");
  format(out, "%=", page);
end;

define function main () => ()
  dumper();
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
