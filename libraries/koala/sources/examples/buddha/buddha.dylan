module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define variable *config* = make(<config>,
                                config-name: "config");

define variable *directory* = "www/buddha/";

define sideways method process-config-element
    (node :: <xml-element>, name == #"buddha")
  let cdir = get-attr(node, #"content-directory");
  if (~cdir)
    log-warning("Buddha - No content-directory specified! - will use ./buddha/");
    cdir := "./buddha/";
  end;
  *directory* := cdir;
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
  respond-to-get(#"net", request, response);
end;

define page net end;
define page vlan end;
define page host end;
define page zone end;
define page user end;
define page save end;
define page restore end;
define page browse end;
define page edit end;

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
        a("Network", href => "/net"),
        a("VLAN", href => "/vlan"),
        a("Host", href => "/host"),
        a("Zone", href => "/zone"),
        a("User interface", href => "/user"),
        a("Save to disk", href => "/save"),
        a("Restore from disk", href => "/restore"),
        a("Class browser", href => "/browse"),
        a("Edit", href => "/edit"),
        a("Shutdown", href => "/koala/shutdown")
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

define constant $obj-table = make(<string-table>);

define class <buddha-form-warning> (<condition>)
  constant slot error-string :: <string>, required-init-keyword: warning:;
end;

define class <buddha-form-error> (<error>)
  constant slot error-string :: <string>, required-init-keyword: error:;
end;

define method respond-to-get
    (page == #"edit",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  let obj-string = get-query-value("obj");
  unless (obj-string)
    obj-string := "";
  end;
  let obj = element($obj-table, obj-string, default: *config*);
  with-buddha-template(out, "Edit")
    with-xml()
      div(id => "content") {
        do(edit-form(obj)),
        do(list-forms(obj))
      }
    end;
  end;
end;

define method respond-to-get
    (page == #"browse",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  let obj-string = get-query-value("obj");
  unless (obj-string)
    obj-string := "";
  end;
  let obj = element($obj-table, obj-string, default: *config*);
  with-buddha-template(out, "Browse")
    with-xml()
      div(id => "content") {
        do(browse(obj))
      }
    end;
  end;
end;

define method get-reference (object :: <object>) => (res :: <string>)
  let address = copy-sequence(format-to-string("%=", address-of(object)),
                              start: 1);
  $obj-table[address] := object;
  address;
end;


define method respond-to-get
    (page == #"save",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "Save Database")
    if (errors & errors.size > 0)
      collect(with-xml()
                div(id => "error")
                { ul
                  {
                    do(for(error in errors)
                         collect(with-xml()
                                   li(error.error-string)
                                 end);
                       end)
                  }
                }
              end);
    end;
    collect(with-xml()
              div(id => "content")
              { form(action => "/save", \method => "post")
                { div(class => "edit")
                  {
                    text("Filename"),
                    input(type => "text", name => "filename"),
                    input(type => "submit",
                          name => "save-button",
                          value => "Save")
                  }
                }
              }
            end);
  end;
end;

define method respond-to-post
    (page == #"save", request :: <request>, response :: <response>)
  let errors = #();
  let file = get-query-value("filename");
  let handler <buddha-form-warning>
    = method(e :: <buddha-form-warning>, next-handler :: <function>)
          errors := add!(errors, e)
      end;
  block(return)
    if (~file | file = "")
      signal(make(<buddha-form-error>,
                  error: "No file specified!"));
    end;
    let dood = make(<dood>,
                    locator: concatenate(*directory*, base64-encode(file)),
                    direction: #"output",
                    if-exists: #"replace");
    dood-root(dood) := *config*;
    dood-commit(dood);
    dood-close(dood);
  exception (e :: <buddha-form-error>)
    errors := add!(errors, e);
    return();
  end;
  respond-to-get(page, request, response, errors: errors);
end;

define method respond-to-get
    (page == #"restore",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "Restore Database")
    with-xml()
      div(id => "content")
        { form(action => "/restore", \method => "post")
          { \select(name => "filename")
            {
              do(do-directory(method(directory :: <pathname>,
                                     name :: <string>,
                                     type :: <file-type>)
                                  if (type == #"file")
                                    collect(with-xml()
                                              option(base64-decode(name),
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
    end;
  end;
end;

define method respond-to-post
    (page == #"restore", request :: <request>, response :: <response>)
  let file = get-query-value("filename");
  let dood = make(<dood>,
                  locator: concatenate(*directory*, file),
                  direction: #"input");
  *config* := dood-root(dood);
  dood-close(dood);
  format(output-stream(response), "Restored database\n");
  respond-to-get(page, request, response);
end;

define method respond-to-get
    (page == #"net",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template (out, "Networks")
    with-xml ()
      div(id => "content")
      {
        do(let res = #();
           for (net in *config*.networks,
                i from 0)
             res := concatenate(gen-xml(net), res);
             res := add!(res, with-xml()
                                form(action => "/net", \method => "post")
                                {
                                  div(class => "edit")
                                  {
                                    input(type => "hidden",
                                          name => "action",
                                          value => "gen-dhcpd"),
                                    input(type => "hidden",
                                          name => "network",
                                          value => integer-to-string(i)),
                                    input(type => "submit",
                                          name => "gen-dhcpd-button",
                                          value => "generate dhcpd.conf")
                                  }
                                }
                              end);
             res := add!(res, with-xml()
                                form(action => "net", \method => "post")
                                {
                                  div(class => "edit")
                                  {
                                    input(type => "hidden",
                                          name => "action",
                                          value => "remove-network"),
                                    input(type => "hidden",
                                          name => "network",
                                          value => integer-to-string(i)),
                                    input(type => "submit",
                                          name => "remove-network-button",
                                          value => "Remove this Network")
                                  }
                                }
                              end);
             res := add!(res, with-xml()
                                form(action => "/net", \method => "post")
                                {
                                  div(class => "edit")
                                  {
                                    text("CIDR"),
                                    input(type => "text", name => "cidr"),
                                    \select(name => "vlan")
                                    {
                                      do(let res = make(<list>);
                                         for (ele in *config*.vlans,
                                              i from 0)
                                           res := add!(res, with-xml()
                                                              option(as(<string>, ele),
                                                                     value => integer-to-string(i))
                                                            end);
                                         end;
                                         reverse(res))
                                    },
                                    text("DHCP?"),
                                    input(type => "checkbox",
                                          value => "dhcp",
                                          checked => "checked",
                                          name => "dhcp"),
                                    text("DHCP start"),
                                    input(type => "text", name => "dhcp-start"),
                                    text("DHCP end"),
                                    input(type => "text", name => "dhcp-end"),
                                    text("DHCP router"),
                                    input(type => "text", name => "dhcp-router"),
                                    text("Default lease time"),
                                    input(type => "text",
                                          name => "default-lease-time"),
                                    text("Maximum lease time"),
                                    input(type => "text", name => "max-lease-time"),
                                    text("DHCP options"),
                                    input(type => "text", name => "options"),
                                    input(type => "hidden",
                                          name => "action",
                                          value => "add-subnet"),
                                    input(type => "hidden",
                                          name => "network",
                                          value => integer-to-string(i)),
                                    input(type => "submit",
                                          name => "add-subnet-button",
                                          value => concatenate
                                            ("Add subnet to ",
                                             as(<string>, net.cidr)))
                                  }
                                }
                              end);
           end;
           reverse(res)),
        form(action => "/net", \method => "post")
        {
          div(class => "edit")
          {
            text("CIDR"),
            input(type => "text", name => "cidr"),
            text("DHCP?"),
            input(type => "checkbox",
                  value => "dhcp",
                  checked => "checked",
                  name => "dhcp"),
            text("Default lease time"),
            input(type => "text",
                  name => "default-lease-time"),
            text("Maximum lease time"),
            input(type => "text", name => "max-lease-time"),
            text("DHCP options"),
            input(type => "text", name => "options"),
            input(type => "hidden",
                  name => "action",
                  value => "add-network"),
            input(type => "submit",
                  name => "add-network-button",
                  value => "Add network")
          }
        }
      }
    end;
  end;
end;

define method respond-to-get
    (page == #"vlan",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "VLAN")
    with-xml()
      div(id => "content")
      {
        do(let res = make(<list>);
           do(method(x)
                  res := concatenate(gen-xml(x), res);
                  res := add!(res,
                              with-xml()
                                form(action => "/vlan", \method => "post")
                                {
                                  div(class => "edit")
                                  {
                                    input(type => "hidden",
                                          name => "action",
                                          value => "remove-vlan"),
                                    input(type => "hidden",
                                          name => "vlan",
                                          value => integer-to-string(x.number)),
                                    input(type => "submit",
                                          name => "remove-vlan-button",
                                          value => "Remove VLAN")
                                  }
                                }
                              end);
              end, *config*.vlans);
           reverse(res)),
        form(action => "/vlan", \method => "post")
        {
          div(class => "edit")
          {
            input(type => "hidden",
                  name => "action",
                  value => "add-vlan"),
            text("VLAN Number"),
            input(type => "text",
                  name => "vlan"),
            text("Name"),
            input(type => "text",
                  name => "name"),
            text("Description"),
            input(type => "text",
                  name => "description"),
            input(type => "submit",
                  name => "add-vlan-button",
                  value => "Add VLAN")
          }
        }
      }
    end;
  end;
end;

define method respond-to-get
    (page == #"host",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "Hosts")
    with-xml()
      div(id => "content")
      {
        table
        {
        tr { th("Name"), th("IP"), th("Net"), th("Mac"), th("Zone") },
        do(for (net in *config*.networks)
             for (subnet in net.subnets)
               do(method(x)
                      collect(gen-xml(x));
                  end, subnet.hosts);
             end;
           end)
        },
        form(action => "/host", \method => "post")
        {
          div(class => "edit")
          {
            text("Name"),
            input(type => "text", name => "name"),
            text("IP"),
            input(type => "text", name => "ip"),
            text("MAC"),
            input(type => "text", name => "mac"),
            \select(name => "zone"),
/*            {
              do(do(method(x)
                        collect(with-xml()
                                  option(x.zone-name, value => x.zone-name)
                                end);
                    end, choose(method(x)
                                    ~ reverse?(x);
                                end, *config*.zones))) 
            }, */
            input(type => "submit",
                  name => "add-host-button",
                  value => "Add Host")
          }
        }
      }
    end;
  end;
end;

define method respond-to-get
    (page == #"zone",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "Zones")
    with-xml()
      div(id => "content")
      {
        table
        {
          tr { th("Name") },
          do(do(method(x)
                    collect(gen-xml(x));
                end, *config*.zones))
        },
        form(action => "/zone", \method => "post")
        {
          div(class => "edit")
          {
            text("Name"),
            input(type => "text", name => "name"),
            text("Hostmaster"),
            input(type => "text", name => "hostmaster"),
            text("Serial"),
            input(type => "text", name => "serial"),
            text("Refresh"),
            input(type => "text", name => "refresh"),
            text("Retry"),
            input(type => "text", name => "retry"),
            text("Expire"),
            input(type => "text", name => "expire"),
            text("Minimum"),
            input(type => "text", name => "minimum"),
            text("Time to live"),
            input(type => "text", name => "time-to-live"),
            text("Nameserver"),
            input(type => "text", name => "nameserver"),
            text("Mail exchange"),
            input(type => "text", name => "mail-exchange"),
            text("txt"),
            input(type => "text", name => "txt"),
            input(type => "submit",
                  name => "add-zone-button",
                  value => "Add Zone")
          }
        }
      }
    end;
  end;
end;

define method respond-to-get
    (page == #"user",
     request :: <request>,
     response :: <response>,
     #key errors)
  let out = output-stream(response);
  with-buddha-template(out, "User Interface")
    with-xml()
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
    end;
  end;
end;

/*
define method respond-to-post
    (page == #"user", request :: <request>, response :: <response>)
  let ip = host-address(remote-host(request-socket(request)));
  let name = get-query-value("name");
  let mac = get-query-value("mac");
  let zone = *config*.config-zones[0];
  let network = find-network(*config*, ip);
  let host = make(<host>,
                  name: name,
                  ip: ip,
                  net: find-network(network, ip),
                  mac: parse-mac(mac),
                  zone: zone);
  add-host(network, host);
  respond-to-get(page, request, response);
end;
*/

define method respond-to-post
    (page == #"net", request :: <request>, response :: <response>)
  let action = get-query-value("action");
  if (do-action(as(<symbol>, action), response))
      respond-to-get(page, request, response);
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

define method do-action (action == #"add-subnet", response :: <response>)
 => (show-get? :: <boolean>)
  let network = get-query-value("network");
  let cidr = get-query-value("cidr");
  let vlan = string-to-integer(get-query-value("vlan"));
  let dhcp? = if (get-query-value("dhcp") = "dhcp") #t else #f end;
  let default-lease-time
    = string-to-integer(get-query-value("default-lease-time"));
  let max-lease-time
    = string-to-integer(get-query-value("max-lease-time"));
  let options = parse-options(get-query-value("options"));
  let dhcp-start = as(<ip-address>, get-query-value("dhcp-start"));
  let dhcp-end = as(<ip-address>, get-query-value("dhcp-end"));
  let dhcp-router = as(<ip-address>, get-query-value("dhcp-router"));
  let subnet = make(<subnet>,
                    cidr: make(<cidr>, network-address: cidr),
                    vlan: *config*.vlans[vlan],
                    dhcp?: dhcp?,
                    dhcp-default-lease-time: default-lease-time,
                    dhcp-max-lease-time: max-lease-time,
                    dhcp-options: options,
                    dhcp-start: dhcp-start,
                    dhcp-end: dhcp-end,
                    dhcp-router: dhcp-router);
  add-subnet(*config*.networks[string-to-integer(network)], subnet);
  #t;
end;

define method do-action (action == #"add-network", response :: <response>)
 => (show-get? :: <boolean>)
  let cidr = get-query-value("cidr");
  let dhcp? = if (get-query-value("dhcp") = "dhcp") #t else #f end;
  let default-lease-time
    = string-to-integer(get-query-value("default-lease-time"));
  let max-lease-time
    = string-to-integer(get-query-value("max-lease-time"));
  let options = parse-options(get-query-value("options"));
  let network = make(<network>,
                     cidr: make(<cidr>, network-address: cidr),
                     dhcp?: dhcp?,
                     dhcp-max-lease-time: max-lease-time,
                     dhcp-default-lease-time: default-lease-time,
                     dhcp-options: options);
  add-net(*config*, network);
  #t;
end;

define method do-action (action == #"remove-network", response :: <response>)
 => (show-get? :: <boolean>)
  let network = get-query-value("network");
  remove-net(*config*, *config*.networks[string-to-integer(network)]);
  #t;
end;

define method parse-options (options) => (list :: <list>)
  format-out("OPTIONS %=\n", options);
  #();
end;

define method respond-to-post
    (page == #"vlan", request :: <request>, response :: <response>)
  let action = get-query-value("action");
  if (do-action(as(<symbol>, action), response))
    respond-to-get(page, request, response);
  end;
end;

define method do-action (action == #"add-vlan", response :: <response>)
 => (show-get? :: <boolean>)
  let number = string-to-integer(get-query-value("vlan"));
  let name = get-query-value("name");
  let description = get-query-value("description");
  let vlan = make(<vlan>,
                  number: number,
                  vlan-name: name,
                  description: description);
  add-vlan(*config*, vlan);
  #t;
end;

define method do-action (action == #"remove-vlan", response :: <response>)
 => (show-get? :: <boolean>)
  let vlan = string-to-integer(get-query-value("vlan"));
  remove-vlan(*config*, choose(method(x)
                                   x.number = vlan
                               end, *config*.vlans)[0]);
  #t;
end;

define method respond-to-post
    (page == #"zone", request :: <request>, response :: <response>)
  let name = get-query-value("name");
  let hostmaster = get-query-value("hostmaster");
  let serial = string-to-integer(get-query-value("serial"));
  let refresh = string-to-integer(get-query-value("refresh"));
  let retry = string-to-integer(get-query-value("retry"));
  let expire = string-to-integer(get-query-value("expire"));
  let minimum = string-to-integer(get-query-value("minimum"));
  let time-to-live = string-to-integer(get-query-value("time-to-live"));
  let nameserver = get-query-value("nameserver");
  let mail-exchange = get-query-value("mail-exchange");
  let txt = get-query-value("txt");
  let zone = make(<zone>,
                  zone-name: name,
                  hostmaster: hostmaster,
                  serial: serial,
                  refresh: refresh,
                  retry: retry,
                  expire: expire,
                  minimum: minimum,
                  time-to-live: time-to-live,
                  nameserver: list(nameserver),
                  mail-exchange: list(mail-exchange),
                  txt: list(txt));
  *config*.zones := sort!(add!(*config*.zones, zone));
  respond-to-get(page, request, response);
end;

define method respond-to-post
    (page == #"host", request :: <request>, response :: <response>)
  let name = get-query-value("name");
  let ip = make(<ip-address>, data: get-query-value("ip"));
  let mac = get-query-value("mac");
  let zone = get-query-value("zone");
  let network = find-network(find-network(*config*, ip), ip);
  let host = make(<host>,
                  host-name: name,
                  ip: ip,
                  net: network,
                  mac: parse-mac(mac),
                  zone: find-zone(*config*, zone));
  add-host(network, host);
  respond-to-get(page, request, response);
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
                   ip: "23.23.23.23",
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

define method main3()
let foo =
with-xml-builder()
  html {
    head {
      title("foo")
    },
    body {
      div(id => "foobar",
          class => "narf") {
        a("here", href => "http://www.foo.com"),
        a(href => "http://www.ccc.de/"),
        text("foobar"),
        ul {
          li("foo"),
          br,
          li("bar"),
          br
        }
      }
    }
  }
end;
  format-out("%=\n", foo);
end;
