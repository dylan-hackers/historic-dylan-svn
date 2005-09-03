module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define variable *config* = make(<config>,
                                name: "config",
                                vlans: make(<table>));

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

define macro page-definer
  { define page ?:name end }
    => { define responder ?name ## "-responder" ("/" ## ?"name")
           (request, response)
           if (request.request-method = #"get")
             respond-to-get(as(<symbol>, ?"name"), request, response)
           elseif (request.request-method = #"post")
             if (respond-to-post(as(<symbol>, ?"name"), request, response))
               respond-to-get(as(<symbol>, ?"name"), request, response)
             end;
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

define macro with-buddha-template
  { with-buddha-template(?stream:variable, ?title:expression)
      ?body:body
    end }
    => { begin
           with-html(?stream)
             with-header(?stream, concatenate("Buddha - ", ?title))
               gen-stylesheet(?stream, "/buddha.css");
             end;
             with-body(?stream)
               with-div (?stream, "id", "header")
                 with-div(?stream, "id", "navbar")
                   gen-link(?stream, "/net", "Network");
                   gen-link(?stream, "/vlan", "VLAN");
                   gen-link(?stream, "/host", "Host");
                   gen-link(?stream, "/zone", "Zone");
                   gen-link(?stream, "/user", "User Interface");
                   gen-link(?stream, "/save", "Save to disk");
                   gen-link(?stream, "/restore", "Restore from disk");
                   gen-link(?stream, "/koala/shutdown", "Shutdown");
                 end;
               end;
               ?body
             end;
           end;
         end; }
end;

define method respond-to-get
    (page == #"save", request :: <request>, response :: <response>)
  let out = output-stream(response);
  with-buddha-template(out, "Save Database")
    with-div (out, "id", "content")
      with-form (out, "/save")
        form-field(out, "filename");
        submit-form-field(out, "save-button", "Save");
      end;
    end;
  end;
end;

define method respond-to-post
    (page == #"save", request :: <request>, response :: <response>)
  let file = get-query-value("filename");
  let dood = make(<dood>,
                  locator: concatenate(*directory*, base64-encode(file)),
                  direction: #"output",
                  if-exists: #"replace");
  dood-root(dood) := *config*;
  dood-commit(dood);
  dood-close(dood);
  format(output-stream(response), "Saved database\n");
  #t;
end;

define method respond-to-get
    (page == #"restore", request :: <request>, response :: <response>)
  let out = output-stream(response);
  with-buddha-template(out, "Restore Database")
    with-div (out, "id", "content")
      with-form(out, "/restore")
        with-select(out, "filename")
          do-directory(method(directory :: <pathname>,
                              name :: <string>,
                              type :: <file-type>)
                           if (type == #"file")
                             gen-option(out, name, base64-decode(name));
                           end if;
                       end, *directory*);
        end;
        submit-form-field(out, "restore-button", "Restore");
      end;
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
  #t;
end;

define method respond-to-get
    (page == #"net", request :: <request>, response :: <response>)
  let out = output-stream(response);
  with-buddha-template (out, "Networks")
    with-div (out, "id", "content")
      for (net in *config*.config-nets,
           i from 0)
        print-html(net, out);
        with-form (out, "/net")
          hidden-form-field(out, "action", "gen-dhcpd");
          hidden-form-field(out, "network", integer-to-string(i));
          submit-form-field(out, "dhcpd.conf", "generate dhcpd.conf");
        end;
        with-form (out, "/net")
          hidden-form-field(out, "action", "remove-network");
          hidden-form-field(out, "network", integer-to-string(i));
          submit-form-field(out,
                            "remove-network-button",
                            "Remove this Network");
        end;
        with-form (out, "/net")
          form-field(out, "cidr");
          with-select(out, "vlan")
            do(method(x)
                   let num = integer-to-string(x.vlan-number);
                   gen-option(out, num, concatenate(num, " ", x.vlan-name));
               end, get-sorted-list(*config*.config-vlans));
          end;
          form-field(out, "dhcp",
                     type: "checkbox",
                     value: "dhcp",
                     checked: #t);
          form-field(out, "dhcp-start");
          form-field(out, "dhcp-end");
          form-field(out, "dhcp-router");
          form-field(out, "default-lease-time");
          form-field(out, "max-lease-time");
          form-field(out, "options");
          hidden-form-field(out, "action", "add-subnet");
          hidden-form-field(out, "network", integer-to-string(i));
          submit-form-field(out,
                            "add-subnet-button",
                            concatenate("Add Subnet to ",
                                        as(<string>, net.network-cidr)));
        end;
      end;
      with-form (out, "/net")
        form-field(out, "cidr");
        form-field(out, "dhcp", type: "checkbox", value: "dhcp", checked: #t);
        form-field(out, "default-lease-time", value: "600");
        form-field(out, "max-lease-time", value: "7200");
        form-field(out, "options");
        hidden-form-field(out, "action", "add-network");
        submit-form-field(out, "add-network-button", "Add Network");
      end;
    end;
  end;
end;

define method respond-to-get
    (page == #"vlan", request :: <request>, response :: <response>)
  let out = output-stream(response);
  with-buddha-template(out, "VLAN")
    with-div(out, "id", "content")
      do(method(x)
             print-html(x, out);
             with-form(out, "/vlan")
               hidden-form-field(out, "action", "remove-vlan");
               hidden-form-field(out, "vlan", integer-to-string(x.vlan-number));
               submit-form-field(out, "remove-vlan-button", "Remove VLAN");
             end;
         end, get-sorted-list(*config*.config-vlans));
    end;
    with-form(out, "/vlan")
      hidden-form-field(out, "action", "add-vlan");
      form-field(out, "vlan");
      form-field(out, "name");
      form-field(out, "description");
      submit-form-field(out, "add-vlan-button", "Add VLAN");
    end;
  end;
end;

define method respond-to-get
    (page == #"host", request :: <request>, response :: <response>)
  let out = output-stream(response);
  with-buddha-template(out, "Hosts")
    with-div(out, "id", "content")
      with-table (out, #("Name", "IP", "Net", "Mac", "Zone"))
        for (net in *config*.config-nets)
          do(method(x)
                 print-html(x, out);
             end, net.network-hosts);
        end;
      end;
    end;
    with-form(out, "/host")
      form-field(out, "name");
      form-field(out, "ip");
      form-field(out, "mac");
      with-select(out, "zone")
        do(method(x)
               gen-option(out, x.zone-name, x.zone-name);
           end, choose(method(x)
                           ~ zone-reverse?(x);
                       end, *config*.config-zones));
      end;
      submit-form-field(out, "add-host-button", "Add Host");
    end;
  end;
end;

define method respond-to-get
    (page == #"zone", request :: <request>, response :: <response>)
  let out = output-stream(response);
  with-buddha-template(out, "Zones")
    with-div(out, "id", "content")
      with-table (out, #("Name"))
        do(method(x)
               print-html(x, out);
           end, *config*.config-zones);
      end;
    end;
    with-form(out, "/zone")
      form-field(out, "name");
      form-field(out, "hostmaster");
      form-field(out, "serial");
      form-field(out, "refresh");
      form-field(out, "retry");
      form-field(out, "expire");
      form-field(out, "minimum");
      form-field(out, "time-to-live");
      form-field(out, "nameserver");
      form-field(out, "mail-exchange");
      form-field(out, "txt");
      submit-form-field(out, "add-zone-button", "Add Zone");
    end;
  end;
end;

define method respond-to-get
    (page == #"user", request :: <request>, response :: <response>)
  let out = output-stream(response);
  with-buddha-template(out, "User Interface")
    with-div(out, "id", "content")
      show-host-info();
    end;
    with-form(out, "/user")
      form-field(out, "hostname");
      form-field(out, "mac");
      submit-form-field(out, "add-host-button", "Add Hostname");
    end;
  end;
end;
      
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

define method respond-to-post
    (page == #"net", request :: <request>, response :: <response>)
  let action = get-query-value("action");
  do-action(as(<symbol>, action), response)
end;

define method do-action (action == #"gen-dhcpd", response :: <response>)
 => (show-get? :: <boolean>)
  let network = get-query-value("network");
  network := *config*.config-nets[string-to-integer(network)];
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
  if (dhcp?)
    let default-lease-time
      = string-to-integer(get-query-value("default-lease-time"));
    let max-lease-time
      = string-to-integer(get-query-value("max-lease-time"));
    format-out("DHCP %= %=\n", default-lease-time, max-lease-time);
    let options = parse-options(get-query-value("options"));
    let dhcp-start = parse-ip(get-query-value("dhcp-start"));
    let dhcp-end = parse-ip(get-query-value("dhcp-end"));
    let dhcp-router = parse-ip(get-query-value("dhcp-router"));
    let subnet = make(<subnet>,
                      cidr: cidr,
                      vlan: vlan,
                      dhcp?: dhcp?,
                      default-lease-time: default-lease-time,
                      max-lease-time: max-lease-time,
                      options: options,
                      dhcp-start: dhcp-start,
                      dhcp-end: dhcp-end,
                      dhcp-router: dhcp-router);
    add-subnet(*config*.config-nets[string-to-integer(network)], subnet);
  else
    let subnet = make(<subnet>,
                      cidr: cidr,
                      vlan: vlan,
                      dhcp?: dhcp?);
    add-subnet(*config*.config-nets[string-to-integer(network)], subnet);
  end;
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
                     cidr: cidr,
                     dhcp?: dhcp?,
                     max-lease-time: max-lease-time,
                     default-lease-time: default-lease-time,
                     options: options);
  add-net(*config*, network);
  #t;
end;

define method do-action (action == #"remove-network", response :: <response>)
 => (show-get? :: <boolean>)
  let network = get-query-value("network");
  remove-net(*config*, *config*.config-nets[string-to-integer(network)]);
  #t;
end;

define method parse-options (options) => (list :: <list>)
  format-out("OPTIONS %=\n", options);
  #();
end;

define method respond-to-post
    (page == #"vlan", request :: <request>, response :: <response>)
  let action = get-query-value("action");
  do-action(as(<symbol>, action), response);
end;

define method do-action (action == #"add-vlan", response :: <response>)
 => (show-get? :: <boolean>)
  let number = string-to-integer(get-query-value("vlan"));
  let name = get-query-value("name");
  let description = get-query-value("description");
  let vlan = make(<vlan>,
                  number: number,
                  name: name,
                  description: description);
  add-vlan(*config*, vlan);
  #t;
end;

define method do-action (action == #"remove-vlan", response :: <response>)
 => (show-get? :: <boolean>)
  let vlan = string-to-integer(get-query-value("vlan"));
  remove-vlan(*config*, vlan);
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
                  name: name,
                  hostmaster: hostmaster,
                  serial: serial,
                  refresh: refresh,
                  retry: retry,
                  expire: expire,
                  minimum: minimum,
                  time-to-live: time-to-live,
                  nameserver: list(nameserver),
                  mail-exchange: list(mail-exchange),
                  txt: txt);
  *config*.config-zones :=
    sort!(add!(*config*.config-zones, zone));
  #t;
end;

define method respond-to-post
    (page == #"host", request :: <request>, response :: <response>)
  let name = get-query-value("name");
  let ip = make(<ip-address>, ip: get-query-value("ip"));
  let mac = get-query-value("mac");
  let zone = get-query-value("zone");
  let network = find-network(*config*, ip);
  let host = make(<host>,
                  name: name,
                  ip: ip,
                  net: find-network(network, ip),
                  mac: parse-mac(mac),
                  zone: find-zone(*config*, zone));
  add-host(network, host);
  #t;
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
                   name: "router",
                   ip: make(<ip-address>, ip: "23.23.23.23"),
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
