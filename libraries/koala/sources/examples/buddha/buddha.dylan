module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define macro page-definer
  { define page ?:name end }
    => { define responder ?name ## "-responder" ("/" ## ?"name")
           (request, response)
           if (request.request-method = #"get")
             respond-to-get(as(<symbol>, ?"name"), request, response)
           elseif (request.request-method = #"post")
             respond-to-post(as(<symbol>, ?"name"), request, response);
           end;
         end; }
end;

define page net end;
define page vlan end;
define page host end;
define page zone end;
define page user end;
//define page buddha.css end;

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
               with-div (?stream, "header")
                 with-div(?stream, "navbar")
                   gen-link(?stream, "/net", "Network");
                   gen-link(?stream, "/vlan", "VLAN");
                   gen-link(?stream, "/host", "Host");
                   gen-link(?stream, "/zone", "Zone");
                   gen-link(?stream, "/user", "User Interface");
                   gen-link(?stream, "/koala/shutdown", "Shutdown");
                 end;
               end;
               ?body
             end;
           end;
         end; }
end;

define method respond-to-get
    (page == #"net", request :: <request>, response :: <response>)
  let out = output-stream(response);
  with-buddha-template (out, "Networks")
    with-div (out, "content")
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
                                        cidr-to-string(net.network-cidr)));
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
    with-div(out, "content")
      do(method(x)
             print-html(x, out)
         end, get-sorted-list(*config*.config-vlans));
    end;
    with-div(out, "edit")
      with-form(out, "/vlan")
        form-field(out, "vlan");
        form-field(out, "name");
        form-field(out, "description");
        submit-form-field(out, "add-vlan-button", "Add VLAN");
      end;
    end;
  end;
end;

define method respond-to-get
    (page == #"host", request :: <request>, response :: <response>)
  let out = output-stream(response);
  with-buddha-template(out, "Hosts")
    with-div(out, "content")
      with-table (out, #("Name", "IP", "Net", "Mac", "Zone"))
        for (net in *config*.config-nets)
          do(method(x)
                 print-html(x, out);
             end, net.network-hosts);
        end;
      end;
    end;
    with-div(out, "edit")
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
end;

define method respond-to-get
    (page == #"zone", request :: <request>, response :: <response>)
  let out = output-stream(response);
  with-buddha-template(out, "Zones")
    with-div(out, "content")
      with-table (out, #("Name"))
        do(method(x)
               print-html(x, out);
           end, *config*.config-zones);
      end;
    end;
    with-div(out, "edit")
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
end;

define method respond-to-get
    (page == #"user", request :: <request>, response :: <response>)
  let out = output-stream(response);
  with-buddha-template(out, "User Interface")
    with-div(out, "content")
      show-host-info();
    end;
    with-div(out, "edit")
      with-form(out, "/user")
        form-field(out, "hostname");
        form-field(out, "mac");
        submit-form-field(out, "add-host-button", "Add Hostname");
      end;
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
  if (do-action(as(<symbol>, action), response))
    respond-to-get(page, request, response);
  end;
end;

define method do-action (action == #"gen-dhcpd", response :: <response>)
 => (show-get? :: <boolean>)
  //generate dhcpd.conf
  let network = get-query-value("network");
  network := *config*.config-nets[string-to-integer(network)];
  set-content-type(response, "text/plain");
  print-isc-dhcpd-file(network, output-stream(response));
  #f; //we don't want the default page!
end;

define method do-action (action == #"add-subnet", response :: <response>)
 => (show-get? :: <boolean>)
  //add-subnet
  let network = get-query-value("network");
  format-out("ADD SUBNET\n");
  let cidr = get-query-value("cidr");
  let vlan = string-to-integer(get-query-value("vlan"));
  let dhcp? = if (get-query-value("dhcp") = "dhcp") #t else #f end;
  format-out("FOO %= %= %=\n", cidr, vlan, dhcp?);
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
  #t;
end;

define method do-action (action == #"add-network", response :: <response>)
 => (show-get? :: <boolean>)
  //add network
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
  let number = string-to-integer(get-query-value("number"));
  let name = get-query-value("name");
  let description = get-query-value("description");
  let vlan = make(<vlan>,
                  number: number,
                  name: name,
                  description: description);
  add-vlan(*config*, vlan);
  respond-to-get(page, request, response);
end;

define method respond-to-post
    (page == #"host", request :: <request>, response :: <response>)
  let name = get-query-value("name");
  let ip = make(<ip-address>,
                ip: string-to-ip-address(get-query-value("ip")));
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
  respond-to-get(page, request, response);
end;

define variable *config* = #f;

define function main () => ()
  let vlans = list(make(<vlan>,
                        number: 0,
                        name: "default",
                        description: "default vlan"),
                   make(<vlan>,
                        number: 23,
                        name: "management",
                        description: "management vlan"));
  format-out("VLANS %=\n", vlans);
  let zone = make(<zone>,
                  name: "foo.bar.com");
  format-out("before make <config>\n");
  force-output(*standard-output*);
  *config* := make(<config>,
                   name: "foobar!",
                   vlans: vlans,
                   zones: list(zone));
  format-out("after make <config>\n");
  let net = make(<network>, cidr: "10.0.0.0/16");
  add-subnet(net,
             make(<subnet>,
                  cidr: "10.0.0.0/24",
                  vlan: 0));
  add-subnet(net,
             make(<subnet>,
                  cidr: "10.0.1.0/24",
                  vlan: 0));
  add-subnet(net,
             make(<subnet>,
                  cidr: "10.0.2.0/24",
                  vlan: 23));
  add-subnet(net,
             make(<subnet>,
                  cidr: "10.0.4.0/23",
                  vlan: 0));
  add-subnet(net,
             make(<subnet>,
                  cidr: "10.0.6.0/24",
                  vlan: 23));
  add-subnet(net,
             make(<subnet>,
                  cidr: "10.0.7.0/25",
                  vlan: 23));
  add-net(*config*, net);
  format-out("NETOWRK %=\n", find-network(*config*,
                                          make(<ip-address>,
                                               ip: "10.0.0.2")));
  let net2 = make(<network>, cidr: "23.23.0.0/16");
  add-subnet(net2,
             make(<subnet>,
                  cidr: "23.23.0.0/24",
                  vlan: 0));
  add-subnet(net2,
             make(<subnet>,
                  cidr: "23.23.1.0/24",
                  vlan: 23));
  add-subnet(net2,
             make(<subnet>,
                  cidr: "23.23.2.0/24",
                  vlan: 0));
  add-subnet(net2,
             make(<subnet>,
                  cidr: "23.23.4.0/24",
                  vlan: 23));
  add-net(*config*, net2);

  let foo = make(<ip-address>, ip: "23.23.23.23") + 66000;
  foo := make(<ip-address>, ip: "192.168.2.254") + 10;
  foo := make(<ip-address>, ip: "192.168.255.254") + 10;
  foo := make(<ip-address>, ip: "192.168.0.1") - 1;
  foo := make(<ip-address>, ip: "192.168.0.1") - 2;
  foo := make(<ip-address>, ip: "23.24.24.231") - 66000;

/*  let dood = make(<dood>,
                  locator: *config*.config-name,
                  direction: #"output",
                  if-exists: #"replace");
  dood-root(dood) := *config*;
  dood-commit(dood);
  dood-close(dood);

  let dood2 = make(<dood>,
                   locator: *config*.config-name,
                   direction: #"input");
  let data = dood-root(dood2);
  dood-close(dood2);


  format-out("DOOD says %=\n", data);
  format-out("%= %=\n", string-to-integer("23foo23422foo"));
  let bar = "10.0.0.2";
  format-out("bar %=\n", split(bar, '.'));
  let ip-bar = string-to-ip-address(bar);
  format-out("stoip %=\n", ip-bar);
  format-out("iptos %=\n", ip-address-to-string(ip-bar));
  format-out("strtonet %=\n", string-to-netmask("255.255.255.252"));*/
  //for (i from 0 to 32)
  //  format-out("netmask %d %=\n", i, netmask-to-vector(i));
  //end;

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
