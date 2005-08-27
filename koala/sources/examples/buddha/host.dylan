module: buddha

define class <host> (<object>)
  slot host-name :: <string>, required-init-keyword: name:;
  slot host-ipv4-address :: <ip-address>, init-keyword: ip:;
  slot host-net :: <subnet>, init-keyword: net:;
  slot host-mac :: <mac-address>, init-keyword: mac:;
  slot host-zone :: <zone>, init-keyword: zone:;
end;

/*
define method make (host == <host>,
                    #next next-method,
                    #rest rest,
                    #key ip,
                    mac,
                    #all-keys) => (res :: <host>)
  let args = rest;
  apply(next-method, host, ip: ip, mac: mac, args);
end method;
*/

define method print-object (host :: <host>, stream :: <stream>)
 => ()
  format(stream, "Host %s Zone %s Mac %s\n",
         host.host-name,
         host.host-zone.zone-name,
         mac-to-string(host.host-mac));
  format(stream, "IP %s Net %s\n",
         ip-address-to-string(host.host-ipv4-address),
         cidr-to-string(host.host-net.network-cidr));
end;

define method \< (a :: <host>, b :: <host>) => (res :: <boolean>)
  a.host-ipv4-address < b.host-ipv4-address
end;

define method print-html (host :: <host>, stream :: <stream>)
  gen-row(stream,
          list(host.host-name,
               ip-address-to-string(host.host-ipv4-address),
               cidr-to-string(host.host-net.network-cidr),
               mac-to-string(host.host-mac),
               host.host-zone.zone-name));
end;

define method print-isc-dhcpd-file (host :: <host>, stream :: <stream>)
 => ()
  format(stream, "host %s {\n", host.host-name);
  format(stream, "\thardware ethernet %s;\n", mac-to-string(host.host-mac));
  format(stream, "\tfixed-address %s;\n",
         ip-address-to-string(host.host-ipv4-address));
  format(stream, "}\n\n");
end;

