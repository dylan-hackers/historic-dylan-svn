Module:    proxy-sockets
Synopsis:  Support for sockets through a proxy server
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See license.txt

// An attempt at defining a defining a proxy server abstract class.
// proxy-server-host and proxy-server-port should point to the 
// location of the proxy server. 
define abstract open class <proxy-server> (<object>)
  slot proxy-server-host :: false-or(type-union(<internet-address>, <string>));
  slot proxy-server-port :: false-or(<integer>) = #f;
end class <proxy-server>;

// Implementations should implement the process-server-connect generic 
// function to handle connecting to the proxy server.
define open generic proxy-server-connect(proxy :: <proxy-server>, socket :: <socket>, host, port);

// Used by make-proxy-socket to be the default proxy server used
// when creating sockets. Can be bound using dynamic-bind(...) to
// bind a proxy server for a series of calls.
define thread variable *default-proxy-server* :: false-or(<proxy-server>) = #f;

define method initialize(server :: <proxy-server>,
                         #key host: requested-host :: false-or(type-union(<internet-address>, <string>)) = #f, 
                              port: requested-port :: false-or(<integer>) = #f) => ()
  server.proxy-server-port := requested-port;
  server.proxy-server-host :=
    select (requested-host by instance?)
      <internet-address> => requested-host;
      <string> => make(<internet-address>, name: requested-host);
    end select;
end method initialize;

// An implementation of <proxy-server> for servers that use the CONNECT method
// of tunnelling sockets through a proxy. Usually this is only for sockets on
// port 80 or 443. Currently does not handle authentication as I don't have
// a proxy requiring authentication to test it against.
define open class <generic-proxy-server> (<proxy-server>)
end class <generic-proxy-server>;

define method  proxy-server-connect(proxy :: <proxy-server>, socket :: <socket>, host, port)
  write-line( socket, format-to-string("CONNECT %s:%d HTTP/1.0", host.host-name, port));
  write-line( socket, "");
  force-output(socket);
  read-line(socket, on-end-of-stream: #f); // authentication here?
  for(line = read-line(socket, on-end-of-stream: #f) 
    then read-line(socket, on-end-of-stream: #f),
      until: line.size = 0)
    end for;
end method proxy-server-connect;

// make-proxy-socket returns a socket that can be used through a firewall.
// It expects the proxy-server: key to contain a subtype of <proxy-server>
// and it defaults to *default-proxy-server*. If #f then the call will fall
// back to using the networks library to create the socket. This allows
// make-proxy-socket to be used to create either proxy or non-proxy sockets
// depending on the existance of the proxy-server key.
define method make-proxy-socket( 
    #rest initargs,
	#key host,
	     port,
	     proxy-server = *default-proxy-server*)
  => (socket :: <socket>)
	if(proxy-server)
	  let s = make(<socket>, 
	               protocol: #"tcp", 
	               host: proxy-server.proxy-server-host, 
                   port: proxy-server.proxy-server-port);
	  // host
  	  let host =
		select (host by instance?)
			<internet-address> => host;
			<string> => make(<internet-address>, name: host);
		end select;
	  proxy-server-connect(proxy-server, s, host, port);
	  apply(make, <socket>, 
	            descriptor: s.socket-descriptor, 
	            initargs);
	else
	  apply(make, <socket>, initargs);
    end if;
end method make-proxy-socket;
	     
// Example of usage:
//
// dynamic-bind(*default-proxy-server* = make(<generic-proxy-server>, 
//                                            host: "proxy.server.com", 
//                                            port: 8080))
//
//   let new-socket = make-proxy-socket(host: "www.double.co.nz", port: 80);
//   block()
//     write-line(new-socket, "GET / HTTP/1.0");
//     write-line(new-socket, "");
//     force-output(new-socket);
//     for(line = read-line(new-socket, on-end-of-stream: #f) 
//         then read-line(new-socket, on-end-of-stream: #f),
//         until: ~line)
//       format-out("%s\n", line)
//     end for;
//   cleanup
//     close(new-socket);
//   end block;    
// end dynamic-bind;
//
// Usage with SSL (requires double-ssl-sockets library):
//
// dynamic-bind(*default-proxy-server* = make(<generic-proxy-server>, 
//                                            host: "proxy.server.com", 
//                                            port: 8080))
//
//   let new-socket = make-proxy-socket(host: "www.elliottwave.com", 
//                                      port: 443, protocol: #"ssl-tcp");
//   block()
//     write-line(new-socket, "GET / HTTP/1.0");
//     write-line(new-socket, "");
//     force-output(new-socket);
//     for(line = read-line(new-socket, on-end-of-stream: #f) 
//         then read-line(new-socket, on-end-of-stream: #f),
//         until: ~line)
//       format-out("%s\n", line)
//     end for;
//   cleanup
//     close(new-socket);
//   end block;    
// end dynamic-bind;

	

