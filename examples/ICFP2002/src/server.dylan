module: icfp2002server
synopsis: test server

define function debug(#rest args)
  apply(format, *standard-error*, args);
  force-output(*standard-error*);
end function debug;

// most of this is waiting on other bits deciding their interfaces
// command args: <port> <map> <num-clients>
define function main(name, arguments)

  init-server(arguments[1],
	      string-to-integer(arguments[2]));
  await-clients(string-to-integer(arguments[0]),
		string-to-integer(arguments[2]));
  while(~contest-over?())
    process-turn();
  end;

  exit-application(0);
end function main;

main(application-name(), application-arguments());

define variable *board* = #f;
define variable *clients* = #f;

define class <client> (<object>)
  slot name :: <string>, init-keyword: name:;
  slot in-stream, init-keyword: in:;
  slot out-stream, init-keyword: out:;
end class;

// setup the board for the server, also opens the server port
define function init-server(file :: <string>,
			    num-clients :: <integer>) => ()

  *clients* := make(<vector>, size: vector(num-clients), fill: #f);
  *board* := load-board(file);
end function;

// wait for clients to connect
define function await-clients(port, num-clients) => ()
  for(num from 0 below num-clients)
    // TODO, this should be -tcp-server-connection now i think about it
    let (i-s, o-s)
      = tcp-client-connection("localhost", port);
    let n = read-line(i-s, on-end-of-stream: "Unknown");
    *clients*[num] := make(<client>, name: n, in-stream: i-s, out-stream: o-s);

    //   send board
    //   send config
  end;
  for(num from 0 below num-clients)
    // send initial response to client
  end;
end function;


define function contest-over?() => (b :: <boolean>);
  // have we finished yet?
end function;

define function process-turn() => ();
  // foreach client
  //   issue current location packages
  // foreach client
  //   get command
  // work out bids
  // execute commands
  // foreach client
  //  tell it what happended
end function;
