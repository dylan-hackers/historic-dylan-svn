module: server
synopsis: test server

define function debug(#rest args)
  apply(format, *standard-error*, args);
  force-output(*standard-error*);
end function debug;

// most of this is waiting on other bits deciding their interfaces
// NOTE for functionality outside this file I've just put an obvious function name;
// will need to be changed to match the actual interface.
// command args: <port> <map> <num-clients>
define function main(name, arguments)
  let pt = string-to-integer(arguments[0]);
  let cli = string-to-integer(arguments[2]);
  
  init-server(arguments[1], cli);
  await-clients(pt, cli);

  while(~contest-over?())
    process-turn(cli);
  end;

  for(i from 0 below cli)
    close(*clients*.in-stream);
    close(*clients*.out-stream);
  end;

  exit-application(0);
end function main;


define variable *board* = #f;
define variable *clients* = #f;
define variable *serv-sock* = #f;

define class <client> (<object>)
  slot name :: <string>, init-keyword: name:;
  slot in-stream, init-keyword: in:;
  slot out-stream, init-keyword: out:;
  slot command;
end class;


define function init-server(file :: <string>,
			    num-clients :: <integer>) => ()

  *clients* := make(<vector>, size: vector(num-clients), fill: #f);
  receive-board(make(<file-stream>, locator: file, direction: #"input",
		     if-does-not-exist: #"signal", element-type: <byte-character>),
		*board*);
end function;


define function await-clients(port, num-clients) => ()
  *serv-sock* := tcp-server-socket(port);

  for(num from 0 below num-clients)
    let (i-s, o-s)
      = tcp-server-accept(*serv-sock*);
    let n = read-line(i-s, on-end-of-stream: "Unknown");
    *clients*[num] := make(<client>, name: n, in-stream: i-s, out-stream: o-s);

    send-board(o-s, *board*);

    // send config TODO hardcoded weight and cash
    write-line(o-s, concatenate(integer-to-string(num-clients),
				" 25 1000"));
  end;

  let bot = make(<robot>,
		 id: num-client, capacity: 25,
		 money: 1000, x: 42, y: 42);
  add-bot(*board*, bot);

  // send initial response to client
  let fst = #t;
  let str = make(<string>);
  for(cli from 0 below num-clients)
    for(num from 0 below num-clients)

      if(bot-alive?(*board*, num))
	if(fst)
	  fst := #f;
	else
	  concatenate(str, " ");
	end;
	concatenate(str,
		    "#", integer-to-string(num-clients),
		    " X ", integer-to-string(bot-column(*board*, num)),
		    " Y ", integer-to-string(bot-row(*board*, num)));
      end;
    end;
    write-line(*clients*[cli].out-stream, str);
    force-output(*clients*[cli].out-stream);
  end;

end function;


define function contest-over?() => (b :: <boolean>);
  packages-left(*board*) = 0 | bots-alive(*board*) = 0;
end function;

define function process-turn(num-clients) => ();
  for(cli from 0 below num-clients)
    let client = *clients*[cli];
    // issue current location packages; its a collection of package details
    let packs = packages-at-bot(*board*, cli);
    let fst = #t;
    let str = make(<string>);
    for(p in packs)
      if(fst)
	fst := #f;
      else
	concatenate(str, " ");
      end;
      concatenate(str,
		  integer-to-string(p.id), " ",
		  integer-to-string(p.dest-column), " ",
		  integer-to-string(p.dest-row), " ",
		  integer-to-string(p.weight));
    end;
    write-line(client.out-stream, str);
    force-output(client.out-stream);
  end;

  // get each clients command and store (bid, id) in a list
  let bid-col = #();
  for(cli from 0 below num-clients)
    let line = read-line(*clients*[cli].in-stream);
    *clients*[cli].command := split(" ", line);
    bid-col := add!(bid-col, pair(string-to-integer(*clients*[col].command[0]), cli));
  end;

  
  // work out bids order
  bid-col := sort!(bid-col, test: method(a, b) => (c)
				      head(a) < head(b);
				  end method);
  bid-col := reverse!(bid-col);

  // execute commands and record them as we go
  let history = make(<string>);
  for(cur in bid-col)
    let client-id = tail(cur);
    select(*clients*[client-id].command[1])
      "Move" =>
	//TODO but we also have to take pushes into account below :(
	select(*clients*[client-id].command[2])
	  "N" => move-bot-north(*board*, client-id); 
	  "S" => move-bot-south(*board*, client-id);
	  "W" => move-bot-west(*board*, client-id);
	  "E" => move-bot-east(*board*, client-id);
	end;
      "Drop" =>
	let id-list = copy-subsequence(*clients*[client-id].command, start: 3);	// may be empty
      "Pick" =>
	let id-list = copy-subsequence(*clients*[client-id].command, start: 3);	// may be empty
      otherwise => error("Unknown client command");
    end;

  end;

  // tell each client what happended
  for(cli from 0 below num-clients)
    write-line(*clients*[cli].out-stream, history);
  end;
end function;


main(application-name(), application-arguments());
