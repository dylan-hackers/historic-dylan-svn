module: messages

// This library contains code to read and write the ICFP 2002 message
// protocols.

// The general design of the library is that methods are divided into
// two classes, send-foo and receive-foo.

// Expected interface for boards:
// 
// make(class == <board>, max-x: <integer>, max-y: <integer>) => <board>;
// This is the board constructor.
//
// The things that go inside boards: 
//   make(<space>)
//   make(<water>)
//   make(<wall>)
//   make(<base>)
//
// Setting board data: aref(space, <board>, x, y) => ()
// Making robots -- robot(N) should be a hash-consing constructor
// that returns the same robot if N is repeated (aka Flyweight pattern).
//
// For commands:
// I need the types <north> <south> <east> <west> & <direction>

// Constants and error handling. 

define constant $player-msg = "Player\n";
define constant $empty-char = '.';
define constant $water-char = '~';
define constant $wall-char = '#';
define constant $base-char = '@';

define constant $north-string = "N";
define constant $south-string = "S";
define constant $west-string = "W";
define constant $east-string = "E";

// The <message-error> class is the class that signals an error during
// sending or receiving a message. The message-error function creates
// an error message. add-error lets you trap a <message-error>, add
// some supplementary error information to the exception, and reraise.

define class <message-error> (<error>)
  slot data :: <list>,
    required-init-keyword: data:;
end class <message-error>;

define function message-error (fmt :: <string>, #rest args)
  error(make(<message-error>,
             data: list(apply(format-to-string, fmt, args))));
end function message-error;

define function add-error (e :: <message-error>, fmt :: <string>, #rest args)
  let msg = apply(format-to-string, fmt, args);
  e.data := add(msg, e.data);
  error(e)
end function add-error;


// Sending functions. 

define function send-player (s :: <stream>) => ()
  force-format(s, $player-msg);
  debug("send-player:\n%s", $player-msg);
end function send-player;

// This code is kind of yucky. How can I clean it up?

define generic send-command (s :: <stream>, c :: <command>) => ();

define method send-command (s :: <stream>, command :: <move>) => ();
  let direction-string =
    select (command.direction)
      $north => $north-string;
      $south => $south-string;
      $east => $east-string;
      $west => $west-string;
      otherwise => error("send-command: Can't happen!")
    end select;
  force-format(s, "%d Move %s\n", command.bid, direction-string);
  debug("send-command(<move>): bid %d and dir %s\n",
        command.bid, direction-string);
end method send-command;

// This sends a message to pick up some objects. 

define function send-package-ids (s :: <stream>,
                                  package-ids :: <sequence>) => ()
  let n = package-ids.size;
  for (i from 0 below n)
    force-format(s, " %d", package-ids[i]);
//    force-format(s, if (i < n - 1) " " else "\n" end);
  end for;
end function send-package-ids;

define method send-command (s :: <stream>, command :: <pick>) => ()
  force-format(s, "%d Pick", command.bid);
  send-package-ids(s, command.package-ids);
  debug("send-command(<pick>): bid %d, ids %=\n",
        command.bid, command.package-ids)
end method send-command;

define method send-command (s :: <stream>, command :: <drop>) => ()
  force-format(s, "%d Drop", command.bid);
  send-package-ids(s, command.package-ids);
  debug("send-command(<drop>): bid %d, ids %=\n",
        command.bid, command.package-ids)
end method send-command;

// 

// Basic receiver functions. These functions will let you understand the
// server and send messages to it. 

define function receive-player (s :: <stream>) => ()
  receive-string(s, $player-msg);
  debug("receive-player\n");
end function receive-player;

define function receive-initial-setup (s :: <stream>)
 => (robot-id :: <integer>,
     carry-max :: <integer>,
     money :: <integer>,
     state :: <state>)
  let board = receive-board-layout(s);
  let (robot-id, carry-max, money) = receive-client-configuration(s);
  let state = receive-initial-robot-positions(s, board);
  state := search-for-bases(state);
  values(robot-id, carry-max, money, state);
end function receive-initial-setup;

define function search-for-bases(state :: <state>)
 => (state :: <state>);
  for (r from 1 to state.board.height)
    for (c from 1 to state.board.width)
      let terrain = state.board[r, c];
      if (instance?(terrain, <base>))
        debug("found base at %d,%d\n", c,r);
        state.bases := add!(state.bases, point(x: c, y: r));
      end;
    end;
  end;
  state;
end;

define function receive-board-layout (s :: <stream>) => <board>;
  block()
    let (max-x, max-y) = receive-board-dimensions(s);
    let board = make(<board>,
                     dimensions: list(max-y, max-x)); // deliberate transpose
    for (y from 1 to max-y)
      receive-board-row(s, board, max-x, y)
    end for;
    debug("receive-board\n");
    board
  exception (e :: <message-error>)
    add-error(e, "Receive-board failed");
  end block;
end function receive-board-layout;

define function receive-client-configuration (s :: <stream>)
 => (robot-id :: <integer>, carry-max :: <integer>, money :: <integer>)
  block()
    let robot-id = receive-integer(s);
    receive-space(s);
    let carry-max = receive-integer(s);
    receive-space(s);
    let money = receive-integer(s);
    receive-newline(s);
    debug("receive-configuration: (%d, %d, %d)\n",
          robot-id, carry-max, money);
    values(robot-id, carry-max, money);
  exception (e :: <message-error>)
    add-error(e, "receive-configuration\n");
  end block;
end function receive-client-configuration;

// 

define function receive-initial-robot-positions
    (s :: <stream>, b :: <board>) => <state>;
  let state = make(<state>, board: b);
  let (robot-id, x, y) = receive-robot-location(s);
  state := add-robot(state, make(<robot>, id: robot-id, 
				 location: point(x: x, y: y)));
  iterate loop (c :: <character> = s.read-element)
    select (c)
      ' ' =>
        begin
          let (robot-id, x, y) = receive-robot-location(s);
          state := add-robot(state, make(<robot>, id: robot-id, 
					 location: point(x: x, y: y)));
          loop(s.read-element);
        end;
      '\n' => #f;
      otherwise => message-error("receive-initial-robot-positions\n");
    end select;
  end iterate;
  debug("receive-initial-robot-positions\n");
  state;
end function receive-initial-robot-positions;

// Supporting functions.

// Read one row of a board data from a stream and update the board with it.

define function receive-board-row (s :: <stream>,
                                   b :: <board>,
                                   max-x :: <integer>,
                                   y :: <integer>) => ()
  for (x from 1 to max-x)
    let c = s.read-element;
    select (c)
      $empty-char => b[y, x] := make(<space>);
      $water-char => b[y, x] := make(<water>);
      $wall-char  => b[y, x] := make(<wall>);
      $base-char  => b[y, x] := make(<base>);
      otherwise =>
        message-error("receive-board-row: bad board element '%c'\n", c);
    end select;
  finally
    block ()
      receive-newline(s);
    exception (e :: <message-error>)
      add-error(e, "receive-board-row -- row did not terminate as expected\n");
    end block;
  end for;
  debug("receive-board-row: max-x = %d, y = %d read\n", max-x, y);
end function receive-board-row;


// Read the dimensions of a board from a stream.
define function receive-board-dimensions (s :: <stream>)
 => (max-x :: <integer>, max-y :: <integer>)
  block()
    let x = receive-integer(s);
    receive-spaces(s);
    let y = receive-integer(s);
    receive-newline(s);
    debug("receive-board-dimensions: %d x %d\n", x, y);
    //
    values(x, y);
  exception (e :: <message-error>)
    add-error(e, "receive-board-dimensions\n");
  end block;
end function receive-board-dimensions;

// This function reads the initial server response and pulls out a 
// robot ID and location

define function receive-robot-location (s :: <stream>)
 => (robot-id :: <integer>, x :: <integer>, y :: <integer>)
  block()
    receive-sharp(s);
    let robot-id = receive-integer(s);
    receive-string(s, " X ");
    let x = receive-integer(s);
    receive-string(s, " Y ");
    let y = receive-integer(s);
    debug("receive-robot-location: Robot %d at (%d, %d)\n",
          robot-id, x, y);
    values(robot-id, x, y);
  exception (e :: <message-error>)
    add-error(e, "receive-robot-location\n");
  end block;
end function receive-robot-location;


//

// Match zero or more spaces. 

define function receive-spaces (stream :: <stream>) => ()
  while (stream.peek == ' ')
    stream.read-element; // Discard spaces. Return value deliberately ignored.
  end while;
end function receive-spaces;

// Match exactly one space.

define function receive-space (stream :: <stream>) => ()
  let c = stream.read-element;
  unless (c == ' ')
    message-error("receive-space: expected space, got '%c'\n", c);
  end unless;
end function receive-space;

// Match exactly one newline. 
      
define function receive-newline (stream :: <stream>) => ()
  let c = stream.read-element;
  unless (c == '\n')
    message-error("receive-newline: got '%c' instead of newline\n", c)
  end unless;
end function receive-newline;

// Match exactly one '#'

define function receive-sharp (stream :: <stream>) => ()
  let c = stream.read-element;
  unless (c == '#')
    message-error("receive-sharp: got '%c' instead of newline\n", c)
  end unless;
end function receive-sharp;

// Test to see if an expected string is on the stream, consuming the
// string if it is.

define function receive-string (stream :: <stream>, str :: <string>) => ();
  let str* = read(stream, str.size);
  unless (str = str*)
    message-error("receive-string: expected '%s', got '%s'\n", str, str*);
  end unless;
end function receive-string;

// Read an integer from a string. The integer *CANNOT* be prefixed with
// spaces. It may start with 1 '-'. 

define function receive-integer (stream :: <stream>) => int :: <integer>;
  let v = make(<stretchy-vector>);
  when (stream.peek == '-')
    v := add!(v, stream.read-element);
  end when;
  while (stream.peek.digit?)
    v := add!(v, stream.read-element);
  end while;
  block()
    string-to-integer(as(<string>, v));
  exception (e :: <error>) 
    message-error("receive-integer: Couldn't convert '%s' to integer\n",
                  as(<string>, v));
  end block;
end function receive-integer;

// Functions for receiving information from the server

// Read package information from the server
define function receive-package-information(s :: <stream>) 
  => (id :: <integer>, x :: <integer>, y :: <integer>, weight :: <integer>)
    let package-id = receive-integer(s);
    receive-space(s);
    let package-x = receive-integer(s);
    receive-space(s);
    let package-y = receive-integer(s);
    receive-space(s);
    let package-weight = receive-integer(s);
    values(package-id, package-x, package-y, package-weight);
end function receive-package-information;

// Return #t if more package information is available on the line
define function more-packages?(s :: <stream>)
  => (r :: <boolean>)
  // Does this need to check for spaces before the new line?
  s.peek ~= '\n';
end function more-packages?;

// Receive server package information
define function receive-server-packages(s :: <stream>, state :: <state>, location :: <point>) 
  => (state :: <state>)
  while(more-packages?(s))
    receive-spaces(s);
    let (id, x, y, weight) = receive-package-information(s);
    state := add-package(state, make(<package>,
                                     id: id,
                                     location: location,
                                     dest: point(x: x, y: y),
                                     weight: weight));
    debug("receive-server-packages: %d %d %d %d\n", id, x, y, weight);
  end while;
  receive-newline(s);
  debug("receive-server-packages: end\n");
  state;
end function receive-server-packages;

// Instantiate a <command> based on the given character and contents
// of the stream.
define generic do-receive-server-command(c :: <character>, s :: <stream>, id :: <integer>)
  => (r :: <command>);

define method do-receive-server-command(c == 'N', s :: <stream>, id :: <integer>)
  => (r :: <command>)
  debug("move N command received\n");
  make(<move>, direction: $north, bid: 1, id: id);
end method do-receive-server-command;

define method do-receive-server-command(c == 'S', s :: <stream>, id :: <integer>)
  => (r :: <command>)
  debug("move S command received\n");
  make(<move>, direction: $south, bid: 1, id: id);
end method do-receive-server-command;

define method do-receive-server-command(c == 'E', s :: <stream>, id :: <integer>)
  => (r :: <command>)
  debug("move E command received\n");
  make(<move>, direction: $east, bid: 1, id: id);
end method do-receive-server-command;

define method do-receive-server-command(c == 'W', s :: <stream>, id :: <integer>)
  => (r :: <command>)
  debug("move W command received\n");
  make(<move>, direction: $west, bid: 1, id: id);
end method do-receive-server-command;

define method do-receive-server-command(c == 'P', s :: <stream>, id :: <integer>)
  => (r :: <command>)
  debug("pick command received\n");
  receive-spaces(s);  
  make(<pick>, bid: 1, id: id, package-ids: list(receive-integer(s)));
end method do-receive-server-command;

define method do-receive-server-command(c == 'D', s :: <stream>, id :: <integer>)
  => (r :: <command>)
  debug("drop command received\n");
  receive-spaces(s);  
  make(<drop>, bid: 1, id: id, package-ids: list(receive-integer(s)));
end method do-receive-server-command;

define method do-receive-server-command(c == 'X', s :: <stream>, id :: <integer>)
  => (r :: <command>)
  debug("transform command received\n");
  receive-spaces(s);  
  let x = receive-integer(s);
  receive-spaces(s);
  receive-string(s, "Y");
  receive-spaces(s);
  let y = receive-integer(s);
  make(<transport>, location: point(x: x, y: y), bid: 1, id: id);
end method do-receive-server-command;

define function receive-server-command(stream :: <stream>, id :: <integer>)
  => (r :: <command>)
  do-receive-server-command(stream.read-element, stream, id);  
end function receive-server-command;

define function more-commands?(s :: <stream>) => (r :: <boolean>)
  receive-spaces(s);
  let ch :: <character> = s.peek;
  ch ~= '#' & ch ~= '\n';
end function more-commands?;

define function more-events?(s :: <stream>) => (r :: <boolean>)
  s.peek ~= '\n';
end function more-events?;

define generic process-server-command(state :: <state>, command :: <command>) => (state :: <state>);

define function receive-server-command-reply(s :: <stream>, state :: <state>) => (state :: <state>)
  block()
    while(more-events?(s))
      receive-sharp(s);
      let id = receive-integer(s);    
      while(more-commands?(s))
        let command = receive-server-command(s, id);
        debug("receive-server-command-reply: %d %=\n", id, command);
        state := process-server-command(state, command);
      end while;
    end while;
    receive-newline(s);
    debug("receive-server-command-reply: end\n");
    state;
  exception(e :: <message-error>)
    add-error(e, "receive-server-command-reply failed");
  end;
end function receive-server-command-reply;

// Ignore commands I don't handle for now.
define method process-server-command(state :: <state>, command :: <command>) => (state :: <state>)
  state   
end method process-server-command;

define method process-server-command(state :: <state>, command :: <move>) => (state :: <state>)
  let bot = find-robot(state, command.robot-id);
  let old-location = bot.location;
  let new-location =
    select (command.direction)
      $north => point(x: old-location.x,     y: old-location.y + 1);
      $east  => point(x: old-location.x + 1, y: old-location.y);
      $south => point(x: old-location.x,     y: old-location.y - 1);
      $west  => point(x: old-location.x - 1, y: old-location.y);
    otherwise => error("process-server-command: Can't happen!")
  end select;

  // For this bot, work through all the packages it carries and
  // update it's location.
  let ps = choose(method(p) p.carrier.id = bot.id end, state.packages);
  for(p in ps)
    state := add-package(state, copy-package(find-package(state, p.id), 
                                             new-location: new-location));
  end for;
  
  add-robot(state, copy-robot(bot, new-location: new-location));
end method process-server-command;

define method process-server-command(state :: <state>, command :: <pick>) => (state :: <state>)
  let bot = find-robot(state, command.robot-id);
  let loc = bot.location;
  for(pid in command.package-ids)
    state := add-package(state, copy-package(find-package(state, pid, create: #t), 
					     new-location: loc, 
					     new-carrier: bot));
    state := add-robot(state, copy-robot(bot, new-inventory: add(bot.inventory, find-package(state, pid))));
  end for;
  state;
end method process-server-command;

define method process-server-command(state :: <state>, command :: <drop>)
 => (state :: <state>)
  let loc = find-robot(state, command.robot-id).location;
  for(pid in command.package-ids)
    let p = find-package(state, pid, create: #t);
    if (loc = p.dest) // We are at the destination, so kill the package
      let packages* = remove(state.packages, p,
                             test: method (p*, p) p*.id = p.id end method);
      state := make(<state>,
                    board: state.board,
                    bases: state.bases,
                    robots: state.robots,
                    packages: packages*);
    else // otherwise just put the package on the floor.
      state := add-package(state, copy-package(p,
                                               new-location: loc,
                                               new-carrier: #f));
    end if;
    let bot = find-robot(state, command.robot-id);
    let new-inventory = remove(bot.inventory, p,
                               test: method (p*, p) p*.id = p.id end method);
    let bot* = copy-robot(bot, new-inventory: new-inventory);
    state := add-robot(state, bot*);
  end for;
  state;
end method process-server-command;

define method process-server-command(state :: <state>, command :: <transport>)
 => (state :: <state>)
  let bot = find-robot(state, command.robot-id);

  // For this bot, work through all the packages it carries and
  // update it's location.
  let ps = choose(method(p) p.carrier.id = bot.id end, state.packages);
  for(p in ps)
	state := add-package(state, copy-package(find-package(state, p.id), 
                                                 new-location: command.transport-location));
  end for;
  add-robot(state, copy-robot(bot, new-location: command.transport-location));
end method process-server-command;
