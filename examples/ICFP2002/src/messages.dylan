module: messages

// This library contains code to read and write the ICFP 2002 message
// protocols.

// The general design of the library is that methods are divided into
// two classes, send-foo and receive-foo.

// Expected interface for boards:
// 
// make(class == <board>, rows: <integer>, cols: <integer>) => <board>;
// This is the board constructor.
//
// The things that go inside boards: 
//   make(<space>)
//   make(<water>)
//   make(<wall>)
//   make(<base>)
//
// Setting board data: aref(space, <board>, row, col) => ()
// Making robots -- robot(N) should be a hash-consing constructor
// that returns the same robot if N is repeated (aka Flyweight pattern).

// Constants and error handling. 

define variable *debug* = #t;

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
  constant slot data :: <list>,
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

define function debug(fmt :: <string>, #rest args) => ()
  if (*debug*)
    apply(format-out, *standard-error*, fmt, args);
    force-output(*standard-error*);
  end if;
end function debug;

// Sending functions. 

define function send-player (s :: <stream>) => ()
  format(s, $player-msg);
  debug("send-player:\n%s", $player-msg);
end function send-player;

// This code is kind of yucky. How can I clean it up?

define generic send-move (s :: <stream>,
                          bid :: <integer>,
                          direction :: <direction>) => ();

define method send-move (s :: <stream>,
                         bid :: <integer>,
                         direction :: <north>) => ();
  format(s, "%d ", bid);
  format(s, "%s\n", $north-string);
  debug("send-move: bid = %d and direction <north>\n")
end function send-move;

define method send-move (s :: <stream>,
                         bid :: <integer>,
                         direction :: <south>) => ();
  format(s, "%d ", bid);
  format(s, "%s\n", $south-string);
  debug("send-move: bid = %d and direction <south> \n")
end function send-move;

define method send-move (s :: <stream>,
                         bid :: <integer>,
                         direction :: <east>) => ();
  format(s, "%d ", bid);
  format(s, "%s\n", $east-string);
  debug("send-move: bid = %d and direction <east>\n")
end function send-move;

define method send-move (s :: <stream>,
                         bid :: <integer>,
                         direction :: <west>) => ();
  format(s, "%d ", bid);
  format(s, "%s\n", $west-string);
  debug("send-move: bid = %d and direction <west>\n")
end function send-move;

// This sends a message to pick up some objects. 

define function send-package-ids (s :: <stream>,
                                  package-ids :: <sequence>) => ()
  let n = package-ids.size;
  for (i from 0 below n)
    format(s, "%d", package-ids[i]);
    format(s, if (i < n - 1) " " else "\n" end);
  end for;
end function send-package-ids;

define method send-pick (s :: <stream>,
                         big :: <integer>,
                         package-ids :: <sequence>) => ();
  format(s, "%d Pick", bid);
  debug("send-pick: %d Pick %=\n", package-ids);
  send-package-ids(s, package-ids);
end method send-pick;

define method send-drop (s :: <stream>,
                         big :: <integer>,
                         package-ids :: <sequence>) => ();
  format(s, "%d Drop", bid);
  debug("send-drop: %d Pick %=\n", package-ids);
  send-package-ids(s, package-ids);
end method send-drop;

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
     board :: <board>)
  let board = receive-board-layout(s);
  let (robot-id, carry-max, money) = receive-client-configuration(s);
  receive-initial-robot-positions(s, b);
  values(robot-id, carry-max, money, board);
end function receive-initial-setup;

define function receive-board-layout (s :: <stream>) => <board>;
  block()
    let (rows, cols) = receive-board-dimensions(s);
    let board = make(<board>, rows: rows, cols: cols);
    for (i from 0 below rows)
      receive-board-row(s, board, i, cols)
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
    (s :: <stream>, b :: <board>) => ()
  let (robot-id, row, col) = receive-robot-positions(s);
  b[row, col] := robot(robot-id);
  iterate loop (c :: <character> = s.read-element)
    select (c)
      ' ' =>
        begin
          let (robot-id, row, col) = receive-robot-positions(s);
          b[row, col] := robot(robot-id);
          loop(s.read-element);
        end;
      '\n' => #f;
      otherwise => message-error("receive-initial-robot-positions\n");
    end select;
  end iterate;
  debug("receive-initial-robot-positions\n");
end function receive-initial-robot-positions;

// Supporting functions.

// Read one row of a board data from a stream and update the board with it.

define function receive-board-row (s :: <stream>,
                                   b :: <board>,
                                   row :: <integer>,
                                   cols :: <integer>) => ()
  for (j from 0 below cols)
    let c = s.read-element;
    select ()
      $empty-char => b[row, j] := make(<space>);
      $water-char => b[row, j] := make(<water>);
      $wall-char  => b[row, j] := make(<wall>);
      $base-char  => b[row, j] := make(<base>);
      otherwise =>
        message-error("receive-board-row: bad board element '%c'\n", c);
    end select;
  finally
    block ()
      receive-newline(s);
    exception (e :: <message-error>)
      add-error("receive-board-row -- row did not terminate as expected\n");
    end block;
  end for;
  debug("receive-board-row: row %d, %d cols read\n", row, cols);
end function receive-board-row;

define function 

// Read the dimensions of a board from a stream.

define function receive-board-dimensions (s :: <stream>)
 => (rows :: <integer>, cols :: <integer>)
  block()
    let x = receive-integer(s);
    receive-spaces(s);
    let y = receive-integer(s);
    receive-newline(s);
    debug("receive-integer: %d x %d\n", x, y);
    //
    values(x, y);
  exception (e :: <message-error>)
    add-error(e, "receive-board-dimensions\n");
  end block;
end function receive-board-dimensions;

// This function reads the initial server response and pulls out a 
// robot ID and location

define function receive-robot-location (s :: <stream>)
 => (robot-id :: <integer>, row :: <integer>, col :: <integer>)
  block()
    receive-sharp(s);
    let robot-id = receive-integer(s);
    receive-string(s, " X ");
    let row = receive-integer(s);
    receive-integer(s, " Y ");
    let col = receive-integer(s);
    debug("receive-robot-location: Robot %d at (%d, %d)\n",
          robot-id, row, col);
    values(robot-id, row, col);
  exception (e :: <message-error>)
    add-error(e, "receive-robot-location\n");
  end block;
end function receive-robot-location;


//

// Match zero or more spaces. 

define function receive-spaces (stream :: <stream>) => ()
  while (stream.peek = ' ')
    stream.read-element; // Discard spaces. Return value deliberately ignored.
  end while;
end function receive-spaces;

// Match exactly one space.

define function receive-space (stream :: <stream>) => ()
  let c = stream.read-element;
  unless (c = ' ')
    message-error("receive-space: expected space, got '%c'\n", c);
  end unless;
end function receive-space;
                           
// Match exactly one newline. 
      
define function receive-newline (stream :: <stream>) => ()
  let c = stream.read-element;
  unless (c = '\n')
    message-error("receive-newline: got '%c' instead of newline\n", c)
  end unless;
end function receive-newline;

// Match exactly one '#'

define function receive-sharp (stream :: <stream>) => ()
  let c = stream.read-element;
  unless (c = '#')
    message-error("receive-sharp: got '%c' instead of newline\n", c)
  end unless;
end function receive-sharp;

// Test to see if an expected string is on the stream, consuming the
// string if it is.

define function receive-string (stream :: <stream>, str :: <string>) => ()
  let str* = read(stream, str.size);
  unless (str = str*)
    message-error("receive-string: expected '%s', got '%s'\n", str, str*);
  end unless;
end function receive-string;

// Read an integer from a string. The integer *CANNOT* be prefixed with
// spaces. It may start with 1 '-'. 

define function receive-integer (stream :: <stream) => int :: <integer>;
  let v = make(<stretchy-vector>);
  when (stream.peek = '-')
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

