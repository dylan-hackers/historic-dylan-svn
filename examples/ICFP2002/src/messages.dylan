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
//   $empty-space
//   $water-space
//   $wall-space
//   $base-space
//
// Setting board data: aref(space, <board>, row, col) => ()



// Constants and error handling. 

define variable *debug* = #t;

define constant $player-msg = "Player\n";
define constant $empty-char = '.';
define constant $water-char = '~';
define constant $wall-char = '#';
define constant $base-char = '@';

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

// Sending functions. 

define function send-player (s :: <stream>) => ()
  write(s, $player-msg);
  debug("send-player:\n%s", $player-msg);
end function send-player;

// Basic receiver functions. 

define function receive-player (s :: <stream>) => ()
  receive-string(s, $player-msg);
  debug("receive-player\n");
end function receive-player;

define function receive-board (s :: <stream>) => <board>;
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
end function receive-board;

define function receive-configuration (s :: <stream>)
 => (unique-id :: <integer>, carry-max :: <integer>, money :: <integer>)
  block()
    let unique-id = receive-integer(s);
    receive-space(s);
    let carry-max = receive-integer(s);
    receive-space(s);
    let money = receive-integer(s);
    receive-newline(s);
    debug("receive-configuration: (%d, %d, %d)\n",
          unique-id, carry-max, money);
    values(unique-id, carry-max, money);
  exception (e :: <message-error>)
    add-error(e, "receive-configuration\n");
  end block;
end function receive-configuration;

define function receive-


// Supporting functions.

define function receive-board-row (s :: <stream>,
                                   b :: <board>,
                                   row :: <integer>,
                                   cols :: <integer>) => ()
  for (j from 0 below cols)
    let c = s.read-element;
    select ()
      $empty-char => b[row, j] := $empty-space;
      $water-char => b[row, j] := $water-space;
      $wall-char  => b[row, j] := $wall-space;
      $base-char  => b[row, j] := $base-space;
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

define function debug(fmt :: <string>, #rest args) => ()
  if (*debug*)
    apply(format-out, *standard-error*, fmt, args);
    force-output(*standard-error*);
  end if;
end function debug;

define function receive-space (stream :: <stream>) => ()
  let c = stream.read-element;
  unless (c = ' ')
    message-error("receive-space: expected space, got '%c'\n", c);
  end unless;
end function receive-space;
                                 
define function receive-spaces (stream :: <stream>) => ()
  while (stream.peek = ' ')
    stream.read-element; // Discard spaces. Return value deliberately ignored.
  end while;
end function receive-spaces;

define function receive-newline (stream :: <stream>) => ()
  let c = stream.read-element;
  unless (c = '\n')
    message-error("receive-newline: got '%c' instead of newline\n", c)
  end unless;
end function receive-newline;

define function receive-string (stream :: <stream>, str :: <string>) => ()
  let str* = read(stream, str.size);
  unless (str = str*)
    message-error("receive-string: expected '%s', got '%s'\n", str, str*);
  end unless;
end function receive-string;

define function receive-integer (stream :: <stream) => <integer>;
  let v = make(<stretchy-vector>);
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