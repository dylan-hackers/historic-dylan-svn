module: command

define constant $north = #"north";
define constant $south = #"south";
define constant $east  = #"east";
define constant $west  = #"west";
define constant <direction> = one-of($north, $south, $east, $west);

define abstract class <command> (<object>)
  constant slot bid :: <integer>,
    required-init-keyword: bid:;
end class <command>;

define class <pick> (<command>)
  constant slot package-ids :: <sequence>, // of <integer>
    required-init-keyword: package-ids:;
end class <pick>;

define class <drop> (<command>)
  constant slot package-ids :: <sequence>, // of <integer>
    required-init-keyword: package-ids:;
end class <drop>;

define class <move> (<command>)
  constant slot :: <direction>,
    required-init-keyword: direction:;
end class <move>;

// TODO: add an initializer to make sure that every element of
// package-ids is an <integer>
