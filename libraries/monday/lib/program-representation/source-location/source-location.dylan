Module: source-location


define open abstract class <source-location> (<object>)
  // No slots
end class;
              
define class <unknown-source-location> (<source-location>)
  // No slots
end class;
              
define variable *unknown-source-location*
  :: false-or(<unknown-source-location>) = #f;
              
define sealed method make
    (class == <unknown-source-location>, #key)
 => (instance :: <unknown-source-location>);
  *unknown-source-location*
    | (*unknown-source-location* := next-method());
end method;

              
define sealed domain initialize(<unknown-source-location>);
              
define class <union-source-location> (<source-location>)
  constant slot source-head :: <source-location>,
    required-init-keyword: head:;
  constant slot source-tail :: <source-location>,
    required-init-keyword: tail:;
end class;
              
define open generic merge-source-locations
    (location1 :: <source-location>,
     location2 :: <source-location>)
 => (location :: <source-location>);
              
define method merge-source-locations
    (location1 :: <source-location>,
     location2 :: <source-location>)
 => (location :: <source-location>);
  make(<union-source-location>, head: location1, tail: location2);
end method;
              
define open generic source-location
    (object :: <object>)
 => (location :: <source-location>);
              
define sealed method source-location
    (location :: <source-location>)
 => (location :: <source-location>);
  location
end method;
              
define open abstract class <source-location-mixin> (<object>)
  sealed constant slot source-location :: <source-location>
    = make(<unknown-source-location>), init-keyword: source-location:;
end class;
              
define class <file-source-location> (<source-location>)
  constant slot source-file :: <file-locator>,
    required-init-keyword: file:;
  constant slot source-start-line :: <integer>,
    required-init-keyword: start-line:;
  constant slot source-start-column :: false-or(<integer>) = #f,
    init-keyword: start-column:;
  constant slot source-end-line :: <integer>,
    required-init-keyword: end-line:;
  constant slot source-end-column :: false-or(<integer>) = #f,
    init-keyword: end-column:;
end class;
              
