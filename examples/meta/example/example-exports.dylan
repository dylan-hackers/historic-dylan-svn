library: example
module: dylan-user

define library example
  use common-dylan;
  use streams;
  use format;
  use format-out;
  use standard-io;
  use meta;
 // use string-extensions;
end library;

define module example
  use common-dylan;
//  use extensions;
  use streams;
  use format;
  use format-out;
  use standard-io;
  use meta;

  export parse-integer, parse-finger-query;

 // use string-conversions;
 // use character-type;
end module;
