Module: source-location-test-suite

define library-spec source-location ()
  module source-location;
  module source-location-rangemap;
end library-spec source-location;

define module-spec source-location ()
  open abstract class <source-location> (<object>);
  sealed instantiable class <unknown-source-location> (<source-location>);
  sealed instantiable class <union-source-location> (<source-location>);
  function source-head (<union-source-location>) => (<source-location>);
  function source-tail (<union-source-location>) => (<source-location>);
  open generic-function merge-source-locations (<source-location>, <source-location>) => (<source-location>);
  open generic-function source-location (<object>) => (<source-location>);
  open abstract class <source-location-mixin> (<object>);
  sealed instantiable class <file-source-location> (<source-location>);
  function source-file (<file-source-location>) => (<string>);
  function source-start-line (<file-source-location>) => (<integer>);
  function source-start-column (<file-source-location>) => (false-or(<integer>));
  function source-end-line (<file-source-location>) => (<integer>);
  function source-end-column (<file-source-location>) => (false-or(<integer>));
end module-spec source-location;

define module-spec source-location-rangemap ()
  sealed instantiable class <source-location-rangemap> (<object>);
  function rangemap-one-to-one? (<source-location-rangemap>) => (<boolean>);
  function rangemap-one-to-one?-setter (<boolean>, <source-location-rangemap>) => (<boolean>);
  function range-source-location (<source-location-rangemap>, <integer>, <integer>) => (<source-location>);
  function rangemap-add-line (<source-location-rangemap>, <integer>, false-or(<integer>)) => ();
  function rangemap-add-line-file (<source-location-rangemap>, <integer>, <integer>, <string>) => ();
end module-spec source-location-rangemap;

