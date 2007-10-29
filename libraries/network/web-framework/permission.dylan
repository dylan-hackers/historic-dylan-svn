module: permission
author: turbo24prg

define macro with-permission
 { with-permission (?action:*)
    ?:body
   end }
 =>
   { block ()
       permitted?(?action);
       ?body
     exception (condition :: <permission-error>)
       permission-error(?action)
     exception (condition :: <authentication-error>)
       authentication-error(?action);
     end block }
end;

define open generic authentication-error (action :: <object>, #key #all-keys);
define open generic permission-error (action :: <object>, #key #all-keys);

define class <permission-error> (<error>) end;
define class <authentication-error> (<error>) end;

define open generic permitted? (action :: <object>, #key #all-keys)
 => (permitted? :: <boolean>);

define method permitted? (action :: <symbol>, #key)
 => (permitted? :: <boolean>);
  #t;
end;
