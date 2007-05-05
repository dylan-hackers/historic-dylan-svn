module: permission
author: turbo24prg

/*
define macro with-permission
 { with-permission(?action:*)
    ?:body
   end }
 =>
   { block ()
       permitted?(?action);
       ?body
     exception (condition :: <permission-error>)
       get(*unprivileged-page*);
     exception (condition :: <authentication-error>)
       get(*not-logged-in-page*);
     end block }
end;
*/
define class <permission-error> (<error>) end;
define class <authentication-error> (<error>) end;

define generic permitted? (action :: <symbol>, #key #all-keys)
 => (permitted? :: <boolean>);

define method permitted? (action :: <symbol>, #key)
 => (permitted? :: <boolean>);
  #t;
end;
