Module: source-location-conditions


define class <source-condition>
    (<format-string-condition>, <source-location-mixin>)
  // no additional slots
end class;
              
define class <source-warning> (<warning>, <source-condition>)
  // no additional slots
end class;
              
define class <source-error> (<error>, <source-condition>)
  // no additional slots
end class;
              
define method source-warning
    (source :: <object>, string :: <string>, #rest arguments)
 => ();
  signal(make(<source-warning>,
             source-location: source-location(source),
             format-string: string,
             format-arguments: arguments));
end method;
              
define method source-error
    (source :: <object>, string :: <string>, #rest arguments)
 => (no-return :: <bottom>);
  error(make(<source-error>,
             source-location: source-location(source),
             format-string: string,
             format-arguments: arguments));
end method;
              
