module: code-browser
Synopsis: Brwose Open Dylan environment objects
Author:   Andreas Bogk, Bastian Mueller, Hannes Mehnert

define body tag used-modules in code-browser
 (page :: <code-browser-page>, response :: <response>, do-body :: <function>)
 ()
  do-used-definitions(method(x)
                       dynamic-bind(*environment-object* = x)
                         do-body()
                       end;
                     end, *project*, *environment-object*);
end;

define body tag module-definitions in code-browser
 (page :: <code-browser-page>, response :: <response>, do-body :: <function>)
 ()
  do-module-definitions(method(x)
                          dynamic-bind(*environment-object* = x)
                            do-body()
                          end;
                        end, *project*, *environment-object*)
end;

begin
  main()
end;

