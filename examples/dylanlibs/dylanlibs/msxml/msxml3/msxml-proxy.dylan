Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define open class <msxml-proxy> (<object>)
  slot msxml3-interface, required-init-keyword: interface:;
end class <msxml-proxy>;

define method initialize( proxy :: <msxml-proxy>, #key ) => ()
  drain-finalization-queue();
  finalize-when-unreachable(proxy);
end method initialize;

define method finalize( proxy :: <msxml-proxy> ) => ()
  let interface = proxy.msxml3-interface;
  when(interface)
    proxy.msxml3-interface := #f;
    IUnknown/release(interface);
  end when;
end method finalize;

define open generic proxy-class (interface) => (class :: <class>);

define macro proxy-method-definer
  { define proxy-method (?method-name:name, ?interface-type:name, ?function:name) ?:body end }
   => { define method "%" ## ?method-name (interface :: "<" ## ?interface-type ## ">", #rest args)
          apply(?function, interface, args);
        end }
end macro proxy-method-definer;

define macro proxy-method-setter-definer
  { define proxy-method-setter (?method-name:name, ?interface-type:name, ?function:name) ?:body end }
   => { define method "%" ## ?method-name (value :: <object>, interface :: "<" ## ?interface-type ## ">")
          ?function (interface) := value;
        end }
end macro proxy-method-setter-definer;

define method make-proxy( interface  ) => (result)
  make(proxy-class(interface), interface: interface);
end method make-proxy;


