Module:    com-utilities
Synopsis:  Some helpful utilties to make COM easier to use.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// 'let macro' is non-standard Dylan, but very useful to 
// avoid lots of nested 'with-' macros.
define macro let-com-interface
  { let-com-interface ?:name :: ?type:expression = ?value:expression;
    ?:body }
  => { let ?name :: ?type = ?value;
       block()         
         ?body
       cleanup
         IUnknown/release(?name)
       end }
end macro let-com-interface;

// Query an interface, returning the correct dispatch client
// type or #f if interface cannot be queried for that type.
define function query-interface(interface, dispatch-client-type)
  let iid = dispatch-client-uuid(dispatch-client-type);
  let (status, result) = IUnknown/QueryInterface(interface, iid);

  when(FAILED?(status))
    ole-error(status, IUnknown/QueryInterface, interface, iid);
  end when;

  result;
end function query-interface;

// Given a <BSTR>, convert to a <byte-string> and destroy the <bstr>
// on return.
define function convert-to-string( bstr :: <bstr> ) => (s :: <byte-string>)
  block()
    as(<byte-string>, bstr);
  cleanup
    destroy(bstr);
  end;
end function convert-to-string;

// Given an object, convert it to a type that is safe to use from Dylan,
// without needing to destroy it.
define method convert-to-safe-type( o :: <object> ) => (s :: <object>)
  o
end method convert-to-safe-type;

define method convert-to-safe-type( bstr :: <bstr> ) => (s :: <string>)
  convert-to-string(bstr);
end method convert-to-safe-type;

