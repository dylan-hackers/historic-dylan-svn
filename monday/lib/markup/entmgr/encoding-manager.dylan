Module: encoding-manager


define open abstract class <encoding> (<object>)

end class;
            
define open abstract class <single-byte-encoding> (<encoding>)
  
end class;
            
define open generic single-byte-encoding-table
    (encoding :: <single-byte-encoding>)
 => (table :: <vector>);
            
define open generic decode-buffer-size
    (encoding :: <encoding>,
     input-size :: <integer>)
 => (output-size :: <integer>);
            
define open generic encode-buffer-size
    (encoding :: <encoding>,
     input-size :: <integer>)
 => (output-size :: <integer>);

            
define open generic decode-characters
    (encoding :: <encoding>,
     output :: <sequence>,
     input :: <sequence>,
     #key start: start-offset, end: end-offset, offset)
 => (output-size :: <integer>, undecoded-size :: <integer>);
            
define open generic encode-characters
    (encoding :: <encoding>,
     output :: <sequence>,
     input :: <sequence>,
     #key start: start-offset, end: end-offset, offset)
 => (output-size :: <integer>, unencoded-size :: <integer>);
            
define function make-encoding (encoding-name :: <string>)
 => (encoding :: <encoding>);
  
          
end function;
            
define function register-encoding (encoding-name :: <string>,
                                   encoding-class :: <class>)
 => ();
  if (~subtype?(encoding-class, <encoding>))
    error("class %= being registered for encoding \"%s\""
          " is not a subtype of <encoding>",
          encoding-class,
          encoding-name);
  end if;

  
          
end function;
            
