module: dfmc-typist

define primary class <typist-type> (<object>)
end;

define primary class <tuple> (<typist-type>)
  slot tuple-types :: <simple-object-vector>,
    required-init-keyword: tuples:;
end;

define primary class <tuple-with-rest> (<tuple>)
end;
 
define primary class <arrow> (<typist-type>)
  constant slot arrow-arguments :: <node>,
    required-init-keyword: arguments:;
  constant slot arrow-values :: <node>,
    required-init-keyword: values:;
end;

define primary class <limited-collection> (<typist-type>)
  constant slot collection-class :: <node>,
    required-init-keyword: class:;
  constant slot element-type :: <node>,
    required-init-keyword: element-type:;
end;

define primary class <type-variable> (<typist-type>)
  //should actually be superfluous, since tv's node should have a representative!
  slot type-variable-contents :: type-union(<&type>, <typist-type>),
    required-init-keyword: contents:;
end;

define compiler-sideways method print-object (l :: <arrow>, str :: <stream>) => ()
  format(str, "%= => %=", l.arrow-arguments, l.arrow-values);
end;

define compiler-sideways method print-object (tv :: <type-variable>, str :: <stream>) => ()
  format(str, "TV: %=", tv.type-variable-contents);
end;

define compiler-sideways method print-object (tv :: <tuple>, str :: <stream>) => ()
  format(str, "{ %= }", tv.tuple-types);
end;

define compiler-sideways method print-object (c :: <limited-collection>, str :: <stream>) => ()
  format(str, "limited[ %= ]", c.element-type);
end;



