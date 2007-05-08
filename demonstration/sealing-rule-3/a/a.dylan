module: a
synopsis: 
author: 
copyright: 

define open generic m(x);

define open class <t> (<object>)
end;

define open class <s> (<object>)
end;

define method m (s :: <s>)
end;

define sealed domain m(<t>);
