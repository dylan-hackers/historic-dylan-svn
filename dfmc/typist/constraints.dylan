module: dfmc-typist

define abstract class <constraint> (<object>)
  constant slot left-hand-side :: <type-variable>,
    required-init-keyword: left:;
  constant slot right-hand-side :: type-union(<&type>, <type-variable>),
    required-init-keyword: right:;
end;

define class <equality-constraint> (<constraint>)
end;

define class <explicit-instance-constraint> (<constraint>)
end;

define class <implicit-instance-constraint> (<constraint>)
  constant slot monomorphic-type-variables :: <stretchy-vector>
    = make(<stretchy-vector>), init-keyword: monomorphic:;
end;


