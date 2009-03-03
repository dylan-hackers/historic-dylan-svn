module: dfmc-typist

define thread variable *typist-visualize* :: false-or(<function>) = #f;

define function debug-types (key :: <symbol>, #rest args)
  *typist-visualize* & apply(*typist-visualize*, key, map(get-id, args))
end;

define method get-id (c :: <computation>)
  c.computation-id;
end;

define method get-id (t :: <temporary>)
  t.temporary-id;
end;

define method get-id (tv :: <type-variable>)
  tv.type-variable-id;
end;

define method get-id (o :: <object>)
  format-to-string("%=", o);
end;
