module: dfmc-typist

define class <type-variable> (<object>)
  slot %type-variable-contents :: <&type>,
    init-keyword: contents:;
  constant slot type-variable-id :: <integer>,
    init-function: next-computation-id;
  slot type-contents-callback :: false-or(<function>) = #f;
end;

define method type-variable-contents (t :: <type-variable>) => (res :: <&type>)
  t.%type-variable-contents;
end;

define method type-variable-contents-setter (te :: <&type>, t :: <type-variable>)
 => (res :: <&type>)
  t.%type-variable-contents := te;
  if (t.type-contents-callback)
    t.type-contents-callback(t);
  end;
  te;
end;
