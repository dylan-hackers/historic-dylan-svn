module: zlib

define C-function zlib-compress
  parameter destination :: <C-string>;
  parameter destination-length :: <C-int*>;
  parameter source :: <C-string>;
  parameter source-length :: <C-int>;
  result return-code :: <C-int>;
  c-name: "compress"
end;

define C-function zlib-compress-bound
  parameter source-length :: <C-int>;
  result return-code :: <C-int>;
  c-name: "compressBound"
end;

define function compress (string :: <string>)
 => (compressed-string :: <string>);
  let destination-length :: <C-int*> = make(<C-int*>);
  destination-length.pointer-value := zlib-compress-bound(size(string));
  let result = make(<C-string>, size: destination-length.pointer-value, fill: ' ');
  if (zlib-compress(result, destination-length, string, size(string)) ~= 0)
    error("zlib compress failed.");
  else
    result;
  end if;
end;
/*
define variable foobar = "foobar";
format-out("%= => %=\n", foobar, compress(foobar));
*/
