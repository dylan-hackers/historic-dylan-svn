module: web-framework

define function printable (title :: <string>)
  regexp-replace(title, "/", "");
end;
