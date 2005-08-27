module: buddha

define class <zone> (<object>)
  slot zone-name :: <string>, required-init-keyword: name:;
  slot zone-reverse? :: <boolean> = #f;
  slot zone-hosts :: <list> = #();
  slot zone-hostmaster :: <string>, init-keyword: hostmaster:;
  slot zone-serial :: <integer>, init-keyword: serial:;
  slot zone-refresh :: <integer>, init-keyword: refresh:;
  slot zone-retry :: <integer>, init-keyword: retry:;
  slot zone-expire :: <integer>, init-keyword: expire:;
  slot zone-time-to-live :: <integer>, init-keyword: time-to-live:;
  slot zone-nameserver :: <list>, init-keyword: nameserver:;
  slot zone-mail-exchange :: <list>, init-keyword: mail-exchange:;
  slot zone-text :: <list>, init-keyword: txt:;
end class;

define method print-object (zone :: <zone>, stream :: <stream>)
 => ();
  format(stream, "%s", zone.zone-name);
end method;

define method print-html (zone :: <zone>, stream :: <stream>)
 => ()
  gen-row(stream, list(zone.zone-name));
end;

define method print-bind-zone-file (zone :: <zone>, stream :: <stream>)
end;
