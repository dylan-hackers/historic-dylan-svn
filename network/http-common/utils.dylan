Module:    http-common
Synopsis:  Various small utilities
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Things that expire.

define open generic date-modified
    (object :: <object>)
 => (date :: false-or(<date>));

define open generic date-modified-setter
    (new-date :: false-or(<date>), object :: <object>)
 => (new-date :: false-or(<date>));

define open class <expiring-mixin> (<object>)
  constant slot duration :: <day/time-duration>,
    init-value: encode-day/time-duration(0, 1, 0, 0, 0),      // 1 hour
    init-keyword: #"duration";
  // When the object was last modified (e.g., loaded from a file).
  slot date-modified :: false-or(<date>) = #f;
end;

define method expired?
    (thing :: <expiring-mixin>)
 => (expired? :: <boolean>)
  thing.date-modified == #f
  | begin
      let now = current-date();
      (now - thing.date-modified) < thing.duration
    end
end method expired?;


//// Attributes

define open class <attributes-mixin> (<object>)
  constant slot attributes :: <table>,
    init-function: curry(make, <table>);
end;

define generic get-attribute (this :: <attributes-mixin>, key :: <object>, #key);
define generic set-attribute (this :: <attributes-mixin>, key :: <object>, value :: <object>);
define generic remove-attribute (this :: <attributes-mixin>, key :: <object>);

// API
define method get-attribute
    (this :: <attributes-mixin>, key :: <object>, #key default)
 => (attribute :: <object>)
  element(this.attributes, key, default: default)
end;

// API
define method set-attribute
    (this :: <attributes-mixin>, key :: <object>, value :: <object>)
  this.attributes[key] := value;
end;

// API
define method remove-attribute
    (this :: <attributes-mixin>, key :: <object>)
  this.attributes[key] := #f;
end;



//// XML/HTML

define table $html-quote-map
  = { '<' => "&lt;",
      '>' => "&gt;",
      '&' => "&amp;",
      '"' => "&quot;"
      };

// I'm sure this could use a lot of optimization.
define function quote-html
    (text :: <string>, #key stream)
  if (~stream)
    with-output-to-string (s)
      quote-html(text, stream: s)
    end
  else
    for (char in text)
      let translation = element($html-quote-map, char, default: char);
      iff(instance?(translation, <sequence>),
          write(stream, translation),
          write-element(stream, translation));
    end;
  end;
end quote-html;



