Module:    http-common-internals
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
  constant slot attributes :: <mutable-explicit-key-collection>
    = make(<string-table>),
    init-keyword: attributes:;
end;

define generic has-attribute?
    (this :: <attributes-mixin>, key :: <string>) => (has-it? :: <boolean>);

define generic get-attribute
    (this :: <attributes-mixin>, key :: <string>, #key)
 => (value :: <object>);

define generic set-attribute
    (this :: <attributes-mixin>, key :: <string>, value :: <object>);

define generic remove-attribute
    (this :: <attributes-mixin>, key :: <string>);


define method has-attribute?
    (this :: <attributes-mixin>, key :: <string>)
 => (has-it? :: <boolean>)
  has-key?(this.attributes, key)
end;

define method get-attribute
    (this :: <attributes-mixin>, key :: <string>, #key default)
 => (attribute :: <object>)
  element(this.attributes, key, default: default)
end;

define method set-attribute
    (this :: <attributes-mixin>, key :: <string>, value :: <object>)
  this.attributes[key] := value;
end;

define method remove-attribute
    (this :: <attributes-mixin>, key :: <string>)
  remove-key!(this.attributes, key);
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


// A media type is a MIME type plus some parameters.  The type and subtype
// may be wild (i.e., "*") for the Accept header but should not be for the
// content-type header.  The two well-known parameters, quality (q) and
// level are converted to <float> and <integer> respectively.  The rest are
// left as strings.
//
// Note that <attributes-mixin> uses <string-table>, which is case sensitive.
// RFC 2616, 3.7 specifies that media-type parameter names are not case
// sensitive.  For now we rely on parse-media-type to lowercase the parameter
// names (and the type and subtype).
define class <media-type> (<attributes-mixin>, <mime-type>)
end;

define constant $mime-wild :: <byte-string> = "*";

// This method isn't in the MIME library because wildcards for type and
// subtype aren't supported by MIME; only by HTTP.
//
define method mime-types-match?
    (type1 :: <mime-type>, type2 :: <mime-type>)
 => (match? :: <boolean>)
  (type1.mime-type = type2.mime-type
     | type1.mime-type = $mime-wild
     | type2.mime-type = $mime-wild)
    & (type1.mime-subtype = type2.mime-subtype
         | type1.mime-subtype = $mime-wild
         | type2.mime-subtype = $mime-wild)
end;

// This method returns #t if type1 is more specific than type2.
// A media type is considered more specific than another if it doesn't
// have a wildcard component and the other one does, or if it has more
// parameters (excluding "q").  See RFC 2616, 14.1.
//
define method media-type-more-specific?
    (type1 :: <media-type>, type2 :: <media-type>)
 => (more-specific? :: <boolean>)
  local method has-more-params? ()
          let nparams1 = type1.attributes.size - iff(has-attribute?(type1, "q"), 1, 0);
          let nparams2 = type2.attributes.size - iff(has-attribute?(type2, "q"), 1, 0);
          nparams1 > nparams2
        end;
  if (type1.mime-type = type2.mime-type)
    (type1.mime-subtype ~= $mime-wild & type2.mime-subtype = $mime-wild)
      | has-more-params?()
  else
    (type1.mime-type ~= $mime-wild & type2.mime-type = $mime-wild)
      | has-more-params?()
  end
end method media-type-more-specific?;

define method media-type-exact?
    (mr :: <media-type>) => (exact? :: <boolean>)
  mr.mime-type ~= $mime-wild & mr.mime-subtype ~= $mime-wild
end;

// Common case
define method media-type-quality
    (media-type :: <media-type>) => (q :: <float>)
  get-attribute(media-type, "q") | 1.0
end;

// Common case
define method media-type-level
    (media-type :: <media-type>) => (level :: false-or(<integer>))
  get-attribute(media-type, "level")
end;

