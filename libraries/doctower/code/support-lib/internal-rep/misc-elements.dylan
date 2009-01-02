module: internal-rep
synopsis: Placeholders that are replaced by actual elements after all is read.

define class <interm-element> (<source-location-mixin>)
end class;

/// Synopsis: Used when the target is unknown. May generally refer to a topic,
/// API, argument, etc.
define class <target-placeholder> (<interm-element>)
   slot target :: <string>, init-keyword: #"link";
end class;

define method \= (obj-1 :: <target-placeholder>, obj-2 :: <target-placeholder>)
=> (equal? :: <boolean>)
   case-insensitive-equal?(obj-1.target, obj-2.target)
end method;

define class <api-list-placeholder> (<interm-element>)
   slot type :: <symbol>;
   slot scope;
end class;

define class <ditto-placeholder> (<interm-element>)
   slot target :: type-union(<topic>, <target-placeholder>),
      init-keyword: #"target";
end class;

define class <line-marker-placeholder> (<interm-element>)
   slot index :: type-union(<integer>, <character>), init-keyword: #"index";
end class;

define class <footnote-placeholder> (<interm-element>)
   slot index :: type-union(<integer>, <character>), init-keyword: #"index";
end class;

/// Synopsis: Placeholder for contents of current topic, i.e. "[CONTENTS]".
define class <toc-placeholder> (<interm-element>)
end class;
