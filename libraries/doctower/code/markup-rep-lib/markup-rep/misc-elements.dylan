module: markup-rep
synopsis: Placeholders that are replaced by actual elements after all is read.

define class <markup-element> (<source-location-mixin>)
end class;


define class <toc-map> (<object>)
   slot title :: <string>;
   slot shortdesc :: <string>;
   slot authors = make(<stretchy-vector>) /* of <string> */;
end class;

/// This is a DITA element, but I also use it as a container for
/// <target-placeholder> so that I can refer to the placeholder's location
/// after it has been resolved to a topic.
define class <topic-ref> (<markup-element>)
   slot target :: type-union(<topic>, <url>, <target-placeholder>),
      init-keyword: #"target";
end class;


/// Synopsis: Used when the target is unknown. May generally refer to a topic,
/// API, argument, etc., but not an URL.
define class <target-placeholder> (<markup-element>)
   slot target :: <string>, init-keyword: #"link";
end class;

define method \= (obj-1 :: <target-placeholder>, obj-2 :: <target-placeholder>)
=> (equal? :: <boolean>)
   case-insensitive-equal?(obj-1.target, obj-2.target)
end method;


define class <line-marker-placeholder> (<markup-element>)
   slot index :: type-union(<integer>, <character>), init-keyword: #"index";
end class;

define class <footnote-placeholder> (<markup-element>)
   slot index :: type-union(<integer>, <character>), init-keyword: #"index";
end class;
