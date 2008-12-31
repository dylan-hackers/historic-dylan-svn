module: internal-rep
synopsis: Classes comprising the table-of-contents and navigation map.

define class <toc-map> (<object>)
   slot title :: <string>;
   slot shortdesc :: <string>;
   slot authors = make(<stretchy-vector>) /* of <string> */;
end class;

/// This is a DITA element, but I also use it as a container for
/// <target-placeholder> so that I can refer to the placeholder's location
/// after it has been resolved to a topic.
define class <topic-ref> (<interm-element>)
   slot target :: type-union(<topic>, <url>, <target-placeholder>),
      init-keyword: #"target";
end class;
