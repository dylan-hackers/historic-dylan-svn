module: internal-rep
synopsis: Classes comprising the table-of-contents and navigation map.

define class <toc-map> (<object>)
   slot title :: <string>;
   slot shortdesc;
   constant slot authors = make(<stretchy-vector>);
end class;

define class <topic-ref> (<object>)
   slot navtitle :: <string>;
   slot target :: false-or(type-union(<topic>, <url>, <target-placeholder>));
   slot automatic-default? :: <boolean> = #f;
end class;
