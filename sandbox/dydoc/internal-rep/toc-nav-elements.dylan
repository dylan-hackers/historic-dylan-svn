module: internal-rep
synopsis: Classes comprising the table-of-contents and navigation map.

define class <toc-map> (<object>)
   slot title :: <string>;
   slot shortdesc;
   slot authors :: <stretchy-vector>;
end class;

define class <topic-ref> (<object>)
   slot navtitle :: <string>;
   slot target :: false-or(type-union(<topic>, <url>, <target-placeholder>));
   slot automatic-default? :: <boolean>, required-init-keyword: #"auto";
end class;
