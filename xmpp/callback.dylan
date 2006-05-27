module: xmpp
synopsis: 
author: 
copyright:

define class <callback> (<priority-queueable-mixin>)
  slot reference :: <symbol>,
    init-keyword: reference:;
  slot handler :: <function>,
    required-init-keyword: handler:;
  slot priority :: <integer>, 
    required-init-keyword: priority:;
end class <callback>;

define method \< (callback1 :: <callback>, callback2 :: <callback>)
 => (boolean :: <boolean>);
  callback1.priority < callback2.priority;
end method \<;
