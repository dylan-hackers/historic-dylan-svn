Module: storage-manager


define open abstract class <storage-manager> (<object>)
  
end class;
            
define class <xml-storage-manager> (<storage-manager>)

end class;
            
define class <fsi-storage-manager> (<storage-manager>)

end class;
            
define class <uri-storage-manager> (<storage-manager>)

end class;
            
define class <osfile-storage-manager> (<storage-manager>)

end class;
            
define open generic make-unparsed-storage-entity
    (entity-manager :: <storage-manager>,
     system-id :: <string>)
 => (storage-entity :: <storage-entity>);
            
define open generic make-parsed-storage-entity
    (entity-manager :: <storage-manager>,
     system-id :: <string>)
 => (storage-entity :: <storage-entity>);
            
define open abstract class <storage-entity> (<object>)

end class;
            
define open generic storage-entity-attribute
    (storage-entity :: <storage-entity>,
     attribute :: <symbol>)
 => (value :: <object>);
            
define open generic storage-entity-maximum-data-size
    (storage-entity :: <storage-entity>)
 => (read-size :: <integer>);
            
define class <osfile-storage-entity> (<storage-entity>)
  constant slot osfile-storage-entity-stream :: <stream>,
    required-init-keyword: stream:;
end class;
            
define method make-unparsed-storage-entity
    (entity-manager :: <osfile-storage-manager>,
     system-id :: <string>)
 => (storage-entity :: <osfile-storage-entity>);
  let stream = make(<file-stream>, locator: system-id);
  make(<osfile-storage-entity>, stream: stream);
end method;
            
define method make-parsed-storage-entity
    (entity-manager :: <osfile-storage-manager>,
     system-id :: <string>)
 => (storage-entity :: <osfile-storage-entity>);
  let stream = make(<file-stream>, locator: system-id);
  make(<osfile-storage-entity>, stream: stream);
end method;
            
define method storage-entity-attribute
    (storage-entity :: <osfile-storage-entity>,
     attribute :: <symbol>)
 => (value :: <object>);
  #f;
end method;
            
define constant $osfile-maximum-data-size = 8192;
            
define method storage-entity-maximum-data-size
    (storage-entity :: <osfile-storage-entity>)
 => (buffer-size :: <integer>);
  $osfile-maximum-data-size;
end method;
            
