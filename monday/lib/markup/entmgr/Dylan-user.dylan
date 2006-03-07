Module: Dylan-user


define library entity-manager
  use Common-Dylan;

  
use streams;
            

  export entity-manager,
         storage-manager,
         encoding-manager;
end library;
            
define module entity-data-consumer
  use Dylan;

  export <entity-data-consumer>,
         <entity-data-buffer>,
         entity-data-protocol;
  create <entity>;
end module;
            
define module entity-manager
  use Common-Dylan;

  use entity-data-consumer, export: all;
  use encoding-manager;
  use storage-manager;

  export <entity-manager>,
         make-unparsed-entity,
         make-parsed-entity,
         entity-public-id,
         entity-system-id,
         entity-generated-system-id,
         entity-read,
         entity-cancel,
         entity-close;
end module;
            
define module storage-manager
  use Common-Dylan;

  use entity-data-consumer;

  
use streams;
            

  export <storage-manager>,
         <xml-storage-manager>,
	 <fsi-storage-manager>,
         <uri-storage-manager>,
         <osfile-storage-manager>,
	 make-unparsed-storage-entity,
         make-parsed-storage-entity,
	 storage-entity-attribute,
         storage-entity-maximum-data-size,
         storage-entity-read,
         storage-entity-cancel,
         storage-entity-close;
end module;
            
define module encoding-manager
  use Common-Dylan;

  
          

  export <encoding>,
         <single-byte-encoding>,
         single-byte-encoding-table,
         decode-buffer-size,
         encode-buffer-size,
         decode-characters,
         encode-characters,
         make-encoding,
         register-encoding;
end module;
            
