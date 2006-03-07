Module: entity-manager


define open class <entity-manager> (<object>)
  constant slot storage-manager :: <storage-manager>,
    required-init-keyword: storage-manager:;
  constant slot entity-resolver-function :: false-or(<function>) = #f,
    init-keyword: entity-resolver-function:;
end class;
            
define open generic make-unparsed-entity
    (entity-manager :: <entity-manager>,
     #key public-id, system-id, relative-to)
 => (entity :: <entity>);
            
define open generic make-parsed-entity
    (entity-manager :: <entity-manager>,
     #key public-id, system-id, relative-to)
 => (entity :: <entity>);
            
define open abstract class <entity> (<object>)
  constant slot entity-storage :: <storage-entity>,
    required-init-keyword: storage-entity:;
  constant slot entity-public-id :: false-or(<string>) = #f,
    init-keyword: public-id:;
  constant slot entity-system-id :: false-or(<string>) = #f,
    init-keyword: system-id:;
  constant slot entity-generated-system-id :: false-or(<string>) = #f,
    init-constant: generated-system-id:;
end class;
            
define open generic entity-read
    (entity :: <entity>,
     consumer :: <entity-data-consumer>,
     #key asynchronous?)
 => ();
            
define open generic entity-cancel
    (entity :: <entity>)
 => ();
            
define open generic entity-close
    (entity :: <entity>)
 => ();
            
define open generic storage-entity-read
    (storage-entity :: <storage-entity>,
     entity :: <entity>,
     consumer :: <entity-data-consumer>,
     #key asynchronous?)
 => ();
            
define open generic storage-entity-cancel
    (storage-entity :: <storage-entity>)
 => ();
            
define open generic storage-entity-close
    (storage-entity :: <storage-entity>)
 => ();
            
define class <entity-implementation> (<entity>)
  constant slot entity-encoding :: false-or(<encoding>) = #f,
    init-keyword: encoding:;
end class;
            
define method make-parsed-entity
    (entity-manager :: <entity-manager>
     #key public-id :: false-or(<string>),
          system-id :: false-or(<string>),
          relative-to :: false-or(<entity>))
 => (entity :: <entity>);
  let generated-system-id
    = entity-manager.entity-resolver-function
      & entity-manager.entity-resolver-function(public-id,
                                                    system-id,
                                                    relative-to);
  let storage-entity
    = make-parsed-storage-entity(entity-manager.storage-manager,
                                   generated-system-id | system-id);
  make-entity(storage-entity, public-id, system-id, generated-system-id, #t);
end method;
            
define method make-unparsed-entity
    (entity-manager :: <entity-manager>
     #key public-id :: false-or(<string>),
          system-id :: false-or(<string>),
          relative-to :: false-or(<entity>))
 => (entity :: <entity>);
  let generated-system-id
    = entity-manager.entity-resolver-function
      & entity-manager.entity-resolver-function(public-id,
                                                    system-id,
                                                    relative-to);
  let storage-entity
    = make-unparsed-storage-entity(entity-manager.storage-manager,
                                   generated-system-id | system-id);
  make-entity(storage-entity, public-id, system-id, generated-system-id, #f);
end method;
            
define method make-entity
    (storage-entity :: <storage-entity>
     public-id :: false-or(<string>),
     system-id :: false-or(<string>),
     generated-system-id :: false-or(<string>),
     parsed? :: <boolean>)
 => (entity :: <entity>);
  let encoding-name = storage-entity-attribute(storage-entity, #"encoding");
  let encoding = encoding-name & make-encoding(encoding-name);
  make(<entity-implementation>,
       storage-entity: storage-entity,
       public-id: public-id,
       system-id: system-id,
       generated-system-id: generated-system-id,
       encoding: encoding);
end method;
            
define class <entity-data-decoder> (<entity-data-consumer>)
  constant slot decoder-consumer :: <entity-data-consumer>,
    required-init-keyword: consumer:;
  constant slot decoder-encoding :: <encoding>,
    required-init-keyword: encoding:;
  constant slot decoder-buffer :: <entity-data-buffer>,
    required-init-keyword: buffer:;
end class;
            
define method entity-read
    (entity :: <entity-implementation>,
     consumer :: <entity-data-consumer>,
     #key asynchronous? :: <boolean> = #f) => ();

  if(entity.entity-encoding)
    let buffer-size
       = decode-buffer-size
           (entity.entity-encoding,
            storage-entity-maximum-data-size(entity.storage-entity));
    let decoder = make(<entity-data-decoder>,
                       consumer: consumer,
                       encoding: entity.entity-encoding,
                       buffer: make(<entity-data-buffer>,
                                    size: buffer-size);
    storage-entity-read(entity.storage-entity, entity, decoder,
                        asynchronous?: asynchronous?);
  else
    storage-entity-read(entity.storage-entity, entity, consumer,
                        asynchronous?: asynchronous?);
end method;
            
define method entity-data-event
    (decoder :: <entity-data-decoder>,
     entity :: <entity>,
     data :: <entity-data-buffer>,
     start-index :: <integer>,
     end-index :: <integer>)
 => (pushback-size :: <integer>);
  let (decoded-size :: <integer>, undecoded-size :: <integer>)
    = decode-characters(decoder.decoder-encoding,
                        decoder.decoder-buffer,
                        data,
                        start: start-index,
                        end: end-index,
                        offset: decoder.decoder-pushback-elements);
  let consumer-pushback-size :: <integer>
    = entity-data-event(decoder.decoder-consumer, entity,
                        decoder.decoder-buffer, 0, decoded-size);

  for(i from 0 below consumer-pushback-size,
      j from decoded-size - consumer-pushback-size below decoded-size)
    decoder.decoder-buffer[i] := decoder.decoder-buffer[j];
  end for;
  decoder.decoder-pushback-elements := consumer-pushback-size;

  undecoded-size;  
end method;
            
define method entity-end-event
    (decoder :: <entity-data-decoder>,
     entity :: <entity>)
 => ();
  if(decoder.decoder-pushback-elements = 0)
    entity-end-event(decoder.decoder-consumer, entity);
  else
    entity-error-event(decoder.decoder-consumer, entity,
                       make(<simple-error>,
                            format-string: "Characters pushed back at end of entity",
                            format-arguments: #()));
  end if;
end method;
            
define method entity-error-event
    (decoder :: <entity-data-decoder>,
     entity :: <entity>,
     err :: <error>)
 => ();
  entity-error-event(decoder.decoder-consumer, entity, err);
end method;
            
define method entity-data-maximum-pushback
    (decoder :: <entity-data-decoder>)
 => (maximum-pushback :: <integer>);
  encode-buffer-size(decoder.decoder-encoding, 1) - 1;
end method;
            
define method entity-cancel
    (entity :: <entity-implementation>) => ();
  storage-entity-cancel(entity.storage-entity);
end method;
            
define method entity-close
    (entity :: <entity-implementation>) => ();
  storage-entity-close(entity.storage-entity);
end method;
            
