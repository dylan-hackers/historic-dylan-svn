Module: entity-data-consumer


define abstract open free class <entity-data-consumer> (<object>)
  // no slots
end class;
                
define constant <entity-data-buffer>
  = limited(<sequence>, of: <integer>);
                
define open generic entity-data-protocol
    (consumer :: <entity-data-consumer>)
 => (data-function :: <function>,
     end-function :: <function>,
     error-function :: <function>);
                
define open generic entity-data-event
    (consumer :: <entity-data-consumer>,
     entity :: <entity>,
     data :: <entity-data-buffer>,
     start-index :: <integer>,
     end-index :: <integer>,
     start-octet-position :: <integer>)
 => ();
            
define open generic entity-end-event
    (consumer :: <entity-data-consumer>,
     entity :: <entity>)
 => ();
            
define open generic entity-error-event
    (consumer :: <entity-data-consumer>,
     entity :: <entity>,
     err :: <error>)
 => ();
            
