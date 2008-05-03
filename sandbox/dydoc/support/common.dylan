module: common

define function log (str :: <string>, #rest args)
   apply(format, *standard-output*, str, args);
   write-element(*standard-output*, '\n');
   force-output(*standard-output*);
end function;


define function log-object (label :: <string>, obj)
   format(*standard-output*, "--- %s ---\n", label);
   print(obj, *standard-output*, pretty?: #t);
   write(*standard-output*, "\n---\n");
   force-output(*standard-output*);
end function;


define macro slot-visitor-definer
   { define collection-recursive slot-visitor ?:name ?classes:* end }
   => {
         define method ?name (collection :: <collection>, operation :: <function>)
            do(rcurry(?name, operation), collection);
         end method;

         define method ?name (collection :: <object>, operation :: <function>)
         end method;

         define slot-visitor ?name ?classes end;
      }
   
   { define slot-visitor ?:name ?classes:* end }
   => { class-visitors(?name; ?classes) }
end macro;


define macro class-visitors
   { class-visitors(?:name) }
   => { }
   
   { class-visitors(?:name; ?class-name:name, ?slots; ?more:*) }
   => {
         define method ?name (object :: ?class-name, operation :: <function>)
            for (slot in vector(?slots))
               ?name(object.slot, operation)
            end for;
            operation(object);
         end method;
         
         class-visitors(?name; ?more)
      }

slots:
   { ?:name, ... } => { ?name, ... }
   { } => { }
end macro;
