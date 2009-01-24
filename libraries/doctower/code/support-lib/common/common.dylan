module: common

define function log (str :: <string>, #rest args)
   apply(format, *standard-output*, str, args);
   write-element(*standard-output*, '\n');
   force-output(*standard-output*);
end function;


define function log-object (label :: <string>, obj)
   format(*standard-output*, "--- %s ---\n", label);
   print(obj, *standard-output*, pretty?: #t);
   write-element(*standard-output*, '\n');
   force-output(*standard-output*);
end function;


define function item-string-list (items :: <collection>)
=> (str :: false-or(<string>))
   if (items.empty?)
      #f
   else
      let item-strings = map(curry(format-to-string, "%s"), items);
      apply(join, ", ", item-strings);
   end if
end function;


define macro with-open-file
   {  with-open-file (?:name = ?locator:expression, #rest ?keys:expression)
         ?:body
      end }
   => {  block ()
            let ?name = make(<file-stream>, locator: ?locator, ?keys);
            block ()
               ?body
            cleanup
               close(?name)
            end block
         exception (err :: <file-does-not-exist-error>)
            file-not-found(#f, filename: ?locator)
         exception (err :: <file-system-error>)
            file-error(#f, filename: ?locator, error: condition-to-string(err))
         end block }
end macro;


/// Synopsis: Define methods to visit objects and their slots.
///
/// Use as follows, where braces indicate optional items:
/// : define {collection-recursive} slot-visitor NAME
/// :   CLASS {, SLOT}... ;
/// :   {CLASS {, SLOT}... ;}...
/// : end {slot-visitor {NAME}}
///
/// This defines a set of visitor methods called NAME. The methods have this
/// signature:
/// : NAME (object :: CLASS, action :: <function>, #key KEY, ...) => ()
/// 
/// The method visits all SLOTS then the object itself, performing the action.
/// The action should have this signature:
/// : (object {:: TYPE}, #key setter, KEY, ...) => ()
///
/// The action is called on the object or slot value. If the TYPE doesn't match,
/// the action is not performed. The setter key is a function by which the action
/// can replace replace the object or slot value. ALL-KEYS passed to each NAME
/// function is also passed to the ACTION.
///
define macro slot-visitor-definer
   { define collection-recursive slot-visitor ?:name ?classes:* end }
   => {
         define method ?name
            (col :: <collection>, f :: <function>, #rest keys, #key, #all-keys)
         => ()
            for (o keyed-by i in col)
               apply(?name, o, f, #"setter", rcurry(element-setter, col, i),
                     keys);
            end for;
         end method;

         define method ?name
            (o :: <object>, f :: <function>, #key, #all-keys)
         => ()
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
         define method ?name
            (object :: ?class-name, action :: <function>, #rest keys, #key, #all-keys)
         => ()
            for (slot in vector(?slots),
                 setter in setters-vector(?slots))
               apply(?name, object.slot, action,
                     #"setter", rcurry(setter, object), keys)
            end for;

            when (instance?(object, action.function-specializers.first))
               apply(action, object, keys);
            end when;
         end method;
         
         class-visitors(?name; ?more)
      }

slots:
   { ?:name, ... } => { ?name, ... }
   { } => { }
end macro;


define macro setters-vector
   { setters-vector(?getters) } => { vector(?getters) }
   { setters-vector() } => { }

getters:
   { ?:name, ... } => { ?name ## "-setter", ... }
   { } => { }
end macro;
