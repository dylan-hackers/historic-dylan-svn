module: slot-visitor

// See README.txt for documentation.

define macro slot-visitor-definer
   { define collection-recursive slot-visitor ?:name ?classes:* end }
   => {
         define slot-visitor ?name ?classes end;

         define method ?name
            (col :: <collection>, action :: <function>, #rest keys, #key, #all-keys)
         => ()
            remove-property!(keys, #"setter");
            for (o keyed-by i in col)
               apply(?name, o, action, setter:, rcurry(element-setter, col, i),
                     keys)
            end for;
         end method;
         
         define method ?name ## "-slots"
            (col :: <collection>, action :: <function>, #key, #all-keys)
         => ()
         end method
      }
   
   { define slot-visitor ?:name ?classes:* end }
   => {
         define generic ?name
            (o :: <object>, f :: <function>, #key, #all-keys)
         => ();

         define method ?name
            (o :: <object>, f :: <function>, #key, #all-keys)
         => ()
         end method;
         
         define generic ?name ## "-slots"
            (o :: <object>, f :: <function>, #key, #all-keys)
         => ();

         define method ?name ## "-slots"
            (o :: <object>, f :: <function>, #key, #all-keys)
         => ()
         end method;

         class-visitors(?name; ?classes)
      }
end macro;


define macro class-visitors
   { class-visitors(?:name) }
   => { }
   
   { class-visitors(?:name; ?class-name:name, ?slots; ?more:*) }
   => {
         define method ?name
            (object :: ?class-name, action :: <function>, #rest keys,
             #key setter, #all-keys)
         => ()
            remove-property!(keys, #"setter");
            let skip-slots? =
                  if (instance?(object, action.function-specializers.first))
                     ~ apply(action, object, setter:, setter, keys)
                  end if;
            unless (skip-slots?)
               apply(?name ## "-slots", object, action, keys)
            end unless;
         end method;
         
         define method ?name ## "-slots"
            (object :: ?class-name, action :: <function>, #next next-method,
             #rest keys, #key, #all-keys)
         => ()
            for (getter in getters-vector(?slots), setter in setters-vector(?slots))
               apply(?name, object.getter, action, setter:, setter & rcurry(setter, object),
                     keys)
            end for;
            next-method()
         end method;

         class-visitors(?name; ?more)
      }

slots:
   { constant ?:name, ... } => { constant ?name, ... }
   { ?:name, ... } => { ?name, ... }
   { } => { }
end macro;


define macro setters-vector
   { setters-vector(?slots) } => { vector(?slots) }

slots:
   { constant ?:name, ... } => { #f, ... }
   { ?:name, ... } => { ?name ## "-setter", ... }
   { } => { }
end macro;


define macro getters-vector
   { getters-vector(?slots) } => { vector(?slots) }

slots:
   { constant ?:name, ... } => { ?name, ... }
   { ?:name, ... } => { ?name, ... }
   { } => { }
end macro;
