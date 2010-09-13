module: common


define variable $verbose? :: <boolean> = #t;

define inline function verbose-log (str :: <string>, #rest args)
   when ($verbose?) apply(log, str, args) end;
end function;


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


/**
Synopsis: Divides a collection into several collections, each containing related
elements.

Compares elements in the collection to each other using a matching function.
Elements that match are placed together in a newly-allocated collection, and all
such collections are returned.

[EXAMPLE]
group-elements(#[1, 2, 2, 3, 4, 3])
⇒ #[#[1], #[2, 2], #[3, 3], #[4]]
[END EXAMPLE]

--- Arguments: ---
collection  - An instance of <collection>.
test:       - An instance of <function> taking two arguments. If the arguments
              should be classified together, the function should return true.
              Optional; defaults to \==.

--- Values: ---
categories  - An instance of <sequence>. Each element of the sequence is of the
              'type-for-copy' of 'collection' and contains one or more elements
              of 'collection'.
**/
define generic group-elements
   (collection :: <collection>, #key test :: <function> = \==)
=> (categories :: <sequence>);

define method group-elements
   (collection :: <table>, #key test :: <function> = \==)
=> (categories :: <sequence>)
   let categories = list();
   for (elem keyed-by key in collection)
      block (matched)
         // See if an existing category fits.
         for (categ in categories)
            if (test(categ.any-element, elem))
               categ[key] := elem;
               matched();
            end if;
         end for;
         // If not, make a new one.
         let categ = table(collection.type-for-copy, key => elem);
         categories := add!(categories, categ);
      end block;
   end for;
   categories;
end method;

define method group-elements
   (collection :: <sequence>, #key test :: <function> = \==)
=> (categories :: <sequence>)
   let categories = make(<stretchy-vector>);
   for (elem in collection)
      block (matched)
         // See if an existing category fits.
         for (categ in categories)
            if (test(categ.any-element, elem))
               // Each categ will be a <stretchy-vector>, so add! is okay.
               add!(categ, elem);
               matched();
            end if;
         end for;
         // If not, make a new one.
         let categ = add!(make(<stretchy-vector>), elem);
         categories := add!(categories, categ);
      end block;
   end for;
   // Fix type of each category's collection.
   let categ-collection = curry(map-as, collection.type-for-copy, identity);
   map(categ-collection, categories);
end method;


define method add-row (array :: <array>, #key count :: <integer> = 1)
=> (new-array :: <array>)
   let new-dimensions = array.dimensions.shallow-copy;
   new-dimensions[0] := new-dimensions[0] + count;
   let new-array = make(<array>, dimensions: new-dimensions);
   map-into(new-array, identity, array)
end method;


/**
Synopsis: Returns the first or any element of the collection.

Calling this function is more efficient than casting to a sequence and taking
the first element.
**/
define generic any-element (collection :: <collection>, #key default) => (elem);

define method any-element (collection :: <collection>, #key default = unsupplied())
=> (element)
   let (initial-state, limit, next-state, finished-state?, current-key, current-element) =
         collection.forward-iteration-protocol;
   if (finished-state?(collection, initial-state, limit))
      if (default.unsupplied?)
         error("Collection is empty");
      else
         default
      end if
   else
      current-element(collection, initial-state)
   end if
end method;
   

define class <case-insensitive-skip-list> (<skip-list>)
   keyword test: = case-insensitive-equal?;
   keyword key-order: = case-insensitive-less?;
end class;


define method case-insensitive-less? (str1 :: <string>, str2 :: <string>)
=> (less? :: <boolean>)
   as-lowercase(str1) < as-lowercase(str2)
end method;
