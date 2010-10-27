module: template-engine
synopsis: Stores or retrieves parsed template from disk.


/**
=== Persistent Format ===

The persistent format of template information is a format version byte, then a
reference to the root persisted object, then a series of persisted objects.
Each object is recorded once by identity and follows all other objects that it
references.

- A tag is a <byte>.
- A size is a <machine-word>.
- A reference is a <machine-word> byte offset from the start of the vector.
- A persisted object is a tag and a payload. The payload is literal data for the
  <boolean>, <character>, or <integer> types, and a series of references for
  other objects.
- A <sequence> payload is a size followed by element references.
- A <boolean> payload is a byte where 0 is #f and 1 is #t.
- A Unicode <string> payload is a size followed by a series of characters.
- A <byte-string> payload is a size followed by a series of bytes.
- A <character> payload is a <machine-word>.
- An <integer> payload is a <machine-word>.
- Other objects' payloads are a series of references, one for each slot.
- The format version byte has this structure:
   > [7   6   5   4   3   2   1   0]
   > [.....version.....   word-size]

The persistent data is intended as a cache, not as a machine-independent and
portable data file. If the version byte is not as expected, the data should be
discarded.
*/

define constant $format-version :: limited(<integer>, min: 0, max: 31)
		= 0;

define constant $machine-word-bytes :: limited(<integer>, min: 1, max: 8)
		= ceiling/($machine-word-size, 8);

define constant $format-version-byte :: <byte>
		= logior(ash($format-version, 3),
		         ash($machine-word-bytes - 1, 0));

define constant $reference-size = $machine-word-bytes;
define constant $size-size = $machine-word-bytes;
define constant $tag-size = 1;
define constant $objects-start = 1 + $reference-size;
define constant $payload-start = $tag-size;


// If these tags are altered, bump $format-version.
define constant $tags = vector(
   // dylan
   <sequence>,                   // 0
   <boolean>,
   <integer>,
   <character>,
   <byte-string>,
   <unicode-string>,             // 5
   
   // base-grammar
   // <lex-exclamation-token>,   // only used while parsing
   // <lex-period-token>,        // only used while parsing
   // <lex-comma-token>,         // only used while parsing
   <lex-not-token>,              // 6
   <lex-ampersand-token>,
   <lex-vert-bar-token>,
   <lex-plus-token>,
   <lex-minus-token>,
   <lex-star-token>,
   <lex-slash-token>,
   <lex-percent-token>,
   <lex-equal-token>,
   <lex-not-equal-token>,
   <lex-lte-token>,
   <lex-gte-token>,
   <lex-lf-angle-token>,
   <lex-rt-angle-token>,         // 19
   // <lex-lf-paren-token>,      // implicit in expression tree
   // <lex-rt-paren-token>,      // implicit in expression tree
   // <lex-lf-brack-token>,      // only used while parsing
   // <lex-rt-brack-token>,      // only used while parsing
   // <lex-lf-brace-token>,      // only used while parsing
   // <lex-rt-brace-token>,      // only used while parsing
   // <lex-lf-parens-token>,     // only used while parsing
   // <lex-rt-parens-token>,     // only used while parsing
   // <lex-lf-angles-token>,     // only used while parsing
   // <lex-rt-angles-token>,     // only used while parsing
   // <lex-lf-bracks-token>,     // only used while parsing
   // <lex-rt-bracks-token>,     // only used while parsing
   // <lex-lf-braces-token>,     // only used while parsing
   // <lex-rt-braces-token>,     // only used while parsing
   // <lex-lf-comment-token>,    // only used while parsing
   // <lex-rt-comment-token>,    // only used while parsing
   <lex-case-token>,             // 20
   <lex-else-token>,
   <lex-end-token>,              // 22
   // <lex-force-token>,         // unused
   <lex-if-token>,               // 23
   // <lex-in-token>,            // only used while parsing
   // <lex-info-token>,          // only used while parsing
   // <lex-operation-token>,     // only used while parsing
   <lex-repeat-token>,           // 24
   <lex-unless-token>,
   <lex-when-token>,
   <lex-with-token>,
   <lex-name-token>,
   <lex-number-token>,
   <lex-hex-number-token>,
   <lex-true-token>,
   <lex-false-token>,            // 32
   
   // directive-grammar
   <case-directive-block-token>, // 33
   <if-directive-block-token>,
   <repeat-directive-block-token>,
   <with-directive-block-token>,
   <case-directive-token>,
   <if-directive-token>,
   <repeat-directive-token>,
   <with-directive-token>,
   <simple-directive-token>,
   <else-directive-token>,
   <end-directive-token>,
   <empty-directive-token>,      // 44
   
   // dirspec-grammar
   <case-dirspec-token>,         // 45
   <if-dirspec-token>,
   <repeat-dirspec-token>,
   <with-dirspec-token>,
   <assignment-token>,
   <simple-dirspec-token>,
   <else-dirspec-token>,
   <end-dirspec-token>,
   <binary-expression-token>,    // 53
   // <binary-tier-1-expression-token>,   // implicit in expression tree
   // <binary-tier-2-expression-token>,   // implicit in expression tree
   // <binary-tier-3-expression-token>,   // implicit in expression tree
   // <binary-tier-4-expression-token>,   // implicit in expression tree
   <binary-operand-token>,       // 54
   <operand-token>,
   <var-name-token>,
   <chained-call-token>,
   <string-token>,               // 58
   // <substring-token>,         // only used while parsing
   <character-token>,            // 59
   
   // template-grammar
   <template-token>,             // 60
   <directive-prolog-token>,
   <directive-epilog-token>      // 62
);


define slot-visitor visit-objects
   // dylan
   <sequence>                       ;                    
   <boolean>                        ;
   <integer>                        ;
   <character>                      ;
   <string>                         ;
   
   // streams
   <stream-position>                ;

   // peg-parser
   <token>,                         parse-start, parse-end;

   // base-grammar
   <lexeme>,                        text /* parse-start, parse-end */;
   <value>                          /* text, const-value?, value, parse-start, parse-end */;

   // directive-grammar
   <case-directive-block-token>,    case-pairs, else-pair, end-directive /* parse-start, parse-end */;
   <if-directive-block-token>,      if-pair, else-pair, end-directive /* parse-start, parse-end */;
   <repeat-directive-block-token>,  block-pair, end-directive /* parse-start, parse-end */;
   <with-directive-block-token>,    block-pair, end-directive /* parse-start, parse-end */;
   <delimited-directive-token>,     left-delimiter, right-delimiter, directive-specifier /* parse-start, parse-end */;

   // dirspec-grammar
   <dirspec-token>,                 dirspec-lexeme /* parse-start, parse-end */;
   <test-dirspec-token>,            test-expression /* dirspec-lexeme, parse-start, parse-end */;
   <expression-token>,              const-value?, value /* parse-start, parse-end */;
   <repeat-dirspec-token>,          loop-item-name, loop-info-name, collection-expression /* dirspec-lexeme, parse-start, parse-end */;
   <with-dirspec-token>,            assignments /* dirspec-lexeme, parse-start, parse-end */;
   <assignment-token>,              operation?, name, value-token /* parse-start, parse-end */;
   <simple-dirspec-token>,          expression /* dirspec-lexeme, parse-start, parse-end */;
   <binary-expression-token>,       left-operand, operator, right-operand /* const-value?, value, parse-start, parse-end */;
   <binary-operand-token>,          operator, operand /* const-value?, value, parse-start, parse-end */;
   <operand-token>,                 base-value, chained-calls /* const-value?, value, parse-start, parse-end */;
   <var-name-token>,                name /* const-value?, value, parse-start, parse-end */;
   <chained-call-token>,            name /* parse-start, parse-end */;

   // template-grammar
   <template-token>,                contents /* parse-start, parse-end */;
   <directive-epilog-token>,        raw-output? /* parse-start, parse-end */;
end slot-visitor;


define constant $payload-sizes = vector(
   // dylan
   vector(<boolean>,                      1),
   vector(<integer>,                      $machine-word-bytes),
   vector(<character>,                    $machine-word-bytes),
                                     
   // streams                        
   vector(<stream-position>,              $machine-word-bytes),
                                     
   // peg-parser                     
   vector(<token>,                        $reference-size * 2),
                                     
   // base-grammar                   
   vector(<lexeme>,                       $reference-size * 3),
   vector(<value>,                        $reference-size * 5),
                                     
   // directive-grammar              
   vector(<case-directive-block-token>,   $reference-size * 5),
   vector(<if-directive-block-token>,     $reference-size * 5),
   vector(<repeat-directive-block-token>, $reference-size * 4),
   vector(<with-directive-block-token>,   $reference-size * 4),
   vector(<delimited-directive-token>,    $reference-size * 5),
                                    
   // dirspec-grammar               
   vector(<dirspec-token>,                $reference-size * 3),
   vector(<test-dirspec-token>,           $reference-size * 4),
   vector(<expression-token>,             $reference-size * 4),
   vector(<repeat-dirspec-token>,         $reference-size * 6),
   vector(<with-dirspec-token>,           $reference-size * 4),
   vector(<assignment-token>,             $reference-size * 5),
   vector(<simple-dirspec-token>,         $reference-size * 4),
   vector(<binary-expression-token>,      $reference-size * 7),
   vector(<binary-operand-token>,         $reference-size * 6),
   vector(<operand-token>,                $reference-size * 6),
   vector(<var-name-token>,               $reference-size * 5),
   vector(<chained-call-token>,           $reference-size * 3),
                                    
   // template-grammar              
   vector(<template-token>,               $reference-size * 3),
   vector(<directive-epilog-token>,       $reference-size * 3)
);


/**
Synopsis: Returns a <byte-vector> representing the persistable part of the
current template object, which may be used to initialize a new <template>
instance.
*/
define method persistable-template (template :: <template>)
=> (bytes :: <byte-vector>)
   // This assertion is because processed templates can have the value slot in
   // <expression-token> set to arbitrary objects, which we do not handle.
   assert(~template.processed?, "Cannot persist processed template.");
   
   let references = make(<table>);     // The index of the byte block for an object.
   let dependencies = make(<table>);   // Objects required to instantiate the object.
   let byte-blocks = make(<stretchy-vector>);
   
   // Collect references and preallocate byte blocks.
   visit-objects(template.parsed-template, catalog-object, references: references,
         dependencies: dependencies, byte-blocks: byte-blocks);
         
   // Calculate order of byte-blocks so that all objects are defined before they
   // are required.
   let sorted-objects = order-by-dependency(dependencies);
   let sorted-indices = map(curry(element, references), sorted-objects);

   // Compute offset of each byte block.
   let block-offsets = make(<vector>, size: byte-blocks.size);
   for (index in sorted-indices,
         offset = $objects-start then offset + byte-blocks[index].size)
      block-offsets[index] := offset
   end for;

	// Fill in byte blocks and forward references.
	do(rcurry(persist-object, references:, references, byte-blocks:, byte-blocks,
	          block-offsets:, block-offsets),
	   references.key-sequence);

   // Make <byte-vector> from header and consolidated byte blocks.
   let byte-count = reduce(\+, $objects-start, map(size, byte-blocks));
   let bytes = make(<byte-vector>, size: byte-count);
   bytes[0] := $format-version-byte;
   replace-bytes!(bytes, 1,
         reference-offset(template.parsed-template, references, block-offsets));
   for (byte-block in byte-blocks, offset in block-offsets)
      replace-bytes!(bytes, offset, byte-block)
   end for;
   
   bytes
end method;


/**
Synopsis: Returns whether the template can be restored from the given persisted
template data.
*/
define method restorable? (bytes :: <byte-vector>) => (restorable? :: <boolean>)
   bytes[0] = $format-version-byte
end method;


/**
Synopsis: Instantiates all objects from a byte-vector containing persisted
objects and returns the root object.
*/
define method restore-template (bytes :: <byte-vector>)
=> (object)
   let root-offset = as(<integer>, machine-word(bytes, 1));
   let offset-objects = make(<table>);
   let offset = $objects-start;
   while (offset < bytes.size)
      let byte-block = extract-byte-block(bytes, offset);
      offset-objects[offset] := make-from-byte-block(byte-block, offset-objects);
      offset := offset + byte-block.size;
   end while;
   offset-objects[root-offset]
end method;


define method order-by-dependency (dependencies :: <table>) => (objects :: <sequence>)
   let added = make(<table>);
   local method add-deps-then-object (dep-list :: <list>, object)
         => (dep-list :: <list>)
            unless (element(added, object, default: #f))
               added[object] := #t;
               let obj-dependencies = dependencies[object];
               for (dep in obj-dependencies)
                  dep-list := add-deps-then-object(dep-list, dep)
               end for;
               dep-list := add!(dep-list, object);
            end unless;
            dep-list
         end method;

   let dependency-list = list();
   for (object in dependencies.key-sequence)
      dependency-list := add-deps-then-object(dependency-list, object)
   end for;
   dependency-list.reverse!
end method;


define method replace-bytes!
   (bytes :: <byte-vector>, byte-index :: <integer>, value :: <byte>)
=> (new-index :: <integer>)
   bytes[byte-index] := value;
   byte-index + 1
end method;


define method replace-bytes!
   (bytes :: <byte-vector>, byte-index :: <integer>, value :: <machine-word>)
=> (new-index :: <integer>)
   let int-value = as(<integer>, value);
   for (i from byte-index below byte-index + $machine-word-bytes,
         bits from 0 by 8)
      let byte-value = logand(ash(int-value, -bits), #xFF);
      bytes[i] := as(<byte>, byte-value);
   finally
      i
   end for
end method;


define method replace-bytes!
   (bytes :: <byte-vector>, byte-index :: <integer>, value :: <byte-vector>)
=> (new-index :: <integer>)
   copy-bytes(bytes, byte-index, value, 0, value.size);
   byte-index + value.size
end method;


define method machine-word (bytes :: <byte-vector>, byte-index :: <integer>)
=> (word :: <machine-word>)
   let int-value = 0;
   for (i from byte-index below byte-index + $machine-word-bytes,
        bits from 0 by 8)
      let byte-value = as(<integer>, bytes[i]);
      int-value := logior(int-value, ash(byte-value, bits));
   end for;
   as(<machine-word>, int-value)
end method;


define method tag-byte (object) => (tag :: <byte>)
   local method key-for-class (test-class :: <class>) => (key :: false-or(<integer>))
            find-key($tags, curry(\=, test-class))
         end method;
   let key = any?(key-for-class, object.object-class.all-superclasses);
   as(<byte>, key)
end method;


define method reference-offset
   (object, references :: <table>, block-offsets :: <vector>)
=> (offset :: <machine-word>)
   let block-number = element(references, object, default: #f);
   let offset = block-offsets[block-number];
   as(<machine-word>, offset)
end method;


define method extract-byte-block (bytes :: <byte-vector>, start :: <integer>)
=> (byte-block :: <byte-vector>)
   let tag = bytes[start];
   let block-class = $tags[tag];
   let payload-size =
         if (subtype?(block-class, <sequence>))
            as(<integer>, machine-word(bytes, start + $payload-start))
         else
            // An object may be an instance of several described classes. The
            // biggest of these is the one to use.
            let sizes = choose-by(curry(subtype?, block-class), map(first, $payload-sizes),
                                  map(second, $payload-sizes));
            debug-assert(~sizes.empty?, "Payload size for tag %d not found", tag);
            apply(max, sizes)
         end if;
   let block-size = $tag-size + payload-size;
   let byte-block = make(<byte-vector>, size: block-size);
   copy-bytes(byte-block, 0, bytes, start, block-size);
   byte-block
end method;


define method make-from-byte-block (bytes :: <byte-vector>, offset-objects :: <table>)
=> (object :: <object>)
   let block-class = $tags[bytes[0]];
   make-object(block-class, bytes, offset-objects);
end method;


define method catalog-object
   (object :: <sequence>,
    #key setter, visited, references, dependencies, byte-blocks)
=> (do-slots? :: <boolean>)
   let payload-size = $size-size + (object.size * $reference-size);
   let block-size = $tag-size + payload-size;
   let bytes = make(<byte-vector>, size: block-size);
   replace-bytes!(bytes, 0, object.tag-byte);
   replace-bytes!(bytes, $payload-start, as(<machine-word>, payload-size));

   dependencies[object] := object;
   references[object] := byte-blocks.size;
   add!(byte-blocks, bytes);

   for (elem in object)
      visit-objects(elem, catalog-object, visited: visited, references: references,
            dependencies: dependencies, byte-blocks: byte-blocks)
   end for;
   #t
end method;


define method catalog-object
   (object :: <byte-string>,
    #key setter, visited, references, dependencies, byte-blocks)
=> (do-slots? :: <boolean>)
   let payload-size = $size-size + object.size;
   let block-size = $tag-size + payload-size;
   let bytes = make(<byte-vector>, size: block-size);
   replace-bytes!(bytes, 0, object.tag-byte);
   replace-bytes!(bytes, $payload-start, as(<machine-word>, payload-size));

   dependencies[object] := #();
   references[object] := byte-blocks.size;
   add!(byte-blocks, bytes);
   #t
end method;


define method catalog-object
   (object :: <unicode-string>,
    #key setter, visited, references, dependencies, byte-blocks)
=> (do-slots? :: <boolean>)
   let payload-size = $size-size + (object.size * 2 /* 16-bit characters */);
   let block-size = $tag-size + payload-size;
   let bytes = make(<byte-vector>, size: block-size);
   replace-bytes!(bytes, 0, object.tag-byte);
   replace-bytes!(bytes, $payload-start, as(<machine-word>, payload-size));

   dependencies[object] := #();
   references[object] := byte-blocks.size;
   add!(byte-blocks, bytes);
   #t
end method;


define method catalog-object
   (object :: <object>,
    #key setter, visited, references, dependencies, byte-blocks)
=> (do-slots? :: <boolean>)
   // An object may be an instance of several described classes. The biggest
   // of these is the one to use.
   let sizes = choose-by(curry(instance?, object), map(first, $payload-sizes),
                         map(second, $payload-sizes));
   let payload-size = apply(max, sizes);
   let block-size = $tag-size + payload-size;
   let bytes = make(<byte-vector>, size: block-size);
   replace-bytes!(bytes, 0, object.tag-byte);

   dependencies[object] :=
         begin
            let slots = object.slot-list;
            let deps = make(<vector>, size: slots.size);
            for (s in slots, i from 0)
               deps[i] := object.s
            end for;
            deps
         end;

   references[object] := byte-blocks.size;
   add!(byte-blocks, bytes);
   #t
end method;


define method persist-object
   (object :: <sequence>, #key references, byte-blocks, block-offsets)
=> ()
   let bytes = byte-blocks[references[object]];
   let byte-index = $payload-start + $size-size;
   for (elem in object)
      byte-index := replace-bytes!(bytes, byte-index,
            reference-offset(elem, references, block-offsets));
   end for;
end method;


define method persist-object
   (object :: <boolean>, #key references, byte-blocks, block-offsets)
=> ()
   let bytes = byte-blocks[references[object]];
   replace-bytes!(bytes, $payload-start, as(<byte>, if (object) 1 else 0 end));
end method;


define method persist-object
   (object :: <integer>, #key references, byte-blocks, block-offsets)
=> ()
   let bytes = byte-blocks[references[object]];
   replace-bytes!(bytes, $payload-start, as(<machine-word>, object));
end method;


define method persist-object
   (object :: <character>, #key references, byte-blocks, block-offsets)
=> ()
   let bytes = byte-blocks[references[object]];
   replace-bytes!(bytes, $payload-start, as(<machine-word>, as(<integer>, object)));
end method;


define method persist-object
   (object :: <byte-string>, #key references, byte-blocks, block-offsets)
=> ()
   let bytes = byte-blocks[references[object]];
   copy-bytes(bytes, $payload-start + $size-size, object, 0, object.size);
end method;


define method persist-object
   (object :: <unicode-string>, #key references, byte-blocks, block-offsets)
=> ()
   let bytes = byte-blocks[references[object]];
   let string-bytes = object.size * 2 /* 16-bit character */;
   copy-bytes(bytes, $payload-start + $size-size, object, 0, string-bytes);
end method;


define method persist-object
   (object :: <stream-position>, #key references, byte-blocks, block-offsets)
=> ()
   let bytes = byte-blocks[references[object]];
   replace-bytes!(bytes, $payload-start, as(<machine-word>, as(<integer>, object)));
end method;


define method persist-object
   (object :: <object>, #key references, byte-blocks, block-offsets)
=> ()
   let bytes = byte-blocks[references[object]];
   let byte-index = $payload-start;
   for (s in object.slot-list)
      byte-index := replace-bytes!(bytes, byte-index,
            reference-offset(object.s, references, block-offsets))
   end for;
end method;


define method slot-list (object :: <object>) => (slots :: <list>)
   #()
end method;

define method slot-list (object :: <token>) => (slots :: <list>)
   reduce(add, next-method(), list(parse-start, parse-end))
end method;

define method slot-list (object :: <lexeme>) => (slots :: <list>)
   reduce(add, next-method(), list(text))
end method;

define method slot-list (object :: <case-directive-block-token>) => (slots :: <list>)
   reduce(add, next-method(), list(case-pairs, else-pair, end-directive))
end method;

define method slot-list (object :: <if-directive-block-token>) => (slots :: <list>)
   reduce(add, next-method(), list(if-pair, else-pair, end-directive))
end method;

define method slot-list (object :: <repeat-directive-block-token>) => (slots :: <list>)
   reduce(add, next-method(), list(block-pair, end-directive))
end method;

define method slot-list (object :: <with-directive-block-token>) => (slots :: <list>)
   reduce(add, next-method(), list(block-pair, end-directive))
end method;

define method slot-list (object :: <delimited-directive-token>) => (slots :: <list>)
   reduce(add, next-method(), list(left-delimiter, right-delimiter, directive-specifier))
end method;

define method slot-list (object :: <dirspec-token>) => (slots :: <list>)
   reduce(add, next-method(), list(dirspec-lexeme))
end method;

define method slot-list (object :: <test-dirspec-token>) => (slots :: <list>)
   reduce(add, next-method(), list(test-expression))
end method;

define method slot-list (object :: <expression-token>) => (slots :: <list>)
   reduce(add, next-method(), list(const-value?, value))
end method;

define method slot-list (object :: <repeat-dirspec-token>) => (slots :: <list>)
   reduce(add, next-method(), list(loop-item-name, loop-info-name, collection-expression))
end method;

define method slot-list (object :: <with-dirspec-token>) => (slots :: <list>)
   reduce(add, next-method(), list(assignments))
end method;

define method slot-list (object :: <assignment-token>) => (slots :: <list>)
   reduce(add, next-method(), list(operation?, name, value-token))
end method;

define method slot-list (object :: <simple-dirspec-token>) => (slots :: <list>)
   reduce(add, next-method(), list(expression))
end method;

define method slot-list (object :: <binary-expression-token>) => (slots :: <list>)
   reduce(add, next-method(), list(left-operand, operator, right-operand))
end method;

define method slot-list (object :: <binary-operand-token>) => (slots :: <list>)
   reduce(add, next-method(), list(operator, operand))
end method;

define method slot-list (object :: <operand-token>) => (slots :: <list>)
   reduce(add, next-method(), list(base-value, chained-calls))
end method;

define method slot-list (object :: <var-name-token>) => (slots :: <list>)
   reduce(add, next-method(), list(name))
end method;

define method slot-list (object :: <chained-call-token>) => (slots :: <list>)
   reduce(add, next-method(), list(name))
end method;

define method slot-list (object :: <template-token>) => (slots :: <list>)
   reduce(add, next-method(), list(contents))
end method;

define method slot-list (object :: <directive-epilog-token>) => (slots :: <list>)
   reduce(add, next-method(), list(raw-output?))
end method;


define method make-object
   (class :: subclass(<sequence>), bytes :: <byte-vector>, offset-objects :: <table>)
=> (object :: <vector>)
   let references-start = $payload-start + $size-size;
   let payload-size = as(<integer>, machine-word(bytes, $payload-start));
   let elem-count = ceiling/(payload-size - $size-size, $reference-size);
   let object = make(<simple-object-vector>, size: elem-count);
   for (i from 0 below elem-count, ref-offset from references-start by $reference-size)
      let ref = as(<integer>, machine-word(bytes, ref-offset));
      object[i] := offset-objects[ref];
   end for;
   object
end method;


define method make-object
   (class == <boolean>, bytes :: <byte-vector>, offset-objects :: <table>)
=> (object :: <boolean>)
   bytes[$payload-start] = 1
end method;


define method make-object
   (class == <integer>, bytes :: <byte-vector>, offset-objects :: <table>)
=> (objects :: <integer>)
   as(<integer>, machine-word(bytes, $payload-start))
end method;


define method make-object
   (class :: subclass(<character>), bytes :: <byte-vector>, offset-objects :: <table>)
=> (objects :: <integer>)
   as(<character>, as(<integer>, machine-word(bytes, $payload-start)))
end method;


define method make-object
   (class == <byte-string>, bytes :: <byte-vector>, offset-objects :: <table>)
=> (object :: <byte-string>)
   let chars-start = $payload-start + $size-size;
   let payload-size = as(<integer>, machine-word(bytes, $payload-start));
   let string-size = (payload-size - $size-size);
   let object = make(class, size: string-size);
   copy-bytes(object, 0, bytes, chars-start, string-size);
   object
end method;


define method make-object
   (class == <unicode-string>, bytes :: <byte-vector>, offset-objects :: <table>)
=> (object :: <unicode-string>)
   let chars-start = $payload-start + $size-size;
   let payload-size = as(<integer>, machine-word(bytes, $payload-start));
   let string-size-bytes = (payload-size - $size-size);
   let string-size-chars = ceiling/(string-size-bytes, 2 /* 16-bit characters */);
   let object = make(class, size: string-size-chars);
   copy-bytes(object, 0, bytes, chars-start, string-size-bytes);
   object
end method;


define method make-object
   (class :: <class>, bytes :: <byte-vector>, offset-objects :: <table>)
=> (object :: <object>)
   let setters = class.setter-list;
   let keywords = class.init-keyword-list;
   let object = apply(make, class, keywords);
   for (ref-offset from $payload-start by $reference-size,
         setter in setters)
      let ref = as(<integer>, machine-word(bytes, ref-offset));
      setter(offset-objects[ref], object);
   end for;
   object
end method;


define method setter-list (class :: <class>)
=> (setters :: <list>)
   #()
end method;

define method setter-list (class :: subclass(<token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(parse-start-setter, parse-end-setter))
end method;

define method setter-list (class :: subclass(<lexeme>))
=> (setters :: <list>)
   reduce(add, next-method(), list(text-setter))
end method;

define method setter-list (class :: subclass(<case-directive-block-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(case-pairs-setter, else-pair-setter, end-directive-setter))
end method;

define method setter-list (class :: subclass(<if-directive-block-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(if-pair-setter, else-pair-setter, end-directive-setter))
end method;

define method setter-list (class :: subclass(<repeat-directive-block-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(block-pair-setter, end-directive-setter))
end method;

define method setter-list (class :: subclass(<with-directive-block-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(block-pair-setter, end-directive-setter))
end method;

define method setter-list (class :: subclass(<delimited-directive-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(left-delimiter-setter, right-delimiter-setter, directive-specifier-setter))
end method;

define method setter-list (class :: subclass(<dirspec-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(dirspec-lexeme-setter))
end method;

define method setter-list (class :: subclass(<test-dirspec-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(test-expression-setter))
end method;

define method setter-list (class :: subclass(<expression-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(const-value?-setter, value-setter))
end method;

define method setter-list (class :: subclass(<repeat-dirspec-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(loop-item-name-setter, loop-info-name-setter, collection-expression-setter))
end method;

define method setter-list (class :: subclass(<with-dirspec-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(assignments-setter))
end method;

define method setter-list (class :: subclass(<assignment-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(operation?-setter, name-setter, value-token-setter))
end method;

define method setter-list (class :: subclass(<simple-dirspec-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(expression-setter))
end method;

define method setter-list (class :: subclass(<binary-expression-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(left-operand-setter, operator-setter, right-operand-setter))
end method;

define method setter-list (class :: subclass(<binary-operand-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(operator-setter, operand-setter))
end method;

define method setter-list (class :: subclass(<operand-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(base-value-setter, chained-calls-setter))
end method;

define method setter-list (class :: subclass(<var-name-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(name-setter))
end method;

define method setter-list (class :: subclass(<chained-call-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(name-setter))
end method;

define method setter-list (class :: subclass(<template-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(contents-setter))
end method;

define method setter-list (class :: subclass(<directive-epilog-token>))
=> (setters :: <list>)
   reduce(add, next-method(), list(raw-output?-setter))
end method;


define method init-keyword-list (class :: <class>) => (keywords :: <list>)
   #()
end method;

define method init-keyword-list (class :: subclass(<token>)) => (keywords :: <list>)
   reduce(add, next-method(), list(start:, 0, end:, 0).reverse!)
end method;
