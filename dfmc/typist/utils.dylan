module: dfmc-typist

define method dynamic? (type ::  <&type>) => (res :: <boolean>)
  #f //^type-equivalent?(type, dylan-value(#"<object>"))
end;

define method dynamic? (type :: <typist-type>) => (res == #f)
  #f
end;

define method dynamic? (tv :: <type-variable>) => (res :: <boolean>)
  #f //tv.type-variable-contents.dynamic?
end;

define method dynamic? (tv :: <&top-type>) => (res == #f)
  #f
end;

define method dynamic? (d :: <dynamic>) => (res == #t)
  #t
end;

define method arrow? (type :: <&type>) => (res :: <boolean>)
  #f
end;

define method arrow? (type :: <typist-type>) => (res == #f)
  #f
end;

define method arrow? (type :: <&limited-function-type>) => (res :: <boolean>)
  #t
end;

define method arrow? (type :: <arrow>) => (res == #t)
  #t
end;

define method arrow? (tv :: <type-variable>) => (res :: <boolean>)
  tv.type-variable-contents.arrow?
end;

define method rest-value? (n :: <node>) => (res :: <boolean>)
  n.find.node-value.rest-value?;
end;

define method rest-value? (tv :: <type-variable>) => (res :: <boolean>)
  tv.type-variable-contents.rest-value?
end;

define method rest-value? (type :: <&type>) => (res == #f)
  #f
end;

define method rest-value? (type :: <typist-type>) => (res == #f)
  #f
end;

define method rest-value? (type  :: <tuple-with-rest>) => (res == #t)
  #t
end;


define constant <type-cache>     
  = limited(<table>, of: #f);

define compiler-sideways method initialize-typist-library-caches 
    (ld :: <compilation-context>) => ()
  // When you make a <library-description> or an interactive layer, install
  // typist caches.   Done this way because of module lossage, i.e. typist
  // classes aren't visible
  // in dfmc-namespace, so couldn't do it with slot initializers. Sigh.
  library-type-cache(ld) := 
    make(<type-cache> /* , size: $type-cache-size-init$ */);
  library-type-estimate-disjoint?-cache(ld) := 
    make(<pair-table>);
  library-type-estimate-cons-cache(ld) := 
    make(<table> /* , size: $cons-cache-size-init$ */);
  library-type-estimate-dispatch-cache(ld) := 
    make(<table> /* , size: $cons-cache-size-init$ */);
end;

define class <pair-table> (<table>)
end;

define function pair-test (key1 :: <pair>, key2 :: <pair>) => (match :: <boolean>)
  ((key1.head = key2.head) & (key1.tail = key2.tail)) |
    ((key1.tail = key2.head) & (key1.head = key2.tail))
end;

define function pair-hash (key :: <pair>, state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>)
  let hash1 = object-hash(key.head, state);
  let hash2 = object-hash(key.tail, state);
  values(merge-hash-ids(hash1, hash2), state);
end;

define method table-protocol (t :: <pair-table>)
 => (test :: <function>, hash :: <function>)
  values(pair-test, pair-hash);
end;

/*
begin
  let p = make(<pair-table>);
  p[pair(23, 42)] := #"foo";
  format-out("p has size %d, (23, 42): %= (42, 23): %=\n",
             p.size, element(p, pair(23, 42), default: #f), element(p, pair(42, 23), default: #f));
  p[pair(#"foo", #"bar")] := #"foobar";
  p[pair(42, 23)] := #"barf";
  format-out("p has size %d, (23, 42): %= (42, 23): %= (#\"foo\", #\"bar\"): %= (#\"bar\", #\"foo\"): %=\n",
             p.size,
             element(p, pair(23, 42), default: #f),
             element(p, pair(42, 23), default: #f),
             element(p, pair(#"foo", #"bar"), default: #f),
             element(p, pair(#"bar", #"foo"), default: #f));
end;
*/

