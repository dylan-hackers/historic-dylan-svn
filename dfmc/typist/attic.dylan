module: dfmc-typist

define abstract primary class <type-estimate> (<object>)
end;

define abstract class <type-estimate-bottom> (<type-estimate>)
end;

define constant <type-variable-vector> = <simple-object-vector>;

define abstract class <type-estimate-class> (<type-estimate>)
  constant slot type-estimate-class /* :: <&class> */,
    required-init-keyword: class:;
end;

define abstract class <type-estimate-raw> (<type-estimate>)
  constant slot type-estimate-raw 
      :: type-union(<&raw-type>, <&raw-aggregate-type>), 
    required-init-keyword: raw:;
end;

define abstract class <type-estimate-union> (<type-estimate>)
  constant slot type-estimate-unionees :: <collection>,
    required-init-keyword: unionees:;
end;

define abstract class <type-estimate-values> (<type-estimate>)
  constant slot type-estimate-fixed-values :: <type-variable-vector> = #[], 
    init-keyword: fixed:;
  constant slot type-estimate-rest-values :: false-or(<type-variable>)
    = make(<type-variable>, 
           contents: make(<type-estimate-class>, 
                          class: dylan-value(#"<object>"))),
    init-keyword: rest:;
end;

define abstract class <type-estimate-limited> (<type-estimate-class>)
end;

define abstract class <type-estimate-limited-collection> (<type-estimate-limited>)
  constant slot type-estimate-concrete-class /* :: <&class> */,
    required-init-keyword: concrete-class:;
  constant slot type-estimate-dimensions :: false-or(limited(<sequence>, 
                                                             of: <integer>
							       // limited(<integer>, min: 0)
							       )),
    init-value: #f, init-keyword: dimensions:;
  constant slot type-estimate-size :: false-or(limited(<integer>, min: 0)) = #f,
    init-keyword: size:;
  constant slot type-estimate-of :: false-or(<type-variable>) = #f,
    init-keyword: of:;
end;

define abstract class <type-estimate-limited-instance> (<type-estimate-limited>)
  constant slot type-estimate-singleton :: <model-value>,
    required-init-keyword: singleton:;
end;

define abstract class <type-estimate-limited-function> (<type-estimate-limited>)
end;

define generic type-estimate-debug-name (x) => (dn :: <string>);

define method type-estimate-debug-name(o :: <object>) => (dn :: <string>)
  // Last-ditch attempt: just print it to a string.
  let str = make(<byte-string-stream>, direction: #"output");
  format(str, "%s", o);
  stream-contents(str)
end;

define method type-estimate-debug-name(o :: <&object>) => (dn :: <string>)
  // Attempt to extract the debug-name.
  let debug-str = ^debug-name(o);
  if (debug-str)
    as(<string>, debug-str)
  else
    // format-out("\n *** type-estimate-debug-name(%s) punted.", o);
    next-method()
  end
end;

define function type-estimate-values-rest-subtype?
    (values-te :: <type-estimate>, index :: <integer>, te :: <type-estimate>)
 => (subtype? :: <boolean>)
  #f;
end;

define function type-estimate-values-element-subtype?
    (values-te :: <type-estimate>, index :: <integer>, te :: <type-estimate>)
 => (subtype? :: <boolean>)
  #f;
end;

define method type-estimate-subtype? (te1 :: <type-estimate>, te2 :: <type-estimate>)
 => (res :: <boolean>)
  #f;
end;

define method type-estimate-retract(ref :: <computation>)
  => (did-anything? :: <boolean>);
  #f;
end;

define constant make-type-estimate = make;

define function type-estimate-values-ref 
  (vte :: <type-estimate-values>, i ::<integer>)
  => (tei :: <type-estimate>, source :: one-of(#"fixed", #"rest", #"default"))
  // Return the ith type in a values type-estimate, and where it 
  // came from: #"fixed", #"rest", or #"default".
  if (i < 0)
    error("Can't take negative (%dth) value from %s!", i, vte)
  elseif (i < size(type-estimate-fixed-values(vte)))
    values(type-estimate-fixed-values(vte)[i], #"fixed")   // In range
  elseif (type-estimate-rest-values(vte))
    values(type-estimate-rest-values(vte), #"rest")        // Out of range, #rest
  else
    values(make(<type-estimate-limited-instance>, singleton: #f), #"default")
  end
end;

define constant <type-cache>     
  = limited(<table>, of: false-or(<type-variable>));

define compiler-sideways method initialize-typist-library-caches 
    (ld :: <compilation-context>) => ()
  // When you make a <library-description> or an interactive layer, install
  // typist caches.   Done this way because of module lossage, i.e. typist
  // classes aren't visible
  // in dfmc-namespace, so couldn't do it with slot initializers. Sigh.
  library-type-cache(ld) := 
    make(<type-cache> /* , size: $type-cache-size-init$ */);
  library-type-estimate-disjoint?-cache(ld) := 
    //make(<type-estimate-pair-match-table> /* , size: $disjoint-cache-size-init$ */);
    make(<table>);
  library-type-estimate-cons-cache(ld) := 
    make(<table> /* , size: $cons-cache-size-init$ */);
  library-type-estimate-dispatch-cache(ld) := 
    make(<table> /* , size: $cons-cache-size-init$ */);
end;

