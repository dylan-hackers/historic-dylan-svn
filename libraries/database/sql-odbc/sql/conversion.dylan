Module: result-set-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: D-databases-sql!conversion.dylan(trunk.5) $


define open generic default-conversion(value :: <statically-typed-pointer>) 
 => (converted-value :: <object>);
 
define method default-conversion(value :: <statically-typed-pointer>) 
 => (converted-value :: <object>);
  pointer-value(value);
end method;

define method default-conversion(value :: <C-string>) 
 => (converted-value :: <byte-string>)
  as(<byte-string>, value)
end method;


define constant $default-coercion = #"default-coercion";
define constant $no-coercion = #"no-coercion";

define constant <coercion-policy> 
  = type-union(singleton($default-coercion),singleton($no-coercion),
               <sequence>, <object>);


define generic convert-value(coercion-policy :: <coercion-policy>, 
                             value :: <object>, key :: <integer>)
 => (converted-value :: <object>);


define method convert-value(coercion-policy == $default-coercion, 
                            value :: <object>, key :: <integer>)
 => (converted-value :: <object>)
  default-conversion(value)
end method;


define method convert-value(coercion-policy :: <sequence>, 
                            value :: <object>, key :: <integer>)
 => (converted-value :: <object>)
  let not-found = pair(#f, #f);
  let conversion-function = element(coercion-policy, key, default: not-found);
  if (conversion-function ~== not-found)
    if (instance?(conversion-function, <function>) = #f)
      error("Coercion-policy sequence contains "
            "an item that is not a function.");
    end if;
    conversion-function(value);
  else
    //++ signal a warning?
    convert-value(#"default-coercion", value, key)
  end if;
end method;


define generic acquire-null-value(indicator :: <object>,
                                  index :: <integer>)
 => (null-value :: <object>);


define method acquire-null-value(indicator :: <object>,
                                 index :: <integer>)
 => (null-value :: <object>);
  indicator;
end method;


define method acquire-null-value(indicator == $no-indicator,
                                 index :: <integer>)
 => (null-value :: <object>);
    error("no output indicator provided.\n");  //+++ throw proper condition
end method; 


define method acquire-null-value(indicator :: <sequence>,
                                 index :: <integer>)
 => (null-value :: <object>);
  let not-found = pair(#f, #f);
  let null-value = element(indicator, index, default: not-found);
  if (null-value == not-found)
    error("no output indicator provided.\n");  //+++ throw proper condition
  else
    null-value
  end if;
end method;
