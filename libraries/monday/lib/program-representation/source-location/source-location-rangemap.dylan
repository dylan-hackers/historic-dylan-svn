Module: source-location-rangemap


define constant <boundary-vector> = <stretchy-object-vector>;
  // = limited(<stretchy-vector>, of: <integer>);
            
define class <source-location-rangemap> (<object>)
  slot rangemap-one-to-one? :: <boolean> = #t,
    init-keyword: one-to-one?:;
  
constant slot rangemap-file-boundaries :: <boundary-vector>
  = make(<boundary-vector>, size: 1, fill: $maximum-integer);
constant slot rangemap-file-names :: <stretchy-object-vector>
  = make(<stretchy-object-vector>, size: 1, fill: "");
constant slot rangemap-line-boundaries :: <boundary-vector>
  = make(<boundary-vector>, size: 1, fill: $maximum-integer);
constant slot rangemap-line-numbers :: <stretchy-object-vector>
  = make(<stretchy-object-vector>, size: 1, fill: $maximum-integer);
            
end class;
              
define method range-source-location
    (rangemap :: <source-location-rangemap>,
     start-position :: <integer>,
     end-position :: <integer>)
 => (location :: <source-location>);
  
local
  method locate-boundary
      (boundary-vector :: <boundary-vector>,
       position :: <integer>,
       low-index :: <integer>, high-index :: <integer>)
   => (index :: <integer>);
    if (low-index > high-index)
      -1;
    else
      let mid = ash(low-index + high-index, -1);
      if (position < boundary-vector[mid])
        locate-boundary(boundary-vector, position, low-index, mid);
      elseif(position >= boundary-vector[mid + 1])
        locate-boundary(boundary-vector, position, mid + 1, high-index);
      else
        mid;
      end;
    end if;  
  end method;
            
let file-boundaries = rangemap.rangemap-file-boundaries;
let start-file-boundary
  = locate-boundary(file-boundaries, start-position,
                    0, rangemap.rangemap-file-names.size - 1);
if (start-file-boundary < 0
      | end-position >= file-boundaries[start-file-boundary + 1])
  make(<unknown-source-location>);
else
  let line-boundaries = rangemap.rangemap-line-boundaries;
  let start-line-boundary
    = locate-boundary(line-boundaries, start-position,
                      0, rangemap.rangemap-line-numbers.size - 1);
  let end-line-boundary
    = locate-boundary(line-boundaries, end-position,
                      start-line-boundary,
                      rangemap.rangemap-line-numbers.size - 1);
  if (start-line-boundary < 0 | end-line-boundary < 0)
    make(<unknown-source-location>);
  elseif (rangemap.rangemap-one-to-one?)
    
let start-column
  = start-position - line-boundaries[start-line-boundary] + 1;
let end-column
  = end-position - line-boundaries[end-line-boundary] + 1;
make(<file-source-location>,
     file: rangemap.rangemap-file-names[start-file-boundary],
     start-line: rangemap.rangemap-line-numbers[start-line-boundary],
     start-column: start-column,
     end-line: rangemap.rangemap-line-numbers[end-line-boundary],
     end-column: end-column);
            
  else
    
make(<file-source-location>,
     file: rangemap.rangemap-file-names[start-file-boundary],
     start-line: rangemap.rangemap-line-numbers[start-line-boundary],
     end-line: rangemap.rangemap-line-numbers[end-line-boundary]);
            
  end if;
end if;
            
end method;
              
define method rangemap-add-line
    (rangemap :: <source-location-rangemap>,
     position :: <integer>,
     line :: false-or(<integer>))
 => ();
  
rangemap.rangemap-line-boundaries.size
  := rangemap.rangemap-line-boundaries.size + 1;
rangemap.rangemap-line-numbers.size
  := rangemap.rangemap-line-numbers.size + 1;
let line-boundaries = rangemap.rangemap-line-boundaries;
for(i :: <integer> from line-boundaries.size - 1 above 0 by -1,
    while: line-boundaries[i - 1] > position)
  rangemap.rangemap-line-boundaries[i]
    := rangemap.rangemap-line-boundaries[i - 1];
  rangemap.rangemap-line-numbers[i]
    := rangemap.rangemap-line-numbers[i - 1];
finally
  rangemap.rangemap-line-boundaries[i] := position;
  rangemap.rangemap-line-numbers[i]
    := line | rangemap.rangemap-line-numbers[i - 1] + 1;
end;
            
end method;
              
define method rangemap-add-line-file
    (rangemap :: <source-location-rangemap>,
     position :: <integer>,
     line :: <integer>,
     file :: <file-locator>)
 => ();
  rangemap-add-line(rangemap, position, line);
  
rangemap.rangemap-file-boundaries.size
  := rangemap.rangemap-file-boundaries.size + 1;
rangemap.rangemap-file-names.size
  := rangemap.rangemap-file-names.size + 1;
let file-boundaries = rangemap.rangemap-file-boundaries;
for(i :: <integer> from file-boundaries.size - 1 above 0 by -1,
    while: file-boundaries[i - 1] > position)
  rangemap.rangemap-file-boundaries[i]
    := rangemap.rangemap-file-boundaries[i - 1];
  rangemap.rangemap-file-names[i]
    := rangemap.rangemap-file-names[i - 1];
finally
  rangemap.rangemap-file-boundaries[i] := position;
  rangemap.rangemap-file-names[i] := file;
end;
            
end method;
              
