module:    inertia-gl-utils
synopsis:  OpenGL utilities
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-gl-utils.dylan
//

define method as (type == <GLdouble*>, collection :: <collection>)
              => (result :: <GLdouble*>)
  let double* = make (<GLdouble*>, element-count: collection.size);
  for (i from 0, value in collection)
    double*[i] := value;
  end;
  double*;
end;

define method element (double* :: <GLdouble*>, index :: <integer>, #key default = 0.0)
                   => (result :: <GLdouble>)
//let x = double*.size;
//  if (index < pointer.size)
    default := pointer-value (double*, index: index);
//  else
//    format-out ("*** element (<GLdouble*>): index out of range");
//    default;
//  end;
  default;
end;

define method element-setter (value :: <GLdouble>, pointer :: <GLdouble*>, index :: <integer>)
                          => (result :: <GLdouble>)
  pointer-value (pointer, index: index) := value;
end;

// ---------------------------------------------------------------------------------------------- //

define method glutxBitmapString (font :: <machine-pointer>, string :: <string>)
  for (char in string)
    glutBitmapCharacter (font, as(<integer>, char));
  end;
end;

define method glutxBitmapLength (font :: <machine-pointer>, string :: <string>)
  reduce (method (width, char)
            width + glutBitmapWidth (font, as(<integer>, char));
          end, 0, string);
end;

