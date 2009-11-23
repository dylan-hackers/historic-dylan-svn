language: infix-dylan
module: Dylan-user

//==============================================================================
//
// Copyright (c) 1996 by Kai W. Zimmermann, Hamburg, Germany
// Distributed under the terms of the GNU Lesser General Public License (LGPL)
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU Lesser General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
// 
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
// details.
// 
// The terms of the GNU Lesser General Public License may be found at 
// <http://www.gnu.org/licenses/lgpl.html>.
// 
// If you make changes to the code, port it to another platform, or use it in a
// major project, please let me know.
// 
//==============================================================================

//==============================================================================
//
// Version 1.0
//
// Author
// 
// KWZ       =    Kai W. Zimmermann, Hamburg, Germany
//                kwz@kai-zimmermann.de
// DJV       =    Dustin Voss, Seattle, United States
//                d_j_v@mac.com, |Agent on irc://irc.freenode.net/#dylan
//
// History
//
// 15.03.1996  KWZ  Created in Apple Dylan TR
// 08.03.2009  DJV  Modified for Gwydion Dylan and Open Dylan.
//                  Removed <basic-skip-list> and clear-cache; only
//                  implementors need those things.
//                  Added iteration independent of key order.
//
//==============================================================================


define library skip-list
  use common-dylan,
     import: {dylan, common-extensions, simple-random, transcendentals};
  use io,
     import: {streams, format, print, pprint, format-out};
  export skip-list;
end library;


define module skip-list
  // Copyright (c) 1996  Kai W. Zimmermann
  
  use dylan,
     // Actually redundant.  Only for documentation purpose.
     export: {element, element-setter, size, key-test,
              forward-iteration-protocol, backward-iteration-protocol,
              initialize, as, remove-key!};
  use common-extensions;
  use simple-random,
     import: {random};
  use transcendentals,
     import: {logn};
  use streams,
     import: {<stream>, write, write-element};
  use format,
     import: {format};
  use print,
     import: {print-object},
     export: all;
  use pprint,
     import: {printing-logical-block, pprint-newline, pprint-indent};
  export 
     // Classes
     <skip-list>,
     // Methods
     probability, level, max-level,
     key-order, element-sequence, element-sequence-setter,
     forward-by-key-iteration-protocol;
end module skip-list;
